#Load in libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(scales)
library(arules)
library(arulesViz)

#Reading in data
aisles <- read.csv("data/aisles.csv")
departments <- read.csv("data/departments.csv")
products_prior <- read.csv("data/order_products__prior.csv")
products_train <- read.csv("data/order_products__train.csv")
orders <- read.csv("data/orders.csv")
products <- read.csv("data/products.csv")

#Inspect the data
#The eval_set field in the orders dataset tells us if the order is from either train, prior, or test
glimpse(aisle) #Category information for products
glimpse(department) #Category information for products
glimpse(products_prior) #products that were on an order
glimpse(products_train) #products that were on an order
glimpse(orders) #order level information
glimpse(products) #product level data

#Convert character variables to factors
aisles <- aisles %>% 
            mutate(aisle = as.factor(aisle))

departments <-  departments %>% 
                mutate(department = as.factor(department))

orders <-  orders %>% 
            mutate(eval_set = as.factor(eval_set)
                   ,order_hour_of_day = as.numeric(order_hour_of_day)
                   ,order_dow = as.factor(order_dow)
                   ,order_hour_of_day = as.factor(order_hour_of_day))

products <- products %>% 
              mutate(product_name = as.factor(product_name))
            

#Top 10 most ordered items - Prior/Train
top10_products_prior <- products_prior %>% 
                        group_by(product_id) %>%
                        summarise(number_of_orders = n()) %>% 
                        top_n(10) %>%
                        left_join(products, by = "product_id") %>%
                        select(product_name, number_of_orders) %>%
                        arrange(desc(number_of_orders))

top10_products_train <- products_train %>% 
                        group_by(product_id) %>%
                        summarise(number_of_orders = n()) %>% 
                        top_n(10) %>%
                        left_join(products, by = "product_id") %>%
                        select(product_name, number_of_orders) %>%
                        arrange(desc(number_of_orders))



#Top 10 most ordered items - Plotting
prod_bar_plot <- function(ds) {
  ds %>% 
    ggplot(aes(reorder(product_name, number_of_orders), number_of_orders, fill = product_name)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(x = "Product Name", y = "Count") +
    theme_minimal()  +
    theme(legend.position = "none") 

} 

prod_prior_plt <- prod_bar_plot(top10_products_prior)
prod_train_plt <- prod_bar_plot(top10_products_train)


plot_grid(prod_prior_plt + labs(title = "Top 10 Items in the Prior Dataset")
          ,prod_train_plt + labs(title =  "Top 10 Items in the Train Dataset"))


#Average Instacart basket size - Train/Prior
basket_size_prior <- products_prior %>% 
                      group_by(order_id) %>% 
                      summarise(basket_size = last(add_to_cart_order)) %>% 
                      ggplot(aes(basket_size)) +
                      geom_histogram(stat = "count", fill = "light blue", binwidth = 5) +
                      xlim(0,60) +
                      #scale_x_continuous(breaks = seq(from = 0, to = 60, by = 5)) +
                      theme_minimal()
                      
basket_size_train <- products_train %>% 
                      group_by(order_id) %>% 
                      summarise(basket_size = last(add_to_cart_order)) %>% 
                      ggplot(aes(basket_size)) +
                      geom_histogram(stat = "count", fill = "light green") +
                      xlim(0,60) +
                      #scale_x_continuous(breaks = seq(from = 0, to = 60, by = 5)) +
                      theme_minimal()
                      
plot_grid(basket_size_prior + labs(title = "Basket Size Distribution - Prior")
          ,basket_size_train + labs(title = "Basket Size Distribution - Train")
          ,ncol = 1)

#Busiest day of the week (dow)
#Day 0 and Day 1 are the busiest days, it might be safe to assume this is the weekend because people tend to get groceries over the weekend
#There is currently no way to determine if Day 0 or 1 is the weekend
orders %>% 
  group_by(order_dow) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  ggplot(aes(reorder(order_dow, -n), n, fill = order_dow)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels =  comma) +
  labs(x = "Day of the Week", y = "Count") +
    theme_minimal()

#Busiest time of day
#Between 10:00 AM and 3:00 PM are the busiest times
orders %>% 
  group_by(order_hour_of_day) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  ggplot(aes(order_hour_of_day, n, fill = order_hour_of_day)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels =  comma) +
  labs(x = "Hour of the Day", y = "Count") +
  theme_minimal()


#Put data into a basket format
#All products in an order will be in one record
train_basket <- products_train %>% 
                    left_join(products, by = "product_id") %>%  
                    select(order_id, product_name) 

#Write basket data to a transaction file then read it in
write.csv(train_basket, file = "data/train_basket.csv")
baskets <- read.transactions("data/train_basket.csv", format = "single", sep = ",", cols = c(2,3))
summary(baskets)

#This does not work with large datasets
#baskets <- as(split(train_basket[,"product_name"], train_basket[,"order_id"]), "transactions")
#summary(baskets)

#Inspecting the first 5 baskets
inspect(baskets[1:5])

#Establish association rules with the Apriori Algorithm
#Through experimentation a confidence of more than 0.40 resulted in 1 or less rules
basket_rules <- apriori(baskets, parameter = list(support = 0.005, confidence = 0.30))
summary(basket_rules)
inspect(basket_rules[1:5])

#Top support and confidence values
#It seems like bananas (regular or organic) are bought with many transactions
top_support <- sort(basket_rules, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top_support, 10))

top_confidence <- sort(basket_rules, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top_confidence, 10))

#arules visualizations
#Organic Hass Avocados and Bag of Organic Bananas have the strongest association
plot(basket_rules)
inspectDT(basket_rules)


