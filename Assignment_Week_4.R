# Set the working directory.
setwd("/Users/sophiayu1/Desktop/LSE Career Accelerator/LSE_DA301_assignment_files")

# Import necessary libraries. 
library (tidyverse)
library (moments)
library (BSDA)
library (dplyr)
library (reshape2)


# Import the turtle_sales.csv. 
ts <- read.csv('turtle_sales.csv', header = T)

# Explore the data set by viewing it and seeing it as tibble.
as_tibble(ts)
head(ts, 10)
View(ts)

# Remove redundant columns so that the data only shows the necessary sales data.
ts2 <- select(ts, -Ranking, -Year, -Genre, -Publisher)
# View head and full data set. 
head(ts2,5)
View (ts2)
# Create a summary od descriptive statistics for the sales data set. 
summary (ts2)

# Exploratory data visualisation to gain insight into trends in Turtle Sales.
qplot(Product, Global_Sales, data = ts2)
qplot(Product, NA_Sales, data = ts2)
qplot(Product, EU_Sales, data = ts2)
# There seems to be a relationship of exponential decay between product number and Global_Sales.
# Product no 107 is by far bringing in the most sales and is a strong outlier is all regions. 

# Create a bar plot to understand which platform is most common. 
qplot(Platform, data = ts2, geom = 'bar' )

#Use boxplot to determine which platforms are most popular regionally. 
qplot(Platform, Global_Sales, data = ts2, geom = 'boxplot')
# Which platform is most popular in North America. 
qplot(Platform, NA_Sales, data = ts2, geom = 'boxplot')
# Which platform is most popular in Europe. 
qplot(Platform, EU_Sales, data = ts2, geom = 'boxplot')

write.csv(ts2, 'turtles_just_sales.csv')

###############################################################################
# The global sales of turtle games correlated in an exponential decay relationship with the product number. 
# This maybe due to products being numbered in order of production and earlier games have more time on the market. 
# Product no 107 is by far bringing in the most sales and is a strong outlier is all regions. 

###############################################################################
# Determine sales of each product. 
product_sales <- ts2 %>% group_by(Product) %>% summarise (NA_Sales_Product = sum(NA_Sales), 
                                                         EU_Sales_Product = sum(EU_Sales),
                                                         Global_Sales_Product = sum (Global_Sales)) 
# Reshape the data so that the columns and rows are inversed
sales_prod_melt <- melt(product_sales, id = c('Product'))
head(sales_prod_melt)

# Create a scatterplot with trendlines for ease of intepretation. 
ggplot(sales_prod_melt, aes(x = Product, y = value, col = variable)) +
  geom_point() +
  stat_smooth(se = F) +
  labs(x = "Product", y = "Value", title = "Product Sales Performance", subtitle = "Turtle Games") +
  theme_bw()
