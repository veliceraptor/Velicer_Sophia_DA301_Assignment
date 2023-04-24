# Import necessary libraries. 
library (tidyverse)
library (moments)
library (BSDA)
library (dplyr)
library (reshape2)
tidyr::smiths


# Set working directory. 
# Import the data set that was created in Week 4. 
ts <- read.csv('turtles_just_sales.csv', header = T)

# Sense check the data set.
View(ts)
head(ts)
dim(ts)
str(ts)
colnames(ts)

# Drop the redundant X column. 
ts <- subset(ts, select = -X)

# Check. 
colnames(ts)

# View measures of central tendency for the sales columns.
summary(ts[,c('NA_Sales', 'EU_Sales', 'Global_Sales')])

# Determine sales of each product. 
product_sales <- ts %>% group_by(Product) %>% summarise (NA_Sales_Product = sum(NA_Sales), 
                                                         EU_Sales_Product = sum(EU_Sales),
                                                         Global_Sales_Product = sum (Global_Sales)) 

# Sense check the new data frame
dim(product_sales)
head(product_sales)
unique(product_sales)

#######################################

# Create exploratory visualisation. 
# Create scatterplot for each sales variable. 
qplot(x = Product, y = Global_Sales_Product, data = product_sales, geom = 'point')
qplot(x = Product, y = NA_Sales_Product, data = product_sales, geom = 'point')
qplot(x = Product, y = EU_Sales_Product, data = product_sales, geom = 'point')

# Create exploratory histograms. 
qplot(x = Global_Sales_Product, data = product_sales, geom = 'histogram')
qplot(x = NA_Sales_Product, data = product_sales, geom = 'histogram')
qplot(x = EU_Sales_Product, data = product_sales, geom = 'histogram')

# Create exploratory boxplots. 
qplot(x = Product, y = Global_Sales_Product, data = product_sales, geom = 'boxplot')
qplot(x = Product, y = NA_Sales_Product, data = product_sales, geom = 'boxplot')
qplot(x = Product, y = EU_Sales_Product, data = product_sales, geom = 'boxplot')

#######################################
# Determine normality of sales distribution. 

# North American sales normality. 
# Creat QQ plot and QQ line. 
qqnorm(product_sales$NA_Sales_Product, col = 'blue')
qqline(product_sales$NA_Sales_Product, col = 'dark blue')

# Double check normality with Shapiro Wilk test. 
shapiro.test(product_sales$NA_Sales_Product)
# P-value < 2.2e-16 < 0.05 so we reject the null hypothesis. 
# The distribution is not normal.

# Determine skewness and kurtosis. 
skewness (product_sales$NA_Sales_Product)
# Positive skew. 
kurtosis(product_sales$NA_Sales_Product)
# Kurtosis is high, indicating heavy tails. 

# Determine European sales normality. 
# Creat QQ plot and QQ line. 
qqnorm(product_sales$EU_Sales_Product, col = 'green')
qqline(product_sales$EU_Sales_Product, col = 'dark green')

# Double check normality with Shapiro Wilk test. 
shapiro.test(product_sales$EU_Sales_Product)
# P-value < 2.987e-16 < 0.05 so we reject the null hypothesis. 
# The distribution is not normal.

# Determine skewness and kurtosis. 
skewness (product_sales$EU_Sales_Product)
# Positive skew. 
kurtosis(product_sales$EU_Sales_Product)
# Kurtosis is high, indicating heavy tails. 

# Determine normality of Global Sales.
# Creat QQ plot and QQ line. 
qqnorm(product_sales$Global_Sales_Product, col = 'violet')
qqline(product_sales$Global_Sales_Product, col = 'purple')

# Double check normality with Shapiro Wilk test. 
shapiro.test(product_sales$Global_Sales_Product)
# P-value < 2.2e-16 < 0.05 so we reject the null hypothesis. 
# The distribution is not normal.

# Determine skewness and kurtosis. 
skewness (product_sales$Global_Sales_Product)
# Positive skew. 
kurtosis(product_sales$Global_Sales_Product)
# Kurtosis is high, indicating heavy tails. 

# Determine correlation between the sales data.  
cor(product_sales$NA_Sales_Product, product_sales$EU_Sales_Product)
cor(product_sales$NA_Sales_Product, product_sales$Global_Sales_Product)
cor(product_sales$Global_Sales_Product, product_sales$EU_Sales_Product)

#######################################

# Plots that display the distribution of the sales data. 
# Global sales distribtuion
ggplot(data=product_sales, mapping=aes(x = Product, y = Global_Sales_Product)) +
  geom_boxplot(fill = 'light green', outlier.color = 'dark green') +
  labs(x = "Product ID", y = "Global Sales (GBP)", title = "The Distribution of Global Sales", 
       subtitle = "Sales review of turtle games") + 
  theme_classic()

# North American sales distribution.
ggplot(data=product_sales, mapping=aes(x = Product, y = NA_Sales_Product)) +
  geom_boxplot(fill = 'pink', outlier.color = 'red') +
  labs(x = "Product ID", y = "North American Sales (GBP)", title = "The Distribution of North American Sales", 
       subtitle = "Sales review of turtle games") + 
  theme_classic()

# European sales distribution. 
ggplot(data=product_sales, mapping=aes(x = Product, y = EU_Sales_Product)) +
  geom_boxplot(fill = 'light blue', outlier.color = 'blue') +
  labs(x = "Product ID", y = "European Sales (GBP)", title = "The Distribution of European Sales", 
       subtitle = "Sales review of turtle games") + 
  theme_classic()


# Comparing sales data against each other. 
# Make a copy of the original sales dataframe to work with. 
sales <- data.frame(ts)
head(sales)

# Create new column that is the remainder sales outside of the EU and NA. 
sales$Other_Sales <- sales$Global_Sales - (sales$NA_Sales + sales$EU_Sales)
head(sales)

# Drop the Global_Sales column. 
sales <- sales[,-5]
head(sales)

# Create a dataframe that is based on gaming platform and view. 
sales_platform <- sales[,-1]
head(sales_platform)

# Reshape the platform dataframe with the melt function. 
sales_plat_melt <- melt(sales_platform, id = c('Platform'), variable = "Region", value = "Sales")

head(sales_plat_melt)

# Reshape the products dataframe. 
sales_product <- sales[,-2]
head(sales_product)


#######################################
# Create visualisations for ease of interpretration. 
# Compare the sales per platform in a barplot. 
ggplot(sales_plat_melt, aes(x = reorder(Platform, value), y = value, fill = Region, reorder)) +
  geom_bar(position = 'stack', stat = 'identity') +
  labs (x = "Gaming Platform", y = "Total Sales (GBP)", title = "Turtle Games Sales per Platform") +
  theme_minimal()

ggplot(sales_plat_melt, aes(x = reorder(Platform, value), y = value, fill = Region, reorder)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  labs (x = "Gaming Platform", y = "Total Sales (GBP)", title = "Turtle Games Sales per Platform")


# Add trendlines on scatterplots.
ggplot(ts, aes(x = Product, y = Global_Sales)) +
  geom_point() +
  stat_smooth(se = F)

###############################################################################

# Fit a simlpe Linear Regressiong model between the sales variables to determine the relationship, 
model1 <- lm(NA_Sales ~ EU_Sales, data = sales)

# View the summary.
summary(model1)

# View residuals on a plot.
plot(model1$residuals)


# Plot the relationship with base R graphics.
plot(sales$EU_Sales, sales$NA_Sales)
coefficients(model1)

# Add line-of-best-fit.
abline(coefficients(model1), col = "red")

# Create a multiple linear regression. 
model2 <- lm(NA_Sales ~ EU_Sales + Other_Sales, data = sales)

# View the summary. 
summary (model2)

# View residuals on a plot.
plot(model2$residuals)

# Test if the model is better with global sales as a variable.
# Create a multiple linear regression. 
model3 <- lm(NA_Sales ~ EU_Sales + Global_Sales, data = ts)

# View the summary. 
summary (model3)

# View residuals on a plot.
plot(model3$residuals)

# Create a model with Global sales as the dependent variable. 
# Create a multiple linear regression. 
model4 <- lm(Global_Sales ~ EU_Sales + NA_Sales, data = ts)

# View the summary. 
summary (model4)
# Model 4 is the best model with an adjusted R squared value of 0.9685. 

# View residuals on a plot.
plot(model4$residuals)

#######################################

# Determine the correlations between the sales variables.
cor(sales$NA_Sales, sales$EU_Sales)
cor(sales$NA_Sales, sales$Other_Sales)
cor(sales$EU_Sales, sales$Other_Sales)
cor(ts$NA_Sales, ts$Global_Sales)
cor(ts$EU_Sales, ts$Global_Sales)

# Plot the correlation. 
# Install neccessary packages.
install.packages('tmvnsim')
install.packages('psych')

library(psych)

# Create a subset of the data set with just the sales variables.
sales_var <- subset(ts, select=c(NA_Sales, EU_Sales, Global_Sales))

#View the outcome
head(sales_var)

#Plot the a correlation matrix
corPlot(sales_var)

#######################################

# Makes a forcast of global sales with Model 4. 
# Make a new dataframe for the forecast values. 
NA_Sales <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales <- c(23.80, 1.56, 0.65, 0.97, 0.52)
sales_forecast <- data.frame(NA_Sales, EU_Sales)

# Use the predict function to forecast the global sales. 
sales_forecast$Global_Sales <- predict(model4, newdata = sales_forecast)

# View the prediction dataframe. 
sales_forecast
