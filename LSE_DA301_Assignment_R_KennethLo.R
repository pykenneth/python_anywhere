## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library(tidyverse)

# Import the data set.
sales <- read.csv('turtle_sales.csv', header=TRUE)

# Print the data frame.
head(sales)
str(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns.
# 1. Sense check if there is some specific features about the product
sales %>% 
  filter(duplicated(Product)) %>% 
  select(c(Product,Ranking, Platform, Publisher, Genre)) %>% 
  head(15)
# 2. Sense check one duplicated Product
sales %>% 
  filter(Product ==515)
# Features:
# 1. A Product can have different Ranking, Plaform and Year
# 2. A Product related only to 1 Genre and Publisher

# Thus, we should study the product with Platform and Genre
sales1 <- sales %>% 
  select(-c(Ranking, Year, Publisher))

# View the data frame.
View(sales1)

# View the descriptive statistics.
summary(sales1)
sum(is.na(sales1))

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots for sales pattern between regions
ggplot(sales1)+
  geom_point(aes(x=Global_Sales, y=EU_Sales), color='red')+
  geom_point(aes(x=Global_Sales, y=NA_Sales), color='blue')+
  ylab('Sales')+
  xlab('Global Sales')+
  labs(title = 'Sales over Region',
       subtitle = "European Union = Blue,Noth America = Red")+
  theme_bw()
 

## 2b) Histograms
# Create histograms.
ggplot(sales1,aes(x=EU_Sales))+
  geom_histogram(aes(x=EU_Sales,y = ..count..),
                 binwidth = 1.5, fill='navy',position = 'dodge')+
  geom_histogram(aes(x=NA_Sales,y = ..count..),
                 binwidth = 1.5, fill='grey',alpha= 0.4,position = 'dodge')+
  geom_histogram(aes(x=Global_Sales,y = ..count..),
                 binwidth = 1.5, color='red',alpha= 0.0,position = 'dodge')+
  xlab('Sales(millions)')+
  labs(title = 'Sales Distribution',
       subtitle = "European Union = Blue,Noth America = Grey, Global=Red")+
  theme_minimal()

ggplot(sales1,aes(x=NA_Sales))+
  geom_histogram(aes(y = ..density..),bins = 35, fill='navy')+
  geom_density(color= 'red')

  
## 2c) Boxplots
ggplot(sales)+
  geom_boxplot(aes(NA_Sales), fill='red')+
  labs(title="Sales Figure in North America")
ggplot(sales)+
  geom_boxplot(aes(EU_Sales), fill='blue')+
  labs(title="Sales Figure in EU")  
ggplot(sales)+
  geom_boxplot(aes(Global_Sales))+
  labs(title="Global Sales Figure")  

# Export the dataframe to csv for record
write.csv(sales1, 'sales1.csv', row.names = F)

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
# The sales of games for both EU & NA region mostly occurred with product sales 
# below 3 millions pounds with similar pattern
# For the same product range, the global sales were more evenly distributed
# and skewed toward higher products' sales record.
# That indicated that many products sold much more in other regions which 
# obtained higher marginal return per product ID.



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data
sales1 <- read.csv('sales1.csv', header=TRUE)

# View data frame created in Week 4.
View(sales1)

# Check output: Determine the min, max, and mean values.
# NA Sales
max(sales$NA_Sales) #34.02
min(sales$NA_Sales) #0
round(mean(sales$NA_Sales),2) #2.52

# EU Sales
max(sales$EU_Sales) #23.8
min(sales$EU_Sales) #0
round(mean(sales$EU_Sales),2) #1.64

# Global Sales
max(sales$Global_Sales) #67.85
min(sales$Global_Sales) # 0.01, note this was no zero sales in Global Sales
round(mean(sales$Global_Sales),2) #5.33

# View the descriptive statistics.
summary(sales1)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Transform the Product with an indicator to show if it's launched with
# various platform or not
# Create a df with Product launched with 1 platform only
single_platform<- sales1 %>% 
  add_count(Product) %>% 
  filter(n==1) %>% 
  distinct()

# Create a df with Product launched with 2 or more platforms
multi_platform<- sales1 %>% 
  add_count(Product) %>% 
  filter(n>1) %>% 
  distinct()
# Combine the Dataframe
df <- rbind(single_platform, multi_platform)
# View the result
View(df)
sum(is.na(df$n))

# Group data based on Product and determine the sum per Product.
sales_grouped <- df %>% 
  group_by(Product) %>% 
  summarise('no_of_platform' = mean(n),
            'Genre' = max(Genre),
            'EU_Sales'= sum(EU_Sales),
            'NA_Sales'= sum(NA_Sales),
            'Global_Sales'= sum(Global_Sales))

# View the data frame.
View(sales_grouped)


# Explore the data frame.
str(sales_grouped)
summary(sales_grouped)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
library(plotly)

ggplot(sales_grouped, aes(y=Global_Sales,x=no_of_platform))+
  geom_point(color='blue')+
  geom_smooth(method="lm", color='red')+
  ylab('Global Sales')+
  xlab('Number of Platform')+
  labs(title = "Product's Global Sales")


# There was an obvious pattern that the product launched with several platform
# had better sales result regardless of regional factor.

# Since investing to launch a product with different platform might incur 
# higher operating cost, we should study about the average sales across 
# instead of cumulative sales in order to see if the investment 
# shold be justified

# First decompose the dataframe into two groups
avg_plaftform_gsales <- sales_grouped %>% 
  filter(no_of_platform>1) %>% 
  mutate(Global_Sales/no_of_platform)
View(avg_plaftform_gsales)

single_platform_gsales <- sales_grouped %>% 
  filter(no_of_platform==1) %>% 
  mutate(Global_Sales/no_of_platform)
View(single_platform_gsales)

# Combine the dataframe again
df_global<-rbind(single_platform_gsales,avg_plaftform_gsales)
View(df_global)

# Visualise the result
ggplot(df_global)+
  geom_point(aes(x=Product, 
                 y=Global_Sales/no_of_platform, 
                 color=no_of_platform>1))+
  ylab('Global Sales')+
  xlab('Prduct ID')+
  labs(title = "Product's Global Sales per Platform")


# Products launched with multiple platforms showed that each platform will
# average out the total sales such that per platform income would be smaller
# than if just launched the product with only one single platform, by which
# each platform owner might be reluctant to implement such strategy without
# premium incentive to justify such losses.

# We then study the subset of product with the Genre features
# Statistics for product sales
View(df_global)

Genre_sales<-sales_grouped %>% 
  group_by(Genre) %>% 
  summarise('Global_Sales'=mean(Global_Sales),
            'EU_Sales'=mean(EU_Sales),
            'NA_Sales'=mean(NA_Sales)) %>% 
  arrange(desc(Global_Sales))

perct_sales<- Genre_sales %>% 
  mutate(Global_Sales/sum(Global_Sales)*100) %>% 
  mutate(EU_Sales/sum(EU_Sales)*100) %>% 
  mutate(NA_Sales/sum(NA_Sales)*100)
genre_perct<-perct_sales %>% select(-c(Global_Sales,EU_Sales,NA_Sales))
new_col_name <- c('Genre','Global_Sales(%)','EU_Sales(%)','NA_Sales(%)')
colnames(genre_perct) <- new_col_name

# View the top5 best selling genre class:
head(genre_perct)

# Global Sales
ggplot(sales_grouped)+
  geom_histogram(aes(x=Global_Sales,y = ..count.., fill=Genre),
                 bins = 35)+
  ylab('No. of Product')+
  xlab('Global Sales(millions)')+
  labs(title = 'Product Sales by Genre Globally')+
  theme_minimal()


# Create boxplots.
ggplot(sales_grouped, aes(x=Genre,y=Global_Sales))+
  geom_boxplot(aes(fill=Genre))+
  coord_flip()+
  labs(title="Product Sales Volumn(Global) by Genre class")+
  scale_y_continuous(breaks = c(0,50,5))

write.csv(sales_grouped, 'product_Sales.csv', row.names = FALSE)

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales_grouped$EU_Sales, col='green')
qqline(sales_grouped$EU_Sales,col='navy', lwd=2)

qqnorm(sales_grouped$NA_Sales)
qqline(sales_grouped$NA_Sales,col='grey', lwd=2)

qqnorm(sales_grouped$Global_Sales)
qqline(sales_grouped$Global_Sales,col='red', lwd=2)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages('moments')
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales_grouped$Global_Sales) # Not distributed as normal
shapiro.test(sales_grouped$EU_Sales) # Not Distributed as normal
shapiro.test(sales_grouped$NA_Sales) # Not Distributed as normal

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
# 1. Global Sales
skewness(sales_grouped$Global_Sales) #3.07, implies unsymmetrically distributed
kurtosis(sales_grouped$Global_Sales) # 17.8 implies excess Kurtosis


## 3d) Determine correlation
# Determine correlation.
cor(sales_grouped$EU_Sales, sales_grouped$Global_Sales) # 0.85
cor(sales_grouped$NA_Sales, sales_grouped$Global_Sales) # 0.91
###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.
#1. Global Sales
qqnorm(sales_grouped$Global_Sales)
qqline(sales_grouped$Global_Sales, col='red', lwd=2)

###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# The correlation coefficient between EU sales and Global sales near, and
# that between NA Sales and Global Sales, were both close to 1.
# There is high correlation between these variables.

# However, the Shapiro test, skewness and Kurtosis test are all indicating
# the sales distributed not as a normal distribution such that a Z-test
# statistics should not be advised for prediction.


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
product_sales <- read.csv('product_Sales.csv')

# Determine a summary of the data frame.
as_tibble(product_sales)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
SLR_EU <- lm(Global_Sales~EU_Sales, product_sales)
summary(SLR_EU)

SLR_NA <- lm(Global_Sales~NA_Sales, product_sales)
summary(SLR_NA)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
# 1, EU Sales
qqnorm(SLR_EU$residuals)
qqline(SLR_EU$residuals, col='red', lwd=2)

# NA Sales
qqnorm(SLR_NA$residuals)
qqline(SLR_NA$residuals, col='blue', lwd=2)


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
MLR <- lm(Global_Sales~EU_Sales+NA_Sales, product_sales)
summary(MLR) 
qqnorm(MLR$residuals)
qqline(MLR$residuals, col='red', lwd=2)
shapiro.test(MLR$residuals)

# Check for heteroskedasticity
library(lmtest)
bptest(MLR) 
# The test statistics p-value is 0.1718 which is larger than 0.05,
# So it is failed to reject the null hypothesis of homoskedasticity

# Check Multicollinearity
# Install and import necessary library
install.packages('car')
library(car)
vif(MLR)

# F-test of significant for the model.
# Establish null Hypothesis:
myH0<- c('EU_Sales=0','NA_Sales=0')
# Test Statistics:
linearHypothesis(MLR, myH0)
# The p-value of F-test is 2.2E-16 which is much smaller than 0.05,
# the null hypothesis coefficients of EU_Sales and NA_Sales being 0 is rejected.


###############################################################################

# 4. Predictions based on given values
# Given the sales of a product in EU and NA are 1 and 2 millions respectively.
# Then the Global sales would be 
df<-MLR$coefficients
df[1] + 1*df[2] + 2*df[3] # The predicted Global sales would be 4.5 millions

# Compare with observed values for a number of records.
# extract a row in the original data set
test<-product_sales %>% head(1) %>% 
  select(c('EU_Sales','NA_Sales','Global_Sales'))
pred <-df[1]+test['EU_Sales']*df[2] + test['NA_Sales']*df[3]
# predicted Global Sales = 68.06 while the actual Global Sales = 67.85
# The error is :
# (68.06-67.85)/67.85 = 0.003, i.e. 0.3%

###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# The gloabal sales is positively correlating to EU and NA sales with
# statistically significant predicting power.
###############################################################################
