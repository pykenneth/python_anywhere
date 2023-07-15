install.packages('GGally')
install.packages('psych')
install.packages('car') # for VIF 
library(GGally)
# Import CSV
wine <- read.csv('wine.csv')
str(wine)
# Check missing values
sum(is.na(wine))


# Visualise the data
ggpairs(wine)

# Explore the data with correlation
cor(wine)

# Visualise the correlation matrix
library(psych)
# Use the corPlot() function.
# Specify the data frame (wine) and set 
# character size (cex=2).
corPlot(wine)

# Now we'll build a model with the wine data. 
# Price is the dependent variable we're trying to predict. 
# We'll use two explanatory variables:
# AGST, which has a relatively strong correlation with price (0.66),
# and HarvestRain, which has a relatively weak correlation with price (-0.56).
model_a <- lm(Price~AGST+HarvestRain, data=wine)
summary(model_a)

# Multiple R2 = 0.7074, Adj R2 = 0.6808

# Add X_variables ~WinterRain, Age, FrancePop into the model_a
model_b <- lm(Price~AGST+HarvestRain+WinterRain+Age+FrancePop, 
              data=wine)
summary(model_b)
## Multiple R2 = 0.8294, Adj R2 = 0.7845

# Improve the model
# Since the variables Age and FrancePop were reported not significant,
# we would remove one by one to see if any further improvement on R2
model_c <- lm(Price~AGST+HarvestRain+WinterRain+Age,
               data=wine)
summary(model_c)              
## ## Multiple R2 = 0.8286, Adj R2 = 0.7943

# Remove 'Age', and change the model name.
modeld = lm(Price~AGST+HarvestRain+WinterRain,
            data=wine)
summary(modeld) 

# F-test of restriction
library(car)
myh0 <- c('FrancePop')
linearHypothesis(model_b,myh0)
# Since the p-value is >0.05, it is not able to reject the H0
# and the restricted model is better, i.e. coef of FrancePop=0

# Load the new data file (wine_test.csv), and view its structure.
winetest <- read.csv('wine_test.csv', header=TRUE)

# View the data.
str(winetest)

# Create a new object and specify the predict function.
predictTest = predict(model_c, newdata=winetest,
                      interval='confidence')

# Print the object.
predictTest 
