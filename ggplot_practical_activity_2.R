# Import the data file
health <- read.csv('insurance.csv')
# View and sense check the data set
str(health)
summary(health)
# The 'children' column displayed the data type is of integer but 
# instead this would be a categorical
health$children <- as.factor(health$children)
str(health)
# Sense check the 'charges' column with a boxplot
ggplot(health, aes(charges))+
  geom_boxplot(notch = T, fill='red',outlier.colour = 'black')
ggplot(health, aes(charges))+
  geom_histogram()

# Create individual plots to visualise and explore each of the 
# specified variables that best display the data. 
# (Hint: Before choosing which plot is best for each variable,
# identify whether the variable is categorical or quantitative data.) 
# The variables are:

# 1. age - Numerical
ggplot(health, aes(age))+
  geom_histogram(bins = 12)

#2. children - Categorical
table(health$children)
ggplot(health, aes(children))+
  geom_bar()

# 3.Region and Sex - Both are categorical
ggplot(health, aes(sex, fill=region))+
  geom_bar()

#4. smoker and sex - both are categorical
ggplot(health, aes(smoker, fill=sex))+
  geom_bar()

# 5. BMI and sex - one is continuous numerical and sex is categorical
ggplot(health, aes(sex, bmi))+
  geom_violin(fill='red')+
  geom_boxplot(fill='green',width=0.25,
               outlier.colour = 'green',
               outlier.shape = 'triangle')
# 6. BMI and region continuous numerical and categorical
ggplot(health, aes(region, bmi))+
  geom_violin(fill='red')+
  geom_boxplot(fill='blue', width = 0.25,
               outlier.colour = 'green',
               outlier.shape = 'square')
# 7. BMI and smoker (continuous and categorical)
ggplot(health, aes(smoker, bmi))+
  geom_violin(fill='red')+
  geom_boxplot(fill='orange', width = 0.25,
               outlier.colour = 'green',
               outlier.shape = 'square')+
  labs(title='BMI Comparison between smokers and non-smoker')+
  scale_x_discrete(labels=c('no'='Non-Smoker','yes'='Smoker'),
                   'Participants')+
  # For web page demonstration
  theme_minimal()

# 8. smoker and charges. (Categorical and continuous numerical)
ggplot(health, aes(smoker, charges))+
  geom_violin(fill='red')+
  geom_boxplot(fill='orange', width = 0.25,
               outlier.colour = 'green',
               outlier.shape = 'square')+
  labs(title = "Monthly Charges Distribution",
       subtitle = "By Smoker Group",
       caption = "Source: I4U health insurance")
