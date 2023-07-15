library(tidyverse)
library(ggplot2)
health<-read.csv('insurance.csv')
ggplot(health, aes(jitter(age),charges))+
  geom_point()
# Remove outlier for charges over 50000
new_health <- health %>% 
  filter(charges<=50000)
ggplot(new_health, aes(age, charges, color=sex))+
  geom_point(alpha=0.5)+
  geom_smooth(se=F)+
  facet_wrap(~smoker)+
  labs(title = "Schedule of Charges against Age",
       subtitle = "by Gender",
       xlab("Age of the Individual"),
       ylab("Monthly Charges$"))

ggplot(new_health, aes(age, charges, color=region))+
  geom_point(alpha=0.5)+
  geom_smooth(se=F)

ggplot(new_health, aes(age, charges, color=children))+
  geom_point(alpha=0.5)+
  geom_smooth(se=F)         

ggplot(new_health, aes(age, charges, color=smoker))+
  geom_point(alpha=0.5)+
  geom_smooth(se=F)+
  facet_wrap(~children)

# Since the plot showed that the relationship was stable against
# all non smokers, let's focus on the smoker group in order to 
# see if any further insight
smoker<- new_health %>% 
  filter(smoker=='yes')
head(smoker)

# Visualise the data
ggplot(smoker, aes(age, charges,color=sex))+
  geom_point(alpha=0.5)+
  geom_smooth(method = lm, se=F)+
  scale_x_continuous(breaks = seq(0,70,5),"Age of Member")+
  scale_y_continuous(breaks = seq(0,55000,5000),"Monthly Charge")

## that showed that the charges for smoker group could be further 
## divided into male and female group with similar 
## marginal effect on age
