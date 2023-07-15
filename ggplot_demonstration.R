library(tidyverse)
# Import the data
wages<-read.csv('wages_plots.csv')
View(wages)
str(wages)
wages1<-select(wages,c('age','wage','education','job_class'))
# Visualise the data
ggplot(wages1, aes(x=age, y=wage))+
  geom_point(color='red',
             alpha=0.5,
             size=1.5)+
  geom_smooth(method = 'lm')

# Specify group of variables
ggplot(wages1, aes(x=age, y=wage,
                   color=education))+
  geom_point(alpha=0.5,
             size=1)+
  geom_smooth(method = 'lm',
              se=F,
              size=0.5)+
  scale_x_continuous(breaks = seq(0,90,5),
                     'Age of the Individual')+
  scale_y_continuous(breaks = seq(0,350,50),
                     'Wage in $1000s')+
  scale_color_manual(values=c('red','blue','green','orange','yellow'))+
  facet_wrap(~job_class)+
  # Add labels for title, subtitle, and caption.
  labs(title = "Relationship between wages and age",
       subtitle = "Survey from the mid-Atlantic region, USA",
       caption = "Source: US govt data") +
  theme_dark()
