library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()


set.seed(1989)

library(HistData)

data("GaltonFamilies")

library(tidyverse)

female_heights = GaltonFamilies%>%
  
  filter(gender=="female")%>%
  
  group_by(family)%>%
  
  sample_n(1)%>%
  
  ungroup()%>%
  
  select(mother, childHeight)%>%
  
  rename(daughter="childHeight")

#Question 1
#Calculate the mean and standard deviation of mothers' heights, the mean and 
#standard deviation of daughters' heights, and the correlaton coefficient between
#mother and daughter heights.

#Mean of mothers' heights
m1=mean(female_heights$mother);m1

#Standard deviation of mothers' heights
s1=sd(female_heights$mother);s1

#Mean of daughters' heights
m2=mean(female_heights$daughter);m2

#Standard deviation of daughters' heights
s2=sd(female_heights$daughter);s2

#Correlation coefficient
cor=cor(female_heights$mother,female_heights$daughter);cor


#Question 2
#Calculate the slope and intercept of the regression line predicting daughters' 
#heights given mothers' heights. Given an increase in mother's height by 1 inch, 
#how many inches is the daughter's height expected to change?

#Slope of regression line predicting daughters' height from mothers' heights
slope = cor*s2/s1;slope

#Intercept of regression line predicting daughters' height from mothers' heights
intercept = m2-s2*cor*m1/s1;intercept

#Question 3

#What percent of the variability in daughter heights is explained by the 
#mother's height?
r_squared = cor^2 ; r_squared

#Question 4
#A mother has a height of 60 inches.

#Using the regression formula, what is the conditional expected value of her 
#daughter's height given the mother's height?
y = intercept + slope*60;y