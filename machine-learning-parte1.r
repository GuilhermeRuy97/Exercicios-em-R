library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#Pesquisando sobre o dataset
#pesquisa_1 <- dat %>% filter(dat$sex == "Male")
#pesquisa_1

#Conclusoes pesquisa
#Online: dominancia da presenca de homens
#Inclass: dominancia da presenca de mulheres
#150 pessoas no total
#39 pessoas inclass
#111 pessoas online
#82 homens
#68 mulheres


#Prever o sexo da pessoa pelo tipo de aula
y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)

#Calculando a sensitivity da funcao anterior com o y original
#sensitivity(y_hat, y)

#Calculando a specificity da funcao anterior com o y original
#specificity(y_hat, y)