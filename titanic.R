#----------------Titanic Assessment ---------------------#
options(digits = 3)
library(tidyverse)
library(titanic)

t <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
head(t)
summary(t)
?titanic_train

#we make the density of age grouped by sex
t %>% filter(!is.na(Age)) %>% ggplot(aes(Age, fill=Sex)) + geom_density(alpha = 0.2)
t %>% filter(!is.na(Age)) %>% ggplot(aes(Age, ..count.., fill=Sex)) + geom_density(alpha = 0.2) 
t %>% filter(!is.na(Age)) %>% ggplot(aes(Age, fill=Sex)) + geom_density(alpha = 0.2) + geom_vline(xintercept = 18, col = 'red') +
  geom_vline(xintercept = 35, col = 'blue')
t %>% filter(!is.na(Age)) %>% group_by(Sex) %>% summarise(prop = mean(Age < 35) - mean(Age <= 18))
t %>% filter(!is.na(Age)) %>% ggplot(aes(Age, fill=Sex)) + geom_density(alpha = 0.2) + geom_vline(xintercept = 17, col = 'red')

#we make a QQ-plot of the age
params <- t %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
t %>% filter(!is.na(Age)) %>% ggplot(aes(sample = Age)) + geom_qq(dparams = params) + geom_abline()

#ploting survive and age distribution
t %>% filter(!is.na(Survived) & !is.na(Sex)) %>% ggplot(aes(Survived, fill = Survived)) + geom_bar(stat = 'count')

t %>% filter(!is.na(Survived) & !is.na(Sex)) %>% ggplot(aes(Survived, fill = Sex)) + geom_bar(stat = 'count', position = 'dodge2')

t %>% filter(!is.na(Survived) & !is.na(Sex)) %>% ggplot(aes(Sex, fill = Survived)) + geom_bar(stat = 'count', position = 'dodge2')

t %>% filter(!is.na(Survived) & !is.na(Sex)) %>% ggplot(aes(Survived, fill = Sex)) + geom_bar(stat = 'count', position = 'stack')
t %>% filter(!is.na(Survived)) %>% summarise(mean = mean(Survived==1))
typeof(t$Survived)
mean(t$Survived==1)
