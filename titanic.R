#----------------Titanic Assessment ---------------------#
getwd()
options(digits = 3)
library(tidyverse)
library(titanic)
library(descr)

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

#making a density plot of age filled by survival status
t %>% filter(!is.na(Age) & !is.na(Survived)) %>% ggplot(aes(Age, ..count.., fill=Survived)) + geom_density(alpha=0.2)
summary(as.factor(t$Age))
t2 <- t %>% filter(!is.na(Age) & !is.na(Survived)) %>% mutate(age_range = case_when(
  .$Age <= 8 ~ ']0 - 8]',
  .$Age <= 10 ~ ']8 - 10]',
  .$Age <= 18 ~ ']10 - 18]',
  .$Age <= 30 ~ ']18 - 30]',
  .$Age <= 50 ~ ']30 - 50]',
  .$Age <= 70 ~ ']50 - 70]',
  .$Age <= 80 ~ ']70 - 80]',
)) %>% select(Survived, age_range)

t2 <- t %>% filter(!is.na(Age) & !is.na(Survived)) %>% mutate(age_range = case_when(
  .$Age < 8 ~ '[0 - 8[',
  .$Age < 10 ~ '[8 - 10[',
  .$Age < 18 ~ '[10 - 18[',
  .$Age < 30 ~ '[18 - 30[',
  .$Age < 50 ~ '[30 - 50[',
  .$Age < 70 ~ '[50 - 70[',
  .$Age <= 80 ~ '[70 - 80]',
)) %>% select(Survived, age_range)

crosstab(t2$age_range, t2$Survived)
CrossTable(t2$age_range, t2$Survived)
tab = CrossTable(t2$age_range, t2$Survived, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,
                 prop.chisq = FALSE, chisq = FALSE)
max(tab$prop.row)

#assessing survival status vs paid fares
t %>% filter(!is.na(Fare) & !is.na(Survived)) %>% ggplot(aes(x=Survived, y=Fare, fill = Survived)) + 
  geom_boxplot(width=0.5, alpha=0.5) + geom_jitter(width = 0.1, alpha = 0.2) + scale_y_continuous(trans = 'log2')
#making bar plots of passenger class vs survival status
t %>% filter(!is.na(Survived) & !is.na(Pclass)) %>% ggplot(aes(Pclass, fill = Survived)) + geom_bar(position = 'dodge2')
t %>% filter(!is.na(Survived) & !is.na(Pclass)) %>% ggplot(aes(Pclass, fill = Survived)) + geom_bar(position = 'fill')
t %>% filter(!is.na(Survived) & !is.na(Pclass)) %>% ggplot(aes(Survived, fill = Pclass)) + geom_bar(position = 'dodge2')
t %>% filter(!is.na(Survived) & !is.na(Pclass)) %>% ggplot(aes(Survived, fill = Pclass)) + geom_bar(position = 'fill')
t %>% filter(!is.na(Survived) & !is.na(Pclass)) %>% ggplot(aes(Pclass, fill = Survived)) + geom_bar(position = 'stack')

#png(filename = 'pic.png')    #try this command to save a plot in a specific file
t %>% filter(!is.na(Survived)) %>% summarise(status=c('survived', 'dead'), proportion = c(mean(Survived==1), dead = mean(Survived==0))) %>% 
  ggplot(aes(status,proportion,  fill = status)) + geom_bar(stat = 'identity') + scale_y_continuous(label = scales::percent_format()) +
  labs(title = 'Reported survival rates for Titanic passengers', caption = '©FRPDJ \n  Source: dslab') + theme(legend.position = 'none')
#dev.off()

#creating a density plot of age filled by survival status and faceted by sex and passenger class
t %>% filter(!is.na(Age) & !is.na(Sex) & !is.na(Pclass)) %>% ggplot(aes(Age, ..count.., fill = Survived)) + geom_density(alpha=0.3) + 
  facet_grid(Pclass~Sex)
t %>% filter(!is.na(Age) & !is.na(Sex) & !is.na(Pclass)) %>% ggplot(aes(Age, ..density.., fill = Sex)) + geom_density(alpha=0.3) + 
  facet_grid(Pclass~.)
