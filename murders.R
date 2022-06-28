# Mapping the cases of Gun murders in USA states in 2010

library(dslabs)
data("murders")
head(murders)

summary(murders)
length(murders$region)

prop.table(table(murders$region))
barplot(table(murders$region), col = rainbow(4))
hist(murders$population)
table(murders$population<20000000 & murders$population>10000000)

library(ggplot2)
ggplot(murders)
?ggplot

library(dplyr)  
library(tidyverse)
p <- murders %>% ggplot()
class(p)

#plot the population*murders
p <- ggplot(murders)
p + geom_point(aes(x=population/10^6, y=total), size = 3) + geom_text(aes(population/10^6, total, label = abb), nudge_x = 1.5)
args(ggplot)
p <- ggplot(murders, aes(population/10^6, total, label = abb))
p + geom_point(size = 3) + geom_text(nudge_x = 1.5)
p + geom_point(size = 3) + geom_text(x=20, y=200, label = "Hello!")
p + geom_point(size = 3) + geom_text(nudge_x = 0.05) + scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10") + xlab("Population in millions (log scale)") + ylab("Total murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

library(ggrepel)
p <- ggplot(murders, aes(population/10^6, total, label = abb)) + geom_text_repel() + 
  scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + xlab("Population in millions (log scale)") + 
  ylab("Total murders (log scale)") + ggtitle("US Gun Murders in 2010")

p + geom_point(aes(col = region), size = 3)
murders[murders$abb=="NC",] %>% summarise(total/population*10^6)

log10(37253956)
log10(37.253956)
log10(1257)
10^1.571172

r <- murders %>% summarise(rate = sum(total)/sum(population)*10^6) %>% pull(rate)
r
log10(r)
p <- p + geom_abline(intercept = log10(r), lty = 2, col = "darkgrey") + geom_point(aes(col = region), size = 3) + 
  scale_color_discrete(name = "Region")

library(ggthemes)
p + theme_economist()
p + theme_economist_white()

p + theme_fivethirtyeight()

A <- data.frame(x = c(1,2,3), y = c(1,2,3))
A %>% ggplot(aes(x,y)) + geom_point(size=3) + geom_abline(intercept = 1)

#Building the plot recap
#1- load necessary libraries
library(tidyverse);library(ggthemes);library(ggrepel)
#2- define the mean line (r)
r <- murders %>% summarize(rate = sum(total)/sum(population)*10^6) %>% .$rate
p <- murders %>% ggplot(aes(x = population/10^6, y = total, label = abb)) +   scale_x_log10() + scale_y_log10() + 
  geom_abline(intercept = log10(r), lty = 2, col = "red") + geom_point(aes(col = region), size = 3) + geom_text_repel() + 
  xlab("Population en millions (log)") + ylab("Nombre de morts(log)") + ggtitle("Mortalité par arme à feu aux USA en 2010") +
  scale_color_discrete(name = "Region") + theme_economist()
p + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#Other personal exercice
View(population)
summary(population)
graph <- population %>% filter(year == 2013) %>% ggplot(aes(x=population/10^6))
graph + geom_histogram(fill = "skyblue", col = "black")
graph <- population %>% filter(year == 2013 & population > 10^8) %>% ggplot(aes(x=country, y=population, label = country))
graph + geom_point(size = 3) + geom_text_repel() + xlab("pays")
