options(digits = 3)
library(dslabs)
library(tidyverse)
library(ggrepel)
data("stars")

str(stars)
head(stars)
s <- stars

#check the mean and std of magnitude
mean(s$magnitude)
sd(s$magnitude)

#make a density plot of magnitude
ggplot(data = s, aes(magnitude)) + geom_density()  # peaks observed in magnitude

#inspecting the distribution of temperature
s %>% ggplot(aes(temp)) + geom_histogram(col='black')   #the majority of stars have low temperature
s %>% ggplot(aes(sample=scale(temp))) + geom_qq() + geom_abline(slope = sd(scale(s$temp)), intercept = mean(scale(s$temp)))  #the distribution in not normal

#plotting magnitude versus temperature
s %>% ggplot(aes(temp, magnitude)) + geom_point()  #we see the distribution of most stars follow a decreasing exponential curve

t <- seq(0, 1, 0.01)
et <- exp(t)
d <- data.frame(val=t, expo_val=et)
d %>% ggplot(aes(val, expo_val)) + geom_line()  #the exp curve shape

s %>% ggplot(aes(temp, magnitude)) + geom_point() + scale_x_continuous(trans = 'log10') + scale_y_reverse() + scale_x_reverse()
s %>% ggplot(aes(log10(temp), magnitude)) + geom_point()+ scale_y_reverse() + scale_x_reverse()
s %>% ggplot(aes(temp, magnitude, label=star)) + geom_point() + scale_x_continuous(trans = 'log10') + scale_y_reverse() + scale_x_reverse() + 
  geom_text_repel()
s %>% ggplot(aes(temp, magnitude, color=type)) + geom_point(size=3) + scale_x_continuous(trans = 'log10') + scale_y_reverse() + scale_x_reverse() +
  theme(legend.position = 'right') + 
  scale_colour_manual(values = c("#000000", "#AAAAAA", "#0022BB", "#22BB00", "#CCCCCC", "#CC00CC", "#CCCC00", "#00CCCC", "#CC0000", "#888888"))


