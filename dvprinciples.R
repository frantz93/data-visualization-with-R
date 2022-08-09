library(dslabs)
library(ggplot2)
library(dplyr)

data("heights")
head(heights)
h <- heights

h %>% ggplot(aes(sex, height, color = sex)) + geom_point()
h %>% ggplot(aes(sex, height, color = sex)) + geom_jitter(width = 0.1, alpha = 0.2)
h %>% ggplot(aes(sex, height, color = sex)) + geom_boxplot()
h %>% ggplot(aes(sex, height)) + geom_col(fill = 'blue')  #worst choice
h %>% ggplot(aes(height, y=..density..)) + geom_histogram(col='black') + facet_grid(sex~.) + scale_y_continuous(labels = scales::percent_format())

data("gapminder")
g <- gapminder
head(g)
tab = g %>% mutate(dpd = gdp/population/365) %>% filter(year %in% c(1970, 2010) & !is.na(dpd))
View(tab)
tab %>% ggplot(aes(continent, dpd)) + geom_boxplot(aes(fill = factor(year))) + scale_fill_discrete(name='Year') + scale_y_continuous(trans = 'log2')


#using color blind friendly colors
color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
  ggplot(aes(x, y, color = col)) +
  geom_point(size = 5)
p1
p1 + scale_color_manual(values = color_blind_friendly_cols)

#----------------ASSESSMENT---------------------
dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% 
  mutate(rate = (count / population) * 10000 * (52 / weeks_reporting))
head(dat)
state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)
levels(state)
state <- reorder(state, rate)

dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>% mutate(state = reorder(state, rate))
dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()

data("murders")
head(murders)
murders %>% mutate(rate = total/population*100000) %>% mutate(region = reorder(region, rate, FUN = 'median')) %>%
  ggplot(aes(region, rate)) + geom_boxplot() + geom_jitter(width = 0.1, alpha = 0.2)


