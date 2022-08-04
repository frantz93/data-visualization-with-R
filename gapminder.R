library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
head(gapminder)
g <- gapminder

g %>% filter(year==2015 & country %in% c('Sri Lanka', 'Turkey')) %>% select(c('country', 'infant_mortality'))
summary(g['year'])

ds_theme_set()
g %>% filter(year==2015) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point()

g %>% filter(year %in% c(1962, 2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() +
  facet_grid(.~year)

g %>% filter(year %in% c(1962, 1970, 1980, 1990, 2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() +
  facet_wrap(.~year, ncol = 2)

g %>% filter(year %in% c(1962, 1970, 1980, 1990, 2012) & continent %in% c('Europe', 'Asia')) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() + facet_wrap(.~year, ncol = 2)

g %>% filter(year %in% c(1962, 2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() +
  facet_grid(continent~year)
