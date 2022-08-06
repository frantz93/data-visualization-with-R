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

#Faceting
g %>% filter(year %in% c(1962, 2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() +
  facet_grid(.~year)

g %>% filter(year %in% c(1962, 1970, 1980, 1990, 2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() +
  facet_wrap(.~year, ncol = 2)

g %>% filter(year %in% c(1962, 1970, 1980, 1990, 2012) & continent %in% c('Europe', 'Asia')) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() + facet_wrap(.~year, ncol = 2)

g %>% filter(year %in% c(1962, 2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() +
  facet_grid(continent~year)

#Time Series Plots
summary(g['country'], maxsum = 1000)
countries <- c('South Korea', 'Germany')
g %>% filter(country %in% countries) %>% ggplot(aes(year, life_expectancy, col=country)) + geom_line()

labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
g %>% filter(country %in% countries) %>% ggplot(aes(year, life_expectancy, color=country)) + geom_line() + 
  geom_text(data = labels, aes(x, y, label=country)) + theme(legend.position = 'none')

#Log Transformation
g <- g %>% mutate(dpd = gdp/population/365)
g <- g %>% mutate(log2_dpd = log2(dpd))
head(g)
    #Log Value transformation
g %>% filter(year==1970 & !is.na(log2_dpd)) %>% ggplot(aes(log2_dpd)) + geom_histogram(binwidth = 1, color = 'black')
    #Log Scale transformation
g %>% filter(year==1970 & !is.na(dpd)) %>% ggplot(aes(dpd)) + geom_histogram(binwidth = 1, color = 'black') + 
  scale_x_continuous(trans = 'log2')

g %>% filter(year==1970 & region=='Caribbean') %>% select(c('country', 'dpd')) %>% filter(dpd>45)#check the country in the Caribbean with highest dpd

#Stratify and Boxplot
levels(g$region)
length(levels(g$region)) #number of regions
    #boxplot by region in 1970
past_year <- 1970
p <- g %>% filter(year == past_year & !is.na(dpd)) %>% ggplot(aes(region, dpd, fill=continent))
p + geom_boxplot() #raw plot
p + geom_boxplot() + theme(axis.text.x = element_text(angle=90, hjust=1)) #plot with ajusted names on the x axis
    #reordering regions by median dpd for easier interpretation
p1 <- g %>% filter(year == past_year & !is.na(dpd)) %>% mutate(region = reorder(region, dpd, FUN=median)) %>%
  ggplot(aes(region, dpd, fill=continent)) + geom_boxplot() + theme(axis.text.x = element_text(size = 11, angle = 90, hjust = 1)) + 
  xlab("") + ylab("dollars per day")
p1  #plotting result
p1 + scale_y_continuous(trans = 'log2')   #plot with scale y log2 transformation. helps to see differences 
                                          #between region with smallest dpd values
p1 + scale_y_continuous(trans = 'log2') + geom_point(show.legend = FALSE)   #add data points to the plot

#Comparing distributions
#We will compare data for the western countries to developing countries
west <- c('Northern Europe', 'Northern America', 'Australia and New Zealand', 'Western Europe', 'Southern Europe')
p3 <- g %>% filter(year==past_year & !is.na(dpd)) %>% mutate(group = if_else(region %in% west, 'West', 'Developping')) %>%
  ggplot(aes(dpd, fill=group))
p3 + geom_histogram(binwidth = 1, col = 'black')  #here we plot the 2 distributions histogram
p3 + geom_histogram(binwidth = 1, col = 'black') + scale_x_continuous(trans = 'log2')  #plotting with scale x log 2 transformation
p3 + geom_histogram(binwidth = 1, col = 'black') + scale_x_continuous(trans = 'log2') + facet_grid(.~group)  #faceting two plots for with respect to each distribution
    #faceting by west/developing and year
present_year <- 2010
p4 <- g %>% filter(year %in% c(past_year, present_year) & !is.na(dpd)) %>% mutate(group = if_else(region %in% west, 'West', 'Developping')) %>%
  ggplot(aes(dpd, fill=group))
p4 + geom_histogram(binwidth = 1, col = 'black') + scale_x_continuous(trans = 'log2') + facet_grid(year~group)
    #comparing the years using boxplot
p5 <- g %>% filter(year %in% c(past_year, present_year) & !is.na(dpd)) %>% mutate(region = reorder(region, dpd, FUN=median)) %>%
  ggplot(aes(region, dpd)) + theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(trans = 'log2') + xlab("")
p5 + geom_boxplot(aes(fill=continent)) + facet_grid(year~.) + scale_fill_discrete(name='Continent')  #by faceting graphs
p5 + geom_boxplot(aes(fill=factor(year))) + scale_fill_discrete(name='Year') #on same graph


#Density Plots

#first, let's built a list of countries which data exist in both 1970 and 2010 years
list1 <- g %>% filter(year==1970 & !is.na(dpd)) %>% .$country
list2 <- g %>% filter(year==2010 & !is.na(dpd)) %>% .$country
countries <- intersect(list1, list2)

years <- c(past_year, present_year)
p6 <- g %>% filter(year %in% years & country %in% countries) %>% ggplot(aes(dpd))
#a faceted smooth density plots - area under each curves adds to 1
p6 + facet_grid(year~.) + geom_density(alpha = 0.5, bw = 0.75, fill='blue') + scale_x_continuous(trans = 'log2')

#note we have 174 developing countries and 42 developed countries
g %>% filter(year %in% years & country %in% countries) %>% 
  mutate(group = if_else(region %in% west, 'West', 'Developing')) %>% group_by(group) %>% summarise(n=n()) %>% 
  knitr::kable()
#we can facet the density of the two distributions (the two groups) together for each year
p7 <- g %>% filter(year %in% years & country %in% countries) %>% 
  mutate(group = if_else(region %in% west, 'West', 'Developing')) %>% ggplot(aes(dpd))
p7 + geom_density(alpha = 0.2, aes(fill=group)) + facet_grid(year~.) + scale_x_continuous(trans = 'log2')   #plot the density in the y axis
p8 <- g %>% filter(year %in% years & country %in% countries) %>% 
  mutate(group = if_else(region %in% west, 'West', 'Developing')) %>% ggplot(aes(x = dpd, y = ..count..))
p8 + geom_density(alpha = 0.2, bw = 0.75, aes(fill=group)) + facet_grid(year~.) + scale_x_continuous(trans = 'log2')   #plot the numbers in the y axis

g <- g %>% mutate(group = case_when(
  .$region %in% west ~ 'West',
  .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ 'East Asia',
  .$region %in% c("Caribbean", "Central America", "South America") ~ 'Latin America',
  .$continent == "Africa" & .$region != "Northern Africa" ~ 'Sub-Saharan Africa',
  TRUE ~ 'Others'))

g$group <- factor(g$group, levels = c('Others', 'Latin America', 'East Asia', 'Sub-Saharan Africa', 'West'))

p9 <- g %>% filter(year %in% years & country %in% countries) %>% ggplot(aes(x = dpd, y = ..count..))
p9 + geom_density(alpha = 0.2, bw = 0.75, aes(fill=group), position = 'stack') + facet_grid(year~.) + scale_x_continuous(trans = 'log2') 

#setting a weight regarding population size
g <- g %>% filter(year %in% years & country %in% countries) %>% group_by(year) %>% mutate(weight = population/sum(population*2)) %>% ungroup()

#new graph
p10 <- g %>% filter(year %in% years & country %in% countries) %>% ggplot(aes(dpd, weight = weight))
p10 + geom_density(alpha = 0.2, bw = 0.75, aes(fill=group), position = 'stack') + facet_grid(year~.) + scale_x_continuous(trans = 'log2')

g %>% filter(year %in% years & country %in% countries) %>% group_by(year) %>% mutate(weight = population/sum(population*2)) %>% ungroup() %>% 
  ggplot(aes(dpd, fill = group, weight = weight)) + scale_x_continuous(trans = 'log2') + geom_density(alpha = 0.2, bw = 0.75, position = 'stack') +
  facet_grid(year~.)


#Ecological fallacy
g <- g %>% mutate(group = case_when(
  .$region %in% west ~ 'The West',
  .$region %in% 'Northern Africa' ~ 'Northern Africa',
  .$region %in% c('Eastern Asia', 'South-Eastern Asia') ~ 'East Asia',
  .$region %in% 'Southern Asia' ~ 'Southern Asia',
  .$region %in% c('Caribbean', 'Central America', 'South America') ~ 'Latin America',
  .$continent == 'Africa' & .$region != 'Northern Africa' ~ 'Sub-Saharan Africa',
  .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))
    
    # define a data frame with group average income and average infant survival rate
surv_income <- g %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

    # plot infant survival versus income, with transformed axes
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) 

