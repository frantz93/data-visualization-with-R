library(dslabs)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

data("heights")
head(heights)
h <- heights

h %>% ggplot(aes(sex, height, color = sex)) + geom_point()
h %>% ggplot(aes(sex, height, color = sex)) + geom_jitter(width = 0.1, alpha = 0.2)
h %>% ggplot(aes(sex, height, color = sex)) + geom_boxplot()
h %>% ggplot(aes(sex, height)) + geom_col(fill = 'blue')  #bad choice
h %>% ggplot(aes('', height, fill=sex)) + geom_bar(stat = 'identity', width = 1) + coord_polar('y', start = 0) + 
  theme_void()#worst choice
h %>% ggplot(aes(height, y=..density..)) + geom_histogram(col='black') + facet_grid(sex~.) + scale_y_continuous(labels = scales::percent_format()) #best choice

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

#disease exercice
dis <- us_contagious_diseases
str(dis)
head(dis)

display.brewer.all()
display.brewer.all(type='seq')
display.brewer.all(type='div')

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

dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>% mutate(state = reorder(state, rate))
dat %>% ggplot(aes('', rate, fill = state)) +
  geom_bar(stat="identity", width = 1) +
  coord_polar('y', start = 0)

data("murders")
head(murders)
murders %>% mutate(rate = total/population*100000) %>% mutate(region = reorder(region, rate, FUN = 'median')) %>%
  ggplot(aes(region, rate)) + geom_boxplot() + geom_jitter(width = 0.1, alpha = 0.2)

#tile plot
dis <- us_contagious_diseases
head(dis)
summary(dis)

#building the graph of Measles
the_disease = "Measles"
dat <- dis %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))
head(dat)
summary(dat)

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("") + geom_vline(xintercept=1963, size=1, col = "blue")

#Building the graph of smallpox
the_disease = "Smallpox"
dat <- dis %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))
head(dat)
summary(dat)

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")

#showing the rate of Measles cases per population by state
the_disease = "Measles"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))
head(dat)
dat <- dat %>% mutate(rate2 = sqrt(rate))

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)
head(avg, 20)
avg <- avg %>% mutate(us_rate2 = sqrt(us_rate))

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt") + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")

dat %>% ggplot() +
  geom_line(aes(year, rate2, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate2),  data = avg, size = 1, color = "black") +
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=sqrt(50)), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")

 
#showing the rate of Smallpox cases per population by state
the_disease = "Smallpox"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))
head(dat)
dat <- dat %>% mutate(rate2 = sqrt(rate))

avg <- us_contagious_diseases %>%
  filter(disease==the_disease & weeks_reporting >= 10) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)
head(avg)
avg <- avg %>% mutate(us_rate2 = sqrt(us_rate))

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt") + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1934, y=2), mapping = aes(x, y, label="US average"), color="black")

dat %>% ggplot() +
  geom_line(aes(year, rate2, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate2),  data = avg, size = 1, color = "black") +
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1934, y=sqrt(2)), mapping = aes(x, y, label="US average"), color="black")


#looking at the rates of all diseases in one state
us_contagious_diseases %>% filter(state=="California" & weeks_reporting >= 10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, col=disease)) + 
  geom_line()


#making a time series plot for the rates of all diseases in the United States
us_contagious_diseases %>% filter(!is.na(population)) %>% group_by(disease, year) %>% 
  summarise(rate = sum(count)/sum(population)*10000) %>% ggplot(aes(year, rate, color=disease)) + geom_line()
