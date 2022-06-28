# Mapping the heights of male and female students

library(dslabs)
data("heights")
head(heights)

index <- heights$sex == "Male"
x <- heights$height[index]
length(x)
z <- scale(x)
sum(z)
length(z[abs(z)<2])/length(z)
abs(z)
mean(abs(z)<2)
abs(z)


#estimate probability with pnorm
summary(x)
1-pnorm(70.5, mean(x), sd(x))

#discretization and normal approximation
# 1-plot distribution of exact height in data
plot(prop.table(table(x)), type = 'h', xlab = "height", ylab = "proportion of height")
# 2- probability in actual data vs estimated pnorm over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))

#quantiles, percentiles and boxplots
quantile(x, 0.5)
p<- seq(0.05, 0.95, 0.05)
qx <- quantile(x, p)
quantile(x, c(0.25, 0.5, 0.75))
qnorm(0.975)
1-pnorm(1.96)

real_quantile <- quantile(x, p) #real quantiles of males' heights
theor_quantile <- qnorm(p, mean(x), sd(x)) #theoritical quantiles of the males' heights
plot(real_quantile, theor_quantile, type = 'p')
abline(0,1)
?abline()
oq <- quantile(z, p)
tq <- qnorm(p)
plot(tq, oq, type = 'p')
abline(0,1)


#Mapping with ggplot
library(ggplot2)
library(dplyr)
#build histograms of males'heights distribution
p1 <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height,)) + 
  geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- heights %>% filter(sex == "Female") %>% ggplot(aes(x = height,)) +
  geom_histogram(binwidth = 1, fill = "blue", col = "black")

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
#build q-q plot of Males' disctribution
p3 <- heights %>% filter(sex == "Male") %>% ggplot(aes(sample = scale(height),))
p3 + geom_qq() + geom_abline() + ggtitle("Q-Q plot for Males")
x <- filter(heights, sex == "Male")
shapiro.test(sample(x$height))

p4 <- heights %>% filter(sex == "Female") %>% ggplot(aes(sample = scale(height),))
p4 + geom_qq() + geom_abline() + ggtitle("Q-Q plot for Females")
y <- filter(heights, sex == "Female") 
shapiro.test(sample(y$height))

             