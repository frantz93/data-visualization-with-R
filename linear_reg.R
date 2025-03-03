
library(tidyverse)
library(HistData)

head(GaltonFamilies,4)
lengths(GaltonFamilies)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)


library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
avg

mean((avg - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)
#or
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

summary(fit)
((2.405^2)*86)/88
sum(fit$residuals**2)/88


#  Comprehensive check: Linear Regression

# 1- We create a data set with the following code
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# and we generate 100 linear models using the created data
y <- dat$y
set.seed(1, sample.kind="Rounding")
test_index <- replicate(n=100, 
          createDataPartition(y, times=1, p=0.5, list=FALSE),
          simplify = FALSE)

train_s = data.frame()
test_s = data.frame()
rmse_df = data.frame(RMSE=0)

for (i in 1:100){
  train_s = dat %>% slice(-test_index[[i]])
  test_s = dat %>% slice(test_index[[i]])
  fit <- lm(y ~ x, data = train_s)
  y_hat <- predict(fit, test_s)
  rmse <- sqrt(mean((y_hat - test_s$y)^2))
  rmse_df[i,1] <- rmse
}

round(mean(rmse_df$RMSE),3)
round(sd(rmse_df$RMSE),3)

# 2- We repeat the precedent exercice using large datasets
error <- function(n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  y <- dat$y
  test_index <- replicate(n=100, 
                          createDataPartition(y, times=1, p=0.5, list=FALSE),
                          simplify = FALSE)
  
  train_s = data.frame()
  test_s = data.frame()
  rmse_df = data.frame(RMSE=0)
  
  for (i in 1:100){
    train_s = dat %>% slice(-test_index[[i]])
    test_s = dat %>% slice(test_index[[i]])
    fit <- lm(y ~ x, data = train_s)
    y_hat <- predict(fit, test_s)
    rmse <- sqrt(mean((y_hat - test_s$y)^2))
    rmse_df[i,1] <- rmse
  }
  
  moy = round(mean(rmse_df$RMSE),3)
  ec_t = round(sd(rmse_df$RMSE),3)
  
  print(paste0('moyenne = ', moy))
  print(paste0('ecart-type = ', ec_t))
  
}

set.seed(1, sample.kind="Rounding")
error(100)
error(500)
error(1000)
error(5000)
error(10000)


# 3- We repeat the exercice from point 1 this time making the correlation between x and y larger
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# and we generate 100 linear models using the created data
y <- dat$y
set.seed(1, sample.kind="Rounding")
test_index <- replicate(n=100, 
                        createDataPartition(y, times=1, p=0.5, list=FALSE),
                        simplify = FALSE)

train_s = data.frame()
test_s = data.frame()
rmse_df = data.frame(RMSE=0)

for (i in 1:100){
  train_s = dat %>% slice(-test_index[[i]])
  test_s = dat %>% slice(test_index[[i]])
  fit <- lm(y ~ x, data = train_s)
  y_hat <- predict(fit, test_s)
  rmse <- sqrt(mean((y_hat - test_s$y)^2))
  rmse_df[i,1] <- rmse
}

round(mean(rmse_df$RMSE),3)
round(sd(rmse_df$RMSE),3)

# 4 - We create a data set using the code
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

# and we generate 3 linear models using as predictors respectively (x_1), (x_2) and (x_1, x_2)
y <- dat$y
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)

train_s = data.frame()
test_s = data.frame()

train_s = dat %>% slice(-test_index)
test_s = dat %>% slice(test_index)

fit1 <- lm(y ~ x_1, data = train_s)
fit2 <- lm(y ~ x_2, data = train_s)
fit12 <- lm(y ~ x_1 + x_2, data = train_s)

y_hat1 <- predict(fit1, test_s)
y_hat2 <- predict(fit2, test_s)
y_hat12 <- predict(fit12, test_s)

rmse1 <- sqrt(mean((y_hat1 - test_s$y)^2))
rmse2 <- sqrt(mean((y_hat2 - test_s$y)^2))
rmse12 <- sqrt(mean((y_hat12 - test_s$y)^2))

rmse1; rmse2; rmse12


# 5 - We repeat the previous exercice but setting x_1 and x_2 highly correlated
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

# and we generate 3 linear models using as predictors respectively (x_1), (x_2) and (x_1, x_2)
y <- dat$y
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)

train_s = data.frame()
test_s = data.frame()

train_s = dat %>% slice(-test_index)
test_s = dat %>% slice(test_index)

fit1 <- lm(y ~ x_1, data = train_s)
fit2 <- lm(y ~ x_2, data = train_s)
fit12 <- lm(y ~ x_1 + x_2, data = train_s)

y_hat1 <- predict(fit1, test_s)
y_hat2 <- predict(fit2, test_s)
y_hat12 <- predict(fit12, test_s)

rmse1 <- sqrt(mean((y_hat1 - test_s$y)^2))
rmse2 <- sqrt(mean((y_hat2 - test_s$y)^2))
rmse12 <- sqrt(mean((y_hat12 - test_s$y)^2))

rmse1; rmse2; rmse12



# Comprehensive Check: Logistic Regression

# set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

set.seed(1, sample.kind="Rounding")
mu_1 <- seq(0, 3, len=25)
d <- make_data(mu_1 = mu_1)

