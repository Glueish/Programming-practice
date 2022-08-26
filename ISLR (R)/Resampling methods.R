library('ISLR2')
library(boot)
library('MASS')
library(class)
library(car)
library('e1071')

set.seed(1)
train <- sample(392, 196)
View(Auto)
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

attach(Auto)
mean((mpg - predict(lm.fit , Auto))[-train ]^2)

lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto,subset = train)
mean((mpg - predict(lm.fit2 , Auto))[-train ]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3 , Auto))[-train ]^2)

glm.fit <- glm(mpg ~ horsepower , data = Auto)
cv.err <- cv.glm(Auto , glm.fit)
cv.err$delta
cv.err

cv.error <- rep(x = NA, 10)

for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}


cv.error.10 <- rep(x = NA, 10)

for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.error.10
cv.error
cv.error < cv.error.10

View(Portfolio)
Portfolio[c(1,1,1,2),]

alpha.fn <- function(data , index) {
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}

alpha.fn(Portfolio , 1:100)
alpha.fn(Portfolio , sample (100 , 100, replace = T))
boot(data = Portfolio, statistic = alpha.fn, R = 1000)

2^3
#Exercise 2
v1 <- 1:1000
v2 <- 1:1000
v2 <- (1- (1 - (1/v1))^v1)
plot(v1,v2)
min(v2)

store <- rep(NA, 10000)
for(i in 1:10000){
  store[i] <- sum(sample (1:1000 , rep=TRUE) == 4) > 0
}
mean(store)

#Exercise 5
View(Default)
train <- sample(nrow(Default), nrow(Default)/2)
lgs.fit <- glm(default ~ balance+income+student, data = Default, subset = train, family = binomial)
summary(lgs.fit)
lgs.probs <- predict(object = lgs.fit, newdata = Default[-train,], type = 'response')
lgs.preds <- rep('No', length(lgs.probs))
lgs.preds[lgs.probs > 0.5] <- 'Yes'
lgs.table <- table(lgs.preds, Default[-train,]$default)
lgs.table

get_metrics <- function(name, tab){
  print(name)
  mat <- matrix(data = c((tab[1,1]+tab[2,2])/(tab[1,1]+tab[1,2]+tab[2,1]+tab[2,2]),tab[2,2]/(tab[2,2]+tab[1,2]), tab[1,1]/(tab[1,1]+tab[2,1]), tab[2,2]/(tab[2,2]+tab[2,1]), tab[1,1]/(tab[1,1]+tab[1,2])), nrow = 1)
  colnames(mat) <- c('accuracy', 'sensitivity', 'specifity', 'precision', 'NPV')
  print(tab)
  mat
}
get_metrics('logistics regression', lgs.table)

store <- c(0.9774, 0.9736, 0.975)
store2 <- c(0.9724, 0.9714, 0.9726)

t.test(store, store2)


#Exercise 6
View(Default)
train <- sample(nrow(Default), nrow(Default)/2)
lgs.fit <- glm(default ~ balance + income, data = Default, subset = train, family = binomial)
summary(lgs.fit)
lgs.fit$coefficients

#version from scratch

storage <- as.list(1:1000)

for (k in 1:1000) {
  lgs.boot <- glm(default ~ balance + income, 
                  data = Default, 
                  subset = sample(train, size = length(train), replace = T),
                  family = binomial)
  storage[[k]] <- lgs.boot$coefficients
}


v1 <- 1:1000
v2 <- 1:1000
v3 <- 1:1000

for (k in 1:1000) {
  v1[k] <- storage[[k]][1]
  v2[k] <- storage[[k]][2]
  v3[k] <- storage[[k]][3]
}

sd(v1)
sd(v2)
sd(v3)
summary(lgs.fit)$coefficients[, 2]
sd(v1) - summary(lgs.fit)$coefficients[, 2][1]
sd(v2)- summary(lgs.fit)$coefficients[, 2][2]
sd(v3) - summary(lgs.fit)$coefficients[, 2][3]

#version with boot
boot.fn <- function(data, index) {
  lgs.b <- glm(default ~ balance + income, data = Default, subset = index, family = binomial)
  return(coef(lgs.b))
}

boot(data = Default[train,], statistic = boot.fn, R = 1000)
summary(lgs.fit)$coefficients[, 2]


#Exercise 7
View(Weekly)
lgs.fit <- glm(Direction ~ Lag1 + Lag2, family = binomial, data = Weekly[-1,])
predict(lgs.fit, newdata = Weekly[1,], type = 'response')

storage <- 1:nrow(Weekly)

for (k in 1:nrow(Weekly)) {
  lgs.fit <-glm(Direction ~ Lag1 + Lag2, family = binomial, data = Weekly[-k,])
  prob <- predict(lgs.fit, newdata = Weekly[k,], type = 'response')
  if (prob > 0.5) {
    pred <- 'Up'
  } else { 
    pred <- 'Down'
  }
  if (pred == Weekly[k,]$Direction){
    storage[k] <- 1
  } else {
    storage[k] <- 0
  }
}

storage
acc <- mean(storage)
acc


#Exercise 8
set.seed(2)
x <- rnorm (100)
y <- x - 2 * x^2 + rnorm (100)
plot(x,y)
df1 <- data.frame(x,y)
View(df1)
glm.fit <- glm(y ~ poly(x, 1), data = df1)
cv.glm(df1, glm.fit)$delta

for (k in 1:4) {
  glm.fit <- glm(y ~ poly(x, k), data = df1)
  print(summary(glm.fit))
  print(cv.glm(df1, glm.fit, K = nrow(df1))$delta)
}


#Exercise 9
View(Boston)
?Boston
mu <- mean(Boston$medv)
mu
medv.se <- sd(Boston$medv)/sqrt(nrow(Boston))
medv.se

storage <- rep(NA, 1000)

for (k in 1:1000) {
  sample_vector <- sample(x = nrow(Boston), size = nrow(Boston), replace = T)
  data_vector <- Boston[sample_vector,]$medv
  mu <- mean(data_vector)
  storage[k] <- mu
}

sd(storage)
medv.se
t.test(Boston$medv)
boot_ci <- c(mean(storage) - 1.964673*sd(storage), mean(storage + 1.964673*sd(storage)))
boot_ci

median(Boston$medv)

storage <- rep(NA, 1000)
for (k in 1:1000) {
  sample_vector <- sample(x = nrow(Boston), size = nrow(Boston), replace = T)
  data_vector <- Boston[sample_vector,]$medv
  med <- median(data_vector)
  storage[k] <- med
}
mean(storage)
sd(storage)



quantile(Boston$medv, c(0.1))

storage <- rep(NA, 1000)
for (k in 1:1000) {
  sample_vector <- sample(x = nrow(Boston), size = nrow(Boston), replace = T)
  data_vector <- Boston[sample_vector,]$medv
  quant <- quantile(data_vector, c(0.1))
  storage[k] <- quant
}

quantile(Boston$medv, c(0.1))
mean(storage)
sd(storage)
