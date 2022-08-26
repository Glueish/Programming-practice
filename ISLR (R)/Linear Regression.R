library('ISLR2')
library('MASS')
library(car)

# Predicting medv with lstat simple linear
View(Boston)
?Boston
attach(Boston)
lm.fit <- lm(medv ~ lstat)
lm.fit
summary(lm.fit)
plot(lm.fit)
plot(lstat, medv)
predict(object = lm.fit, newdata = data.frame(lstat = c(5, 10, 15)), interval =  'confidence')
predict(object = lm.fit, newdata = data.frame(lstat = c(5, 10, 15)), interval =  'prediction')
plot(lstat, medv, pch = 1)
abline(lm.fit, lwd = 2, col = 'red')

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

#Playing with different multiple regression models
lm.fit2 <- lm(medv ~ ., data = Boston)
summary(lm.fit2)

# Predicting medv with lstat + sqrt(lstat)
lm.fit <- lm(medv ~ lstat + sqrt(lstat))
summary(lm.fit)
plot(lm.fit)

# Multiple Linear Regression
lm.fit <- lm(medv ~ lstat + age)
lm.fitall <- lm(medv ~ ., data = Boston)
summary(lm.fit)
summary(lm.fitall)
cor(age, lstat)
vif(lm.fitall)

#Multiple Linear Regression cd.
lm.fit <- lm(medv ~ . -tax -age -indus, data = Boston)
summary(lm.fit)
vif(lm.fit)
plot(lm.fit)


#Interaction term
lm.fit <- lm(medv ~ lstat*black, data = Boston)
summary(lm.fit)
vif(lm.fit)
plot(lm.fit)


#Polynomial regression
lm.fit <- lm(medv ~ lstat, data = Boston)
lm.fit2 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3) + I(lstat^4), data = Boston)
lm.fit3 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit2)
summary(lm.fit3)
plot(lm.fit)
anova(lm.fit, lm.fit2, lm.fit3)
vif(lm.fit2)

#Log transformations
plot(log(rm), medv)
lm.fit <- lm(medv ~ rm)
summary(lm.fit)
plot(lm.fit)

lm.fit2 <- lm(medv ~ log(rm))
summary(lm.fit2)


#Carseats data, qualitative predictors
?Carseats
View(Carseats)
attach(Carseats)
pairs(Carseats[2:4])
lm.fit <- lm(Sales ~. + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)
contrasts(ShelveLoc)
?contrasts

#Exercise 1
#Null hypothesis - there is no impact on sales while changing the value of a given predictor
#TV and Radio themselves have positive impact on sales while taking account also for the newspaper
#expenditures.

#Exercise 2
#KNN Classifier is used to predict qualitative variables, classes. On the other hand KNN regression
#methods predict quantitative variable. 

#Exercise 4
#a) We might expect likely overfitting from the cubic regression if the errors are high enough. 
#b) We would expect higher errors (measures by RSS) in the cubic regression, since it does not match
#the true underlying functional form while the linear regression does
#c) We would expect lower from cubic regression since it would likely better match the true
#functional form
#d) Same as c, but it depends on how far the functional form is from linear.

#Exercise 8
attach(Auto)
View(Auto)
?Auto
lm.fit <- lm(mpg ~ horsepower)
lm.fit2 <- lm(mpg ~ poly(horsepower, 2))
plot(horsepower, mpg)
abline(lm.fit, col = 'red')
summary(lm.fit)
summary(lm.fit2)
anova(lm.fit, lm.fit2)
plot(lm.fit)
plot(lm.fit2)
#iv
predict(lm.fit, newdata = data.frame(horsepower = c(98)), interval = 'confidence')
predict(lm.fit, newdata = data.frame(horsepower = c(98)), interval = 'prediction', level = 0.95)
#Exercise 9
pairs(Auto[1:3])
cor(Auto[1:8], Auto[1:8])
lm.fit <- lm(mpg ~ . -name, data = Auto)
summary(lm.fit)
plot(lm.fit)
vif(lm.fit)

lm.fit <- lm(mpg ~ . -name -weight -displacement, data = Auto)

lm.fit <- lm(mpg ~ cylinders*displacement + weight + year*origin + horsepower*acceleration, data = Auto)

lm.fit <- lm(mpg ~ cylinders*displacement*horsepower*acceleration)

lm.fit <- lm(mpg ~ log(cylinders))
plot(log(displacement), mpg)
plot(displacement, mpg)

avPlots(lm.fit, layout = c(1,1))

#Exercise 10
attach(Carseats)
View(Carseats)
?Carseats
lm.fit <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit)
plot(lm.fit)
cor(Carseats[1:6], Carseats[1:6])

lm.fit <- lm(Sales ~ Price + US)
summary(lm.fit)
predict(object = lm.fit, newdata = data.frame(Sales = c(1,2,3)), interval = 'confidence')
confint(lm.fit, level = 0.95)

#Exercise 11
set.seed (1)
x <- rnorm(100)
y <- 2 * x + rnorm(100)
lm.fit <- lm(y~x-1)
lm.fit2 <- lm(y~x)
summary(lm.fit2)
plot(x,y)
abline(lm.fit, col = 'red')
plot(lm.fit)

lm.fit3 <- lm(x~y-1)
lm.fit4 <- lm(x~y)
summary(lm.fit4)
plot(y,x)
abline(lm.fit3, col='red')
plot(lm.fit3)

#Exercise 12
#a) under assumption that sum of squares(x_i) is the same as sum of squares (y_i)
#b)
x = rnorm(100)
y = rnorm(100)
summary(lm(x~y-1))
summary(lm(y~x-1))
plot(x,y)

#c)
x <- c(1,2,3,4,5,6,7,8,9,10)
sum(x^2)

y <- c(-4, -8, -1, -6, -9, -10, -3, -5, -7, -2)
sum(y^2)

summary(lm(y~x-1))
summary(lm(x~y-1))
plot(x,y)
plot(y,x)

#Exercise 13
set.seed(1)
x <- rnorm(100)
x
eps <- rnorm(n = 100, sd = 2)
y <- -1 + 0.5*x +eps
length(y)
plot(x,y)
lm.fit <- lm(y~x)
abline(lm.fit, col = 'red')
abline(a = -1, b = 0.5, col = 'blue')
summary(lm.fit)
plot(lm.fit)
legend(0.5, -1.5, text.font = 4,legend = c('population line', 'OLS best fit'), col = c('red', 'blue'), cex = 0.6, lty = 1:2)
lm.fit2 <- lm(y~ poly(x, 2))
summary(lm.fit)
summary(lm.fit2)
plot(lm.fit2)

#Exercise 14
set.seed (1)
x1 <- runif (100)
x2 <- 0.5 * x1 + rnorm (100) / 10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm (100)
plot(x1,x2)
cor(x1,x2)
lm.fit <- lm(y ~ x1 + x2)
summary(lm.fit)
lm.fit2 <- lm(y ~ x1)
lm.fit3 <- lm(y ~ x2)
summary(lm.fit2)
summary(lm.fit3)

x1 <- c(x1 , 0.1)
x2 <- c(x2 , 0.8)
y <- c(y, 6)
plot(lm.fit)

#Exercise 15
attach(Boston)
View(Boston)
?Boston

lm.fit <- lm(crim ~ ., data = Boston)
summary(lm.fit)
vif(lm.fit)
plot(lm.fit)
y <- lm.fit$coefficients[2:14]
x <- c(lm.fit1$coefficients[2],lm.fit2$coefficients[2],lm.fit3$coefficients[2],lm.fit4$coefficients[2],lm.fit5$coefficients[2],lm.fit6$coefficients[2],lm.fit7$coefficients[2],lm.fit8$coefficients[2],lm.fit9$coefficients[2],lm.fit10$coefficients[2],lm.fit11$coefficients[2],lm.fit12$coefficients[2],lm.fit13$coefficients[2])
plot(x,y)