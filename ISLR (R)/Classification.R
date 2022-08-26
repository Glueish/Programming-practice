library('ISLR2')
library('MASS')
library(class)
library(car)
install.packages("e1071")
library('e1071')

#Data inspection
View(Smarket)
?Smarket
pairs(Smarket[c(8:9)])
cor(Smarket[-9])
attach(Smarket)
plot(Volume)

#Logistic regression
glm.fits <- glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = Smarket)
summary(glm.fits)
plot(glm.fits)
predict(glm.fits)
predict(glm.fits, type = 'response')[0:10]
contrasts(Direction)
length(predict(glm.fits))
glm.pred = rep(x = 'Down', times = 1250)
glm.pred[predict(glm.fits, type = 'response') > 0.5] <- 'Up'
glm.pred
table(glm.pred, Direction)
mean(glm.pred == Direction)

#Dividing between training and test data
train <- (Year < 2005)
train
!c(TRUE, FALSE, TRUE, TRUE, TRUE)
Smarket.2005 <- Smarket[!train,]
Direction.2005 <- Direction[!train]
length(Smarket.2005)
Smarket.2005
View(Smarket.2005)
Direction.2005

glm.fits <- glm(Direction ∼ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                data = Smarket,
                family = binomial,
                subset = train)
summary(glm.fits)
glm.probs <- predict(object = glm.fits, Smarket.2005, type = 'response', )
glm.pred <- rep('Down', 252)
glm.pred[glm.probs > 0.5] <- 'Up'
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

#Developing model which takes into the account only two previous days and considers only strong returns
Smarket.strong <- Smarket[abs(Smarket$Lag1) >0.5 & abs(Smarket$Lag2) >0.5 & (Smarket$Lag1*Smarket$Lag2)>0,]
Smarket.strong
View(Smarket.strong)
Smarket.train <- Smarket.strong[Smarket.strong$Year != 2002,]
Smarket.test <- Smarket.strong[Smarket.strong$Year == 2002,]
glm.fits <- glm(Direction ∼ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume , data = Smarket.train , family = binomial)
glm.probs <- predict(object = glm.fits, type = 'response', newdata = Smarket.test)
glm.pred <- rep('Down', nrow(Smarket.test))
glm.pred[glm.probs > 0.5] <- 'Up'
mean(glm.pred == Smarket.test$Direction)
table(glm.pred, Smarket.test$Direction)

#Linear discriminant analysis
train <- (Year < 2005)
lda.fit <- lda(Direction ∼ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
summary(lda.fit)
plot(lda.fit)
lda.pred <- predict(lda.fit, Smarket.2005)
lda.pred
table(lda.pred$class, Direction.2005)
max(lda.pred$posterior[,1])
max(lda.pred$posterior[,2])

#Quadratic discriminant analysis
qda.fit <- qda(Direction ∼ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit
qda.pred <- predict(qda.fit, Smarket.2005)
table(qda.pred$class, Direction.2005)
mean(qda.pred$class == Direction.2005)
summary(qda.fit)
qda.fit$scaling

#Naive Bayes
nb.fit <- naiveBayes(Direction ∼ Lag1 + Lag2, data = Smarket, subset = train)
nb.fit
nb.pred <- predict(nb.fit, Smarket.2005)
nb.pred
table(nb.pred, Direction.2005)
mean(nb.pred == Direction.2005)
nb.pred_raw <- predict(nb.fit, Smarket.2005, type = 'raw')
nb.pred_raw

#KNN
library(class)
attach(Smarket)
train.X <- cbind(Lag1, Lag2)[train,]
head(train.X)
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]
test.Direction <- Direction.2005
set.seed(1)
knn.pred <- knn(train = train.X, test = test.X, k = 1, cl = train.Direction)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

dim(train.X)
dim(test.X)

names(Caravan)
attach(Caravan)
summary(Purchase)
standardized.X <- scale(Caravan[,-86])
View(Caravan)
View(standardized.X)
test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train = train.X, test = test.X, cl = train.Y, k = 5)
mean(knn.pred == test.Y)
table(knn.pred,test.Y)


#Poisson regression
attach(Bikeshare)
View(Bikeshare)
?Bikeshare

mod.pois <- glm(bikers ~ mnth + hr + workingday + temp + weathersit, family = poisson, data=Bikeshare)
summary(mod.pois)
plot(mod.pois)
coef.mnth <- c(coef(mod.pois)[2:12], -sum(coef(mod.pois)[2:12]))
plot(coef.mnth , xlab = "Month", ylab = "Coefficient", xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M",
                                       "J", "J", "A", "S", "O", "N", "D"))
coef.hours <- c(coef(mod.pois)[13:35],
                  -sum(coef(mod.pois)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient",
       col = "blue", pch = 19, type = "o")

contrasts(Bikeshare$hr) = contr.sum (24)
contrasts(Bikeshare$mnth) = contr.sum (12)
mod.lm2 <- lm(bikers ∼ mnth + hr + workingday + temp + weathersit , data = Bikeshare)

plot(predict(mod.lm2), predict(mod.pois , type = "response"))
abline (0, 1, col = 2, lwd = 3)

#Task 13
attach(Weekly)
View(Weekly)
?Weekly
all(!is.na.data.frame(Weekly))
#The variability of Lag variables differ accros different years
plot(Weekly[c(1,2)])
#The volume seems to increase exponentially with years
plot( Weekly$Year, Weekly$Volume)

#Logistic regression
lgs <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = Weekly)
summary(lgs)
lgs.prob <- predict(lgs, newdata = Weekly, type = 'response')
lgs.pred <- rep('Down', length(lgs.prob))
lgs.pred[lgs.prob>0.5] <- 'Up'
lgs.table <- table(lgs.pred, Weekly$Direction)
mean(lgs.pred == Weekly$Direction)
sensitivity <- 557/(557+48)
specifity <- 54/(54+430)
precision <- 557/(557+430)
negative_predictive_value <- 54/(54+48)

#d)
train_arr <- Weekly$Year < 2009
train_arr
nrow(Weekly) == length(train_arr)
lgs2 <- glm(Direction ~ Lag2, family = binomial, data = Weekly, subset = train_arr)
lgs2.prob <- predict(lgs2, newdata = Weekly[!train_arr,], type = 'response')
lgs2.pred <- rep('Down', length(lgs2.prob))
lgs2.pred[lgs2.prob>0.5] <- 'Up'
lgs2.table <- table(lgs2.pred, Weekly[!train_arr,]$Direction)
mean(lgs2.pred == Weekly[!train_arr,]$Direction)
library(plyr)
count(Weekly[!train_arr,]$Direction)
61/104

#lda
lda.fit <- lda(Direction ~ Lag2, data = Weekly, subset = train_arr)
lda.pred <- predict(lda.fit, newdata = Weekly[!train_arr,])
lda.table <- table(lda.pred$class, Weekly[!train_arr,]$Direction)

#qda
qda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = train_arr)
qda.pred <- predict(qda.fit, Weekly[!train_arr,])
length(qda.pred)
qda.pred$class
qda.table <- table(qda.pred$class, Weekly[!train_arr,]$Direction)

#knn
knn.fit <- knn(train = as.matrix(Weekly[train_arr,]$Lag2), test = as.matrix(Weekly[!train_arr,]$Lag2), cl = Weekly[train_arr,]$Direction, k = 1)
knn.fit
knn.table <- table(knn.fit, Weekly[!train_arr,]$Direction)
knn.table

#Naive Bayes
nb.fit <- naiveBayes(Direction ~ Lag2, data = Weekly, subset = train_arr)
nb.pred <- predict(nb.fit, newdata = Weekly[!train_arr,])
nb.pred
nb.table <- table(nb.pred, Weekly[!train_arr,]$Direction)

get_metrics <- function(name, tab){
  print(name)
  mat <- matrix(data = c((tab[1,1]+tab[2,2])/(tab[1,1]+tab[1,2]+tab[2,1]+tab[2,2]),tab[2,2]/(tab[2,2]+tab[1,2]), tab[1,1]/(tab[1,1]+tab[2,1]), tab[2,2]/(tab[2,2]+tab[1,2]), tab[1,1]/(tab[1,1]+tab[1,2])), nrow = 1)
  colnames(mat) <- c('accuracy', 'sensitivity', 'specifity', 'precision', 'NPV')
  print(tab)
  mat
}

get_metrics('lgs2',  lgs2.table)
get_metrics('lda',  lda.table)
get_metrics('qda',  qda.table)
get_metrics('knn',  knn.table)
get_metrics('nb',  nb.table)

#Experimenting j)
View(Weekly)

#knn3
knn3.fit <- knn(train = as.matrix(Weekly[train_arr,]$Lag2), test = as.matrix(Weekly[!train_arr,]$Lag2), cl = Weekly[train_arr,]$Direction, k = 3)
knn3.fit
knn3.table <- table(knn3.fit, Weekly[!train_arr,]$Direction)
knn3.table
get_metrics('knn3',  knn3.table)

#Developing model which takes into the account only two previous days and considers only strong returns
Weekly.strong <- Weekly[abs(Weekly$Lag1) >0.5 & abs(Weekly$Lag2) >0.5 & (Weekly$Lag1*Weekly$Lag2)>0,]
Weekly.strong
View(Weekly.strong)
train <- Weekly.strong$Year != 2002 & Weekly.strong$Year != 2009 & Weekly.strong$Year != 1995
train
Weekly.train <- Weekly.strong[train,]
Weekly.test <- Weekly.strong[!train,]
glm.fits <- glm(Direction ∼ Lag1 + Lag2, data = Weekly.train , family = binomial)
glm.probs <- predict(object = glm.fits, type = 'response', newdata = Weekly.test)
glm.pred <- rep('Down', nrow(Weekly.test))
glm.pred[glm.probs > 0.5] <- 'Up'
mean(glm.pred == Weekly.test$Direction)
table(glm.pred, Weekly.test$Direction)
get_metrics('lgs', table(glm.pred, Weekly.test$Direction))

#lda2
lda2.fit <- lda(Direction ~ Lag1 + Lag2, data = Weekly, subset = train)
lda2.pred <- predict(lda2.fit, newdata = Weekly[!train_arr,])
lda2.table <- table(lda2.pred$class, Weekly[!train_arr,]$Direction)
get_metrics('lda2', lda2.table)

#Exercise 14
View(Auto_1)
?Auto
formula_1 <- mpg01 ~ cylinders + weight + acceleration
formula_2 <- mpg01 ~ cylinders*displacement + weight + year*origin + horsepower*acceleration
mpg01 <- rep(0, nrow(Auto))
Auto_1 <- Auto
Auto_1$mpg01 <- mpg01
Auto_1$mpg01[Auto_1$mpg > median(Auto_1$mpg)] <- 1
ncol(Auto_1)
plot(Auto_1[c(10,10)])
plot(Auto_1$mpg)
boxplot(Auto_1[c(5,6,7,8,10)])
cor(Auto_1[-9])

train_arr <- rep(TRUE, nrow(Auto_1))
train_arr[50:120] <- FALSE
train_arr

#lgs
lgs.fit <- glm(formula_1, family = binomial, data = Auto_1, subset = train_arr)
lgs.prob <- predict(lgs.fit, newdata = Auto_1[!train_arr,], type = 'response')
lgs.pred <- rep(0, length(lgs.prob))
lgs.pred[lgs.prob>0.5] <- 1
lgs.table <- table(lgs.pred, Auto_1[!train_arr,]$mpg01)

#lda
lda.fit <- lda(formula_1, data = Auto_1, subset = train_arr)
lda.pred <- predict(lda.fit, newdata = Auto_1[!train_arr,])
lda.table <- table(lda.pred$class, Auto_1[!train_arr,]$mpg01)

#qda
qda.fit <- qda(formula_1, data = Auto_1, subset = train_arr)
qda.pred <- predict(qda.fit, Auto_1[!train_arr,])
qda.pred$class
qda.table <- table(qda.pred$class, Auto_1[!train_arr,]$mpg01)

#knn
knn.fit <- knn(train = as.matrix(Auto_1[train_arr,]$cylinders), test = as.matrix(Auto_1[!train_arr,]$cylinders), cl = Auto_1[train_arr,]$mpg01, k = 1)
knn.fit
knn.table <- table(knn.fit, Auto_1[!train_arr,]$mpg01)
knn.table

#Naive Bayes
nb.fit <- naiveBayes(formula_1, data = Auto_1, subset = train_arr)
nb.pred <- predict(nb.fit, newdata = Auto_1[!train_arr,])
nb.pred
nb.table <- table(nb.pred, Auto_1[!train_arr,]$mpg01)


#Exercise 15
raise <- function(a,b){
  return(a^b)
}

raise(1,3)
x <- 1:10
y <- raise(x,2)

PlotPower <- function(range, p){
  return(plot(range, raise(range, p)))
}
PlotPower(1:10, 50)


#EXERCISE 16
View(Boston_1)
?Boston
Boston_1 <- scale(Boston)
cor(Boston)
formula_1 <- mpg01 ~ cylinders + weight + acceleration
formula_2 <- mpg01 ~ cylinders*displacement + weight + year*origin + horsepower*acceleration
mpg01 <- rep(0, nrow(Auto))
Auto_1 <- Auto
Auto_1$mpg01 <- mpg01
Auto_1$mpg01[Auto_1$mpg > median(Auto_1$mpg)] <- 1
ncol(Auto_1)
plot(Auto_1[c(10,10)])
plot(Auto_1$mpg)
boxplot(Auto_1[c(5,6,7,8,10)])
cor(Auto_1[-9])

train_arr <- rep(TRUE, nrow(Auto_1))
train_arr[50:120] <- FALSE
train_arr

#lgs
lgs.fit <- glm(formula_1, family = binomial, data = Auto_1, subset = train_arr)
lgs.prob <- predict(lgs.fit, newdata = Auto_1[!train_arr,], type = 'response')
lgs.pred <- rep(0, length(lgs.prob))
lgs.pred[lgs.prob>0.5] <- 1
lgs.table <- table(lgs.pred, Auto_1[!train_arr,]$mpg01)

#lda
lda.fit <- lda(formula_1, data = Auto_1, subset = train_arr)
lda.pred <- predict(lda.fit, newdata = Auto_1[!train_arr,])
lda.table <- table(lda.pred$class, Auto_1[!train_arr,]$mpg01)

#qda
qda.fit <- qda(formula_1, data = Auto_1, subset = train_arr)
qda.pred <- predict(qda.fit, Auto_1[!train_arr,])
qda.pred$class
qda.table <- table(qda.pred$class, Auto_1[!train_arr,]$mpg01)

#knn
knn.fit <- knn(train = as.matrix(Auto_1[train_arr,]$cylinders), test = as.matrix(Auto_1[!train_arr,]$cylinders), cl = Auto_1[train_arr,]$mpg01, k = 1)
knn.fit
knn.table <- table(knn.fit, Auto_1[!train_arr,]$mpg01)
knn.table

#Naive Bayes
nb.fit <- naiveBayes(formula_1, data = Auto_1, subset = train_arr)
nb.pred <- predict(nb.fit, newdata = Auto_1[!train_arr,])
nb.pred
nb.table <- table(nb.pred, Auto_1[!train_arr,]$mpg01)