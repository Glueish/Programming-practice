install.packages("e1071")
library('e1071')
library('MASS')

# Read data
Train <- read.csv('train.csv')
Test <- read.csv('test.csv')
View(Train)
View(Test)

# Factorize
Train$Pclass <- as.factor(Train$Pclass)
Train$Embarked <- as.factor(Train$Embarked)
Train$Sex <- as.factor(Train$Sex)
Train$Survived <- as.factor(Train$Survived)

Test$Pclass <- as.factor(Test$Pclass)
Test$Embarked <- as.factor(Test$Embarked)
Test$Sex <- as.factor(Test$Sex)

# Prepare age and test dataset for 'Age' prediction
Train_age <- rbind(Train[!is.na(Train$Age),][-2], Test[!is.na(Test$Age),])
Test_age <- rbind(Train[is.na(Train$Age),][-2], Test[is.na(Test$Age),])
Test[153,]$Fare <- mean(Train$Fare)

# Multiple linear regression for 'Age' prediction
lm.fit_age <- lm(formula = Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = Train_age)
summary(lm.fit_age)

lm.pred_train <- predict(object = lm.fit_age, newdata = Train[is.na(Train$Age),])
Train[is.na(Train$Age),]$Age <- lm.pred_train

lm.pred_test <- predict(object = lm.fit_age, newdata = Test[is.na(Test$Age),])
Test[is.na(Test$Age),]$Age <- lm.pred_test


# Null model for 'Survived' prediction
Null_model <- data.frame(PassengerId = Test$PassengerId, Survived = rep(x = 0, times = 418))
# 0.62200 Accuracy


# Multinomial logistic regression for 'Survived' prediction
glm.fits <- glm(formula = Survived ~ Pclass:Fare + Sex:Age + SibSp:Parch + Embarked, 
                family = binomial, 
                data = Train)
summary(glm.fits)

glm.raw_pred <- predict(object = glm.fits, newdata = Test, type = 'response')
glm.pred <- rep(x = 0, times = 418)
glm.pred[glm.raw_pred > 0.5] <- 1

Results_lr <- data.frame(PassengerId = Test$PassengerId, Survived = glm.pred)
# write.csv(Results_lr,"Pred_lr.csv", row.names = FALSE)
# 0.77751 Accuracy
 

# Linear discriminant analysis for 'Survived' prediction
lda.fit <- lda(formula = Survived ~ Pclass:Fare + Sex:Age + SibSp:Parch + Embarked, 
               data = Train)
lda.raw_pred <- predict(object = lda.fit, newdata = Test)
lda.pred <- lda.raw_pred$class
Results_lda <- data.frame(PassengerId = Test$PassengerId, Survived = lda.pred)
# write.csv(Results_lda,"Pred_lda.csv", row.names = FALSE)
# 0.77990 Accuracy


# Naive bayes for 'Survived' prediction
nb.fit <- naiveBayes(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                     data = Train)
nb.pred <- predict(nb.fit, Test)
Results_nb <- data.frame(PassengerId = Test$PassengerId, Survived = nb.pred)
# write.csv(Results_nb, "Pred_nb.csv", row.names = FALSE)
# 0.74880 Accuracy