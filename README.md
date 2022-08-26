The repository contains selected programming exercises from Python and R.

## Python
This folder contains an implementation of several data structures:
- Singly linked list
- Doubly linked list (2 ways)
- Queue (FIFO)
- Stack (LIFO)
- Hash Table
- Binary Search Tree
- Trie

Moreover, it contains a short expected value analysis from playing a lottery called [Keno](https://www.lotto.pl/keno).

Conclusions:
1. The player statistically losses the least amount of money when choosing 5 numbers. The loss amounts to 0.42 PLN for every 1 PLN invested into the game.

2. The loss when choosing the different amount of numbers is similar except for choosing only one number. In that case, the loss amounts to 0.57 PLN for every 1 PLN invested into the game

## ISLR (R)
This folder contains selected solutions to the exercises from the book [An Introduction to Statistical Learning](https://www.statlearning.com/). The solutions come from chapters on Linear Regression, Classification and Resampling Methods.

## Titanic (R)
This folder contains Multinomial Logistic Regression, Linear Discriminant Analysis and Naive Bayes models implementation in R aimed to predict the survivorship status of selected Titanic passengers. Out of 13 models tested, only the best performing 3 are shown. 

Missing values for the "Age" independent variable were predicted by fitting a Multiple Linear Regression model.

Accuracy of the models (evaluated by [Kaggle](https://www.kaggle.com/competitions/titanic/overview) submissions): 
- Null model (all predictions as "didn't survive") - 0.62200
- Naive Bayes - 0.74880
- Multinomial Logistic Regression - 0.77751
- Linear Discriminant Analysis - 0.77990

