#Data extraction
library(readxl)
freyhacks_puno_risk <- read_excel("freyhacks - puno - risk.xlsx")
#We save the data in other variable because it will be edited
data <- as.data.frame(freyhacks_puno_risk)

class(data)
names(data)

data$Year <- NULL
data$Day <- NULL

#We are going to see if it's possible to predict the amount of sick people
#with Covid-19 based on the precipitation, the maximum temperature and minimun
#temperature and the maximum temperature.


#70% of random rows are going to be used to train the program and the rest to 
#test its accuracy.

set.seed(12345)
ran <- sample(1:nrow(data),0.9*nrow(data))
ran
length(ran)
#Creation of a function to normalize
nor <- function(x){(x-min(x))/(max(x)-min(x))}

#We select our data to be normalized
names(data)
data_norm <- as.data.frame(lapply(data[,c(1,2,3,4)], nor))

summary(data_norm)

#Training and testing data
data_train <- data_norm[ran,]

#Data de testeo
data_test <- data_norm[-ran,] 

#The column we will use as the class to train our program 
data_target_category <- data[ran, 5]

#Data to test the accuracy of our data
data_test_category <- data[-ran,5]

library(class)
library(fpc)
#Working with knn

data_train_pamk <- pamk(data_train)
pr <- knn(as.data.frame(data_train), data_test, cl=data_target_category, 
          k=data_train_pamk$nc)
pr

#Confusion matrix
tab <- table(pr, data_test_category)
tab

#Making of the accuracy function and its utilization.

accuracy <- function(x){sum(diag(x))/(sum(rowSums(x)))*100}
diag(tab)
accuracy(tab)



