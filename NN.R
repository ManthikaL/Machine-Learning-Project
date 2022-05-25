#Installing the packages
install.packages("fpp")
install.packages("reshape2")
install.packages("gridExtra")
install.packages("fpp2")
install.packages("e1071")
install.packages("openxlsx")
install.packages("MLmetrics")

library(fpp)
library(MASS)
library(readxl)
library(neuralnet)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(fpp2)
library(e1071)
library(openxlsx)
library(MLmetrics)

dataset_electricity <- read_excel("D:\IIT\2ND YEAR\2ND SEMESTER\Machine Learning and Data Mining\Coursework\CW\UoW_load.xlsx")
#normalize
normalize  <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
str(dataset_electricity)


date_col <- factor(dataset_electricity$Dates)
date_col <- as.numeric(date_col)
date_col

change_dataset <- data.frame(date_col, dataset_electricity$`09:00`, dataset_electricity$`10:00`, dataset_electricity$`11:00`)
change_dataset

scaled_dataset <- as.data.frame(lapply(change_dataset, normalize ))

names(change_dataset)[1] <- "Date"
names(change_dataset)[2] <- "nineth_hour"
names(change_dataset)[3] <- "tenth_hour"
names(change_dataset)[4] <- "eleventh_hour"
str(scaled_dataset)



#Training the model
set.seed(1234)
train_scaled_dataset<- scaled_dataset[1:430, ]
test_scaled_dataset <- scaled_dataset[431:500, ]
str(test_scaled_dataset)

nn <- neuralnet(eleventh_hour ~ Date + nineth_hour + tenth_hour + eleventh_hour, hidden = c(3,3), data = train_scaled_dataset)
plot(nn)

#Evaluation model performance
nn_results <- predict(nn, test_scaled_dataset[1:4])
nn_results

# extract the original  training and testing desired Output
train_original <- dataset_electricity[1:430,"11:00"] 
test_original <- dataset_electricity[431:500,"11:00"] 

# and find its maximum & minimum value
s_min1 <- min(train_original)
s_max1 <- max(train_original)

# display its contents
head(train_original)

#denormalize
denormalize <- function(x, min, max) {
  return( x*(max - min) + min )
}

renorm <- denormalize(nn_results, s_min1, s_max1)
renorm  

# RMSE function
RMSE(exp(renorm),test_original$`11:00`)
#MSE
MSE(exp(renorm),test_original$`11:00`)
# MAPE
MAPE(exp(renorm),test_original$`11:00`)

# correlation between predicted and actual values
cor(renorm,test_original$`11:00`)

#Plot for exchange_model
par(mfrow=c(1,1))
plot(test_original$`11:00`, renorm ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')
final_result1.1 <- cbind(test_original, renorm)
final_result1.1
plot(test_original$`11:00` , ylab = "Predicted vs Expected", type="l", col="red" )
par(new=TRUE)
plot(renorm, ylab = " ", yaxt="n", type="l", col="green" )
legend("topright",
       c("Predicted","Expected"),
       fill=c("red","green")
)