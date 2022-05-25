library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)
library(ggcorrplot)
library(stats)
library(ggfortify)
library(dplyr)
library(gmodels)
library(caret)
library(InformationValue)
library(ISLR)
library(e1071)
library(FactoMineR)
library(stats)


DS = read.csv("D:/IIT/2ND YEAR/2ND SEMESTER/Machine Learning and Data Mining/Coursework/Extra/2nd/Whitewine_v2_CSV.csv", header=TRUE)
wine.stand = DS

#Remove na values
wine.stand <- na.omit(wine.stand)
#wine.stand <- scale(wine.stand)

attach(wine.stand)

boxplot(wine.stand)

summary(fixed.acidity)
IQR_fa=7.300-6.300
upFen_fa=7.300+1.5*IQR_fa

summary(volatile.acidity)
IQR_va=0.3200-0.2100
upFen_va=0.3200+1.5*IQR_va

summary(citric.acid)
IQR_ca=0.3900-0.2700
upFen_ca=0.3900+1.5*IQR_ca

summary(residual.sugar)
IQR_rs=10.000-1.700
upFen_rs=10.000+1.5*IQR_rs

summary(chlorides)
IQR_c=0.05000-0.03600
upFen_c=0.05000+1.5*IQR_c

summary(free.sulfur.dioxide)
IQR_fsd=46.00-24.00
upFen_fsd=46.00+1.5*IQR_fsd

summary(total.sulfur.dioxide)
IQR_tsd=167.0-109.0
upFen_tsd=167.0+1.5*IQR_tsd

summary(density)
IQR_d=0.9961-0.9917
upFen_d=0.9961+1.5*IQR_d

summary(pH)
IQR_ph=3.280-3.090
upFen_ph=3.280+1.5*IQR_ph

summary(sulphates)
IQR_s=0.5500-0.4100
upFen_s=0.5500+1.5*IQR_s

summary(alcohol)
IQR_a=11.40-9.50
upFen_a=11.40+1.5*IQR_a

#summary(quality)
#IQR_q=6.000-5.000
#upFen_q=6.000+1.5*IQR_q

wine.stand_clean=subset(wine.stand, fixed.acidity<=8.8 & volatile.acidity<=0.485 & citric.acid<=0.57 & residual.sugar<=22.45 & chlorides<=0.071 & free.sulfur.dioxide<=79 & total.sulfur.dioxide<=254 & density<=1.0027 & pH<= 3.565 & sulphates<=0.76 & alcohol<=14.25) 
wine.data.points = wine.stand_clean[-12]

boxplot(wine.stand_clean)


check_scale = scale(wine.data.points)
boxplot(check_scale)


#Elbow method
set.seed(123)
fviz_nbclust(check_scale, kmeans, method = "wss")


# compute gap statistic
set.seed(123)
gap_stat <- clusGap(check_scale, FUN = kmeans, nstart = 25,K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)


set.seed(123)

#K=2
kmeans1 <- kmeans(check_scale, 2, nstart = 25)
fviz_cluster(kmeans1, data = check_scale)
autoplot(kmeans1,check_scale,frame=TRUE)

#K=3
kmeans2 <- kmeans(check_scale, 3, nstart = 25)
fviz_cluster(kmeans2, data = check_scale)
autoplot(kmeans2,check_scale,frame=TRUE)

#K=4
kmeans3 <- kmeans(check_scale, 4, nstart = 25)
fviz_cluster(kmeans3, data = check_scale)
autoplot(kmeans3,check_scale,frame=TRUE)



column<-factor(wine.stand_clean$quality)
as.numeric(column)
wines_numeric<-as.numeric(column)



###Confusion Matrix###
conf_matrix1<-caret::confusionMatrix(data = as.factor(kmeans1$cluster),reference = as.factor(wines_numeric))
print(conf_matrix1)


conf_matrix2<-caret::confusionMatrix(data = as.factor(kmeans2$cluster),reference = as.factor(wines_numeric))
print(conf_matrix2)


conf_matrix3<-caret::confusionMatrix(data = as.factor(kmeans3$cluster),reference = as.factor(wines_numeric))
print(conf_matrix3)



### PCA ###
pc_results <- prcomp(check_scale, scale = TRUE)

#reverse the signs
pc_results$rotation <- -1*pc_results$rotation

#display principal components
pc_results$rotation
summary(pc_results)


results_new=as.data.frame(-pc_results$x[,1:9])
dim(results_new)

#plot(results_new)

kmeans_pc <- kmeans(results_new, 2, nstart = 25)
autoplot(kmeans_pc,results_new,frame=TRUE)


conf_matrix_pca<-caret::confusionMatrix(data = as.factor(kmeans_pc$cluster),reference = as.factor(wines_numeric))
print(conf_matrix_pca)

