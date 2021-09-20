library(tidyverse)
library(dplyr)
library(class)
library(ggplot2)
#We will be using the IRIS dataset to classify the data with KNN. 
#Setting the dataset----
df = iris
#Let's look at the variables:
glimpse(iris)
#All 4 variables can be used to predict the classification of the Species.
#Visualizing Pethal and Sepal features----
df %>% ggplot(aes(Sepal.Length,Sepal.Width,col=Species))+
  geom_point()

df %>% ggplot(aes(Petal.Length,Petal.Width,col=Species))+
  geom_point() #KNN takes the nearest neighbor and these variables seems to work.

#Spliting dataset into training and testing----
#To make the dataset reusable let's set the seed 1:
set.seed(1)
train_index = sample(1:nrow(df),0.7*nrow(df)) #Select either seed and data.
df_train = df[train_index,] 
df_test= df[-train_index,]
#KNN----
#There are two KNN in Class and FNN packages.
predict1= class::knn(train=dplyr::select(df_train,-Species),
                    test=dplyr::select(df_test,-Species),
                    cl=df_train$Species, #Response var
                    k=1, #Number of Neighbours
                    prob=T)
predict1
#Probability will be equal to 1 because 1 neighbour is taken. Major class is this one
#Confusion matrix----
table(predict1,df_test$Species)

#How well performs our prediction? To find out so let's find the accuracy (True/Total). 
accuracy = sum(diag(table(predict1,df_test$Species)))/sum(table(predict1,df_test$Species))

#4.44% of the predictions are wrong
#Increasing the numer of neighbours----
max=100
final_accuracy=c()   #The empty list with errors
for(i in 1:100){
  # KNN classification
  out = class::knn(train = dplyr::select(df_train,-Species),
                   test = dplyr::select(df_test,-Species),
                   cl = df_train$Species,
                   k = i,
                   prob=T)
  final_accuracy[i]=sum(diag(table(Predict=out,Test=df_test$Species)))/sum(table(Predict=out,Test=df_test$Species)) #Adding into the list index a rate value
}
#Final predictions----
outfinal_2 = data.frame(k=1:100,Accuracy = final_accuracy)
#Plotting----
outfinal_2 %>% 
  ggplot() +
  geom_line(aes(x=k,y=Accuracy))
#Here is the percentual of the misclassified predictions by the number of neighbours
which.max(final_accuracy) #The smallest errorrate is 2nd index
print(paste0("Your best KNN accuracy result is ", round(final_accuracy[which.max(final_accuracy)]*100,1), "%"))

