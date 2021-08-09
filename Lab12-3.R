#Decision tree
library(xtable)
# library for Classification & Regression Trees
library(rpart)
#iris data set gives the measurements in centimeters of the 
#variables sepal length and width and petal length and width, 
#respectively, for 50 flowers from each of 3 species of iris
data("iris")
# Upload Iris dataset into a variable iris
iris<-iris
str(iris)

#Converting the class variable into factor
iris$Species<-as.factor(iris$Species) 
iris$Species<-as.numeric(iris$Species)  
str(iris)

#There are three level in the class variable, 1 as setosa and 2 as versicolor
#3 as Virginica
v<-iris$Species
table(v)

#set seed to ensure reproducible results
set.seed(250)
#spliting into training and test data sets in 3:1 ratio
iris[,'train'] <- ifelse(runif(nrow(iris))<0.75,1,0)
#separate training and test sets
train_iris <- iris[iris$train==1,]
test_iris <- iris[iris$train==0,]
#get column index of train flag
iris_trainColNum <- grep('train',names(train_iris))
str(test_iris)

#Obtaining the train and test data set
#remove train flag column from train and test sets
train_iris <- train_iris[,-iris_trainColNum]
test_iris <- test_iris[,-iris_trainColNum]
#Get column index of predicted variable in dataset
typeColNum_iris <- grep('Species',names(iris))
#Constructing the required Decision tree model
rpart_model_iris <- rpart(Species~.,data = train_iris, method= 'class')
# Plotting the tree
plot(rpart_model_iris)
text(rpart_model_iris)

summary(rpart_model_iris)

#Checking how good the model 
rpart_predict_iris<- predict(rpart_model_iris,test_iris[,-typeColNum_iris],type='class')
mn_iris <- mean(rpart_predict_iris==test_iris$Species)
mn_iris

# Constructing the confusion matrix to find out the efficiency of the model
table(pred=rpart_predict_iris,true=test_iris$Species)

# Applying the cost-complexity pruning
printcp(rpart_model_iris)

#Finding index of CP with lowest xerror
opt_iris <- which.min(rpart_model_iris$cptable[,'xerror'])
#Finding the values of CP# no pruning
cp_iris <- rpart_model_iris$cptable[opt_iris, 'CP' ]
cp_iris

pruned_model_iris <- prune(rpart_model_iris,cp_iris)
#plot tree
plot(pruned_model_iris)
text(pruned_model_iris) 

# Pruning is not required
summary(pruned_model_iris)

#Check the summary without pruning
summary(rpart_model_iris)


