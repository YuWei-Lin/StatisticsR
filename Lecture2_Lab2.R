getwd()
setwd("C:/your path"); # this is for window system

#Practice-1
#simple arithmetic calculation
4+3+4+3
mean(c(4,3,4,3))
(4+3+4+3)/4

#generate data and summarize it
mygpa <- c(4,3,4,3); #create data
mean(mygpa);
boxplot(mygpa);

#Practice-2
help(ToothGrowth, package="datasets")
#Read the help on ToothGrowth on the other pane
coplot(len ~ dose | supp, data = ToothGrowth, 
       panel = panel.smooth,
       xlab = "ToothGrowth Data: length vs dose, by supplement")

#rename the label of y-axis from len to Length.
coplot(len ~ dose | supp, data = ToothGrowth, 
       panel = panel.smooth,
       ylab = "Length", 
       xlab = "ToothGrowth Data: length vs dose, by supplement")

#Practice-3
#import data into R-studio
#Go to file tab -> clink "Import Dataset: -> select "From Text(readr)" ->click "Delimiter"
# -> apply "tab" -> click "Import"

library(readr)
lab1_1 <- read_delim("C:/YOUR WORKING DIR PATH/lab2.1.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

lab1_1$fGender <- factor(lab1_1$Gender, labels=c('1X','2X'))

boxplot(lab1_1$Folate~lab1_1$Group, data=lab1_1, id.method="y")

#apply different colour
boxplot(lab1_1$Folate~lab1_1$Group, data=lab1_1, id.method="y", col='pink')

#Practice-4
#Low Birth Weight Infant Data 
#download data from Canvas
data=read.table(file="./Data/lowbwt1.txt",header=T)
summary(data)


# 2-Way Frequency Table
attach(data)
mytable <- table(data$tox,data$grmhem) # A will be rows, B will be columns
mytable # print table

margin.table(mytable, 1) # A frequencies (summed over B)
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages

mytable1 <- table(data$sex,data$grmhem) # A will be rows, B will be columns
mytable1 # print table

margin.table(mytable1, 1) # A frequencies (summed over B)
margin.table(mytable1, 2) # B frequencies (summed over A)

prop.table(mytable1) # cell percentages
prop.table(mytable1, 1) # row percentages
prop.table(mytable1, 2) # column percentages

# Boxplot of Apgar score by germinal matrix hemorrhage 
boxplot(data$apgar5~data$grmhem,data=data, main="Boxplot apgar5 vs grmhem",
        xlab="germinal matrix hemorrhage (0=No, 1=Yes)", ylab="Apgar score")

# Simple Scatterplot
plot(data$gestage, data$sbp, main="Scatterplot of sbp vs gestage",
     xlab="gestational age (weeks) ", ylab="systolic blood pressure ", pch=19, col="red")

#Histogram
hist(data$apgar5,main="Histogram of apgar5",
     xlab="Apgar score at 5 minutes ", ylab="Frequency")

#Practice-5
#9 health insurace claims
claim <- c(1100,1900,600,890,690,890000,380,1200,1050)

summary(claim); sum(claim)
sd(claim);var(claim)

#Practice-6
#datasets in "An Introduction to Stat Learning with application in R" 
install.packages("ISLR")
library(ISLR)
help(ISLR)
??ISLR
data()

#data - NCI60
help("NCI60")
table(NCI60$labs)
View(NCI60$data)

dta=NCI60$data
dim(dta)
write.table(dta,"NCI60.txt",col.names = F,row.names = F)

#data - Khan
help("Khan")
summary(Khan)
table(Khan$ytrain)
table(Khan$ytest)
View(Khan)

#generate training dataset
xtrain=Khan$xtrain
dim(xtrain)
ytrain=Khan$ytrain
train=data.frame(cbind(ytrain,xtrain))
dim(train)

#generate testing dataset
xtest=Khan$xtest
dim(xtest)
ytest=Khan$ytest
test=data.frame(cbind(ytest,xtest))
dim(test)

write.table(train,"Khan-training.txt",col.names = F,row.names = F)
write.table(test,"khan-testing.txt",col.names = F,row.names = F)



