#Lab4

##############################Assigning Values############################
#In R, <- is usually used to assign a value to a variable, not the = symbol 
#e.g. x_scalar <- 1. The shortcut for an assignment is Alt and -. 
#Most other mathematical operators function as expected.

x_scalar1 <- 1
x_scalar2 = 2
x_scalar1;x_scalar2

#The combine function, c() is used to assign multiple same type values to a variable. 
x_vec <- c(1, 2, 3)
x_vec

#The list function, list() is used to assign multiple different types of values to a variable.
x_list <-  list(1, "two", 3.0)
x_list
#Note that the first value in an R list is at position 1, not 0, 
#and R assumes that 1 and 3.0 are both numeric data types, not an integer and a float.

##############################Matrices and Data Frames##############################
#A matrix consists of rows and columns of the same data type. 
x_matrix1 <- matrix(c(1, 2, 3, 4),nrow=2, ncol=2)
x_matrix1
#By default, a matrix fills by column. The dim() function returns the dimensions of a matrix.
dim(x_matrix1)

x_matrix2 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),nrow=3, ncol=4,byrow = T)
x_matrix2

x_matrix3 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),nrow=3)
x_matrix3 

#A data frame has rows and columns, but can store different data types in each column, 
#and each column must have a name, preferably unique. 
x_dataframe <- data.frame(a = 1:5, b = 6:10)
x_dataframe
dim(x_dataframe)
#Note that the colon operator is used to generate a range of numbers 
#and the equal sign is properly used here as an equality operator.

##############################Operations and Loops##############################
#Operations, including loops, are generally applied in R with the apply() function, 
#although if and if...else statements are also supported. 
#The third-party plyr package lets you easily apply operations to part of a data set.

##############################viewing data##############################
#attach() attaches a list or data frame to the R search path for easy searching.
#edit() displays an editable version of an object.
#print() displays the values of an object.

##############################graphing data##############################
#barplot() creates a bar plot.
#boxplot() creates a box plot.
#heatmap() creates a heat map.
#hist() creates a histogram.
#plot() creates a scatter plot.

##############################statistics##############################
#mean() calculates the arithmetic mean.
#sd() calculates the standard deviation.
#summary() provides summary statistics if used with a data frame.
#table() cross-tabluation and table creation.
#t.test() one and two sample t-tests on vectors of data (Student's t-Test).
#var, cov and cor compute the variance of x and the covariance or correlation of x and y.

##############################Input##############################
#test_data <- read.csv("filename.txt", header=FALSE) will read the data in the named CSV file into a data frame. 
#Note that a header is normally expected, and in some cases you may need to add stringsAsFactor=FALSE 
#to prevent strings from being interpreted as statistical values by R. 
#You can also provide a URL instead of a file name (assuming the data at the URL is in standard CSV format).
#read.csv() is identical to read2.csv() and read.table() except with a different set of defaults.
#In RStudio, you can simply use Environment tab > Import Dataset.
#Many third party packages exist for reading data in particular file formats, databases or online data sources.

##############################Output##############################
#save.image() will save the entire workspace to a file named .RData
#To save data in a compact binary file, use save(data, file="filename.rda") and to load the object, 
#use load("filename.rda").
#To save data in text format, use dump(data, "filename.Rdmpd") and to load it, use source("filename.Rdmpd").
#write.csv(test_data, "test.csv") will save data to a CSV text file. To omit headers, add the argument row.names=F. 
#Use write.table(test_data, "test.txt") to save data to a text file.

##############################Statistics Examples with R##############################

#Uniform distribution, also try 10000
x <- runif(10) 
summary(x)

y <-sample(1:100, 10, replace=T)
summary(y)
print(y)

y <- sample(1:100, 100, replace=T)
summary(y)
mean(y)
sd(y)
unique(y)
length(unique(y))

y <- sample(1:100, 10000, replace=T)
summary(y)
sd(y)

#Normal distribution, also try 10000
z <- rnorm(10) 
summary(z)
sd(z) #Expected values is 1

##############################Probability with R##############################

#Coin Tosses - Binomial distribution
#rbinom(#obs, #trials, prob)
rbinom(10,1,0.5) 
rbinom(10,100,0.5)
rbinom(10,10000,0.5)
rbinom(10,1000000,0.5)

#Random DNA
x10 <- sample(c("A","C","G","T"),10,replace=T)
x10
x100 <- sample(c("A","C","G","T"),100,replace=T)
x100
write(x100, file="100test.txt") #To avoid one character per line
y = paste(x100, collapse="") #use an empty collapse argument
y #to remove the default blank spaces
write(y, file="100randomdna.txt")

##############################Heat Maps with R##############################
#The following R source code generates a random matrix of 10 columns and 20 rows containing 200 random integers between 1 and 100, 
#then views the randomly generated data. 
#It then creates a heat map using the default cyan to purple heatmap colors 
#(note that there is no line break in the third line).
hm <- matrix(sample(1:100, 200, replace=T), ncol=10)
hm
#par(mar = rep(2, 4))
#op <- par(oma=c(5,7,1,1));par(op)
#install.packages("RColorBrewer")
library("RColorBrewer")
#display all colour schemes
display.brewer.all()
heatmap(hm,col=brewer.pal(9,"RdYlBu"))
#if you want to preserve the column order
#since the order may be informative
heatmap(hm,Colv=NA,col=brewer.pal(9,"Blues"))

##############################plotting examples with R##############################
#Random sample with replacement
a <- sample(1:100, 100, replace=T) 
plot(a)
b <- rnorm(100, mean=50, sd=9)
plot(b) #looks similar, but check the y axis!
c <- rnorm(100, mean=50, sd=3)
plot(c) #looks similar, but check the y axis!
#How to display this on a single plot? 
#The lines() function will add lines to an existing plot.
a <- sample(1:100, 10000, replace=T) #Increased n for visibility
b <- rnorm(10000, mean=50, sd=9)
c <- rnorm(10000, mean=50, sd=3)
plot(a, type="l",col="red")
lines(b, col="green")
lines(c, col="blue")
#Histogram of normal distribution?
x <- rnorm(100);#rnorm(n. of obs, mean = 0, sd = 1)
hist(x, col="lightblue", freq=T)
summary(x)
x <- rnorm(100) #can vary n
hist(x, col="lightblue", freq=F) #density instead of frequency
curve(dnorm(x, mean=0, sd=1), add=T, col="darkblue") #adds a density curve

##############################Graphing Error Bars with R##############################
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) !=
     length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

y <- rnorm(50000, mean=1)
y <- matrix(y,10000,5);#equivalent?
y.means <- apply(y,2,mean);#check the function "apply". 
y.sd <- apply(y,2,sd)
y1 <- rnorm(50000, mean=1.1)
y1 <- matrix(y1,10000,5)
y1.means <- apply(y1,2,mean)
y1.sd <- apply(y1,2,sd)
yy <- matrix(c(y.means,y1.means),2,5,byrow=TRUE);yy
ee <- matrix(c(y.sd,y1.sd),2,5,byrow=TRUE)*1.96/sqrt(10000);ee
barx <- barplot(yy, beside=TRUE,col=c("blue","magenta"), ylim=c(0,1.5),
                names.arg=1:5, axis.lty=1, xlab="Replicates", ylab="Value (arbitrary units)")

error.bar(barx,yy,ee)
#source from http://monkeysuncle.stanford.edu/?p=485

########################Example from lecture ##########################
# Example
meanflow <- c(3.17, 2.72, 2.63, 2.29, 2.12)
sdflow <- c(0.74, 0.71, 0.73, 0.70, 0.72)

sd2flow <- sdflow^2
s2within <- mean(sd2flow)
xbar <- mean(meanflow)

s2xbar1 <- (meanflow - xbar)^2
s2xbar <- sum(s2xbar1)/(5-1)
s2between <- 200*s2xbar

FF <- s2between/s2within
FF
#[1] 64.18434

pvalue <- pf(FF,4,995,lower.tail=F)
pvalue
#[1] 2.613141e-48

#########################################################################################
# Since we don't have individual-level data,
# Generate data that approximate the summary statistics 
# This is to illustrate the analysis process.
mf<-c(3.17,2.72,2.63,2.29,2.12) # sample mean flows
sdf<-c(0.74,0.71,0.73,0.7,0.72) # sample standard deviations

# create and concatenate 5 samples into one long vector
flow<-c(rnorm(200,mf[1],sdf[1]),
        rnorm(200,mf[2],sdf[2]),
        rnorm(200,mf[3],sdf[3]),
        rnorm(200,mf[4],sdf[4]),
        rnorm(200,mf[5],sdf[5]))

# create and concatenate together grouping variable values 
# for each exposure group
group<-factor(c(rep('1 cln/nonsmkr',200),
                rep('2 smky/nonsmkr',200),
                rep('3 lite smkr',200),
                rep('4 mod smkr',200),
                rep('5 hvy smkr',200)))

# This is where you would begin if you had the REAL data.
# compute means, standard deviations and sample size by group,
# 'bind' together in columns, and print
cbind(mean=tapply(flow,group,mean), sd=tapply(flow,group,sd),
      n=tapply(flow,group,length))
#                   mean        sd   n
#1 cln/nonsmkr  3.189200 0.6688532 200
#2 smky/nonsmkr 2.755187 0.7151070 200
#3 lite smkr    2.584007 0.7698609 200
#4 mod smkr     2.253726 0.7191788 200
#5 hvy smkr     2.153189 0.7297743 200

# run analysis of variance with flow as the outcome and 
# group as the grouping variable
# store the result in an object, here called "result" 
result<-lm(flow~group)

anova(result) #print anova table from object result


#when individual-level data is available
#Bartlett test of homogeneity of variances 
#bartlett.test(flow, group)

#Shapiro-Wilk normality test
#shapiro.test(flow[group=='5 hvy smkr'])

############################################################################

# define coordinate vectors for vertices of the polygon
x <- c(2, seq(2, 10, 0.01), 10)
y <- c(0, df(seq(2, 10, 0.01), 3, 14), 0)

# draw density of F_{3, 14}
curve(df(x ,3 ,14), 
      ylim = c(0, 0.8), 
      xlim = c(0, 10), 
      ylab = "Density",
      main = "Density Function")

# draw the polygon
polygon(x, y, col = "orange")


###Create the same plot from lecture note
#############################################################################
# draw density of F_{2, 30}
curve(df(x ,2 ,30), 
      ylim = c(0, 1.2), 
      xlim = c(0, 5),
      ylab = "Density",
      main = "Density Function")


curve(df(x, 4,30), add = TRUE, col="red")
curve(df(x, 16,30), add = TRUE, col="blue")
curve(df(x, 8,30), add = TRUE, col="green")
###########################################################################
curve(df(x ,2 ,30), 
      ylim = c(0, 1.2), 
      xlim = c(0, 5),
      xaxs="i",yaxs="i",
      ylab = "Density",
      main = "Density Function")
abline(v = qf(p=.05, df1=2, df2=30, lower.tail=F), col="black")
# define coordinate vectors for vertices of the polygon
p <- qf(p=.05, df1=2, df2=30, lower.tail=F);p
x <- c( p, seq(p, 5, 0.01), 5,5)
y <- c(0, df(c(seq(p, 5, 0.01), 5),2, 30), 0)

# draw the polygon for area under curve
polygon(x, y, col = "grey")

#add other f-dist
curve(df(x, 4,30), add = TRUE, col="red",xlim = c(0, 5),yaxs="i")
abline(v = qf(p=.05, df1=4, df2=30, lower.tail=F), col="red")
curve(df(x, 16,30), add = TRUE, col="blue",xlim = c(0, 5),yaxs="i")
abline(v = qf(p=.05, df1=16, df2=30, lower.tail=F), col="blue")
curve(df(x, 8,30), add = TRUE, col="green",xlim = c(0, 5),yaxs="i")
abline(v = qf(p=.05, df1=8, df2=30, lower.tail=F), col="green")

legend("topright", legend=c("F(2,30)", "F(4,30)","F(16,30)","F(8,30)"),
       col=c("black","red","blue","green"), lty=1, cex=0.8)

#############################################################################


