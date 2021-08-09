#Lab3
#1.Create data 

##1.1
# enter data and save in a vector called "dta1"
dta1 <- c(79725,12862,18022,76712,256440,14013,46083,6808,85781,1251, 
          6081,50397,11020,13633,1064,496433,25308,6616,11210,13900)
dta1

length(dta1)
dim(dta1)
dim(as.matrix(dta1))
as.matrix(dta1)

#Force R not to use exponential notation
#options("scipen"=999,"digits"=2)
options("scipen"=999)

# compute some descriptive statistics
mean(dta1)
median(dta1)
sd(dta1)
#summary(dta1, digits=6)
summary(dta1)

par(mfrow=c(1,2))

hist(dta1,freq = T, col='light blue') # make a histogram
hist(dta1,freq = F, col='light blue') # make a histogram

# add a curve to the plot to show a normal distribution 
# with the same mean and sd as the data
lines(x=seq(0,500000,1000),y=dnorm(seq(0,500000,1000),mean(dta1),sd(dta1)), 
      col='red')

# set option for penalty for scientific notation to allow more digits in dta1 
par(mfrow=c(1,1))
options(scipen=1) 
hist(dta1, freq = F, col='light blue')
lines(seq(0,500000,1000),dnorm(seq(0,500000,1000), mean(dta1),sd(dta1)), col= 'blue')

##1.2
# transform and compute some descriptive statistics
logdta1 <- log10(dta1) 
mean(logdta1 )
median(logdta1 )
sd(logdta1 )
summary(logdta1 )
hist(logdta1 ,freq = F, col='light blue')  #make histogram
lines(log(seq(1,500000,1000)),
      dnorm(log(seq(1,500000,1000)),mean(logdta1),sd(logdta1)), 
      col= 'red')

par(mfrow=c(1,2))
hist(dta1, freq = F, col='light blue')
lines(seq(0,500000,1000),dnorm(seq(0,500000,1000), mean(dta1),sd(dta1)), col= 'blue')

hist(logdta1 ,freq = F, col='light blue')  #make histogram
lines(log(seq(1,500000,1000)),dnorm(log(seq(1,500000,1000)),mean(logdta1),sd(logdta1)), col= 'red')

##1.3
d <- data.frame(V1=c(23, 45, 56), V2=c(45, 45, 67))

## enter id here, you could also use 1:nrow(d) instead of rownames
id <- rownames(d)
d <- cbind(id=id, d)

## set colnames to OP's wishes
colnames(d) <- paste0("V", 1:ncol(d))


#2. From slide 6

##Create data frame from table

lab3.1 <- data.frame(CellLine=c("parental","parental","parental","parental","LR","LR","LR","LR","LTR","LTR","LTR","LTR"),
                     none=c(99.36,98.64,101.00,100.45,97.66,99.45,101.29,98.89,97,100.35,102.95,97.82),
                     lowdose=c(82.59,86.40,87.10,85.65,52.04,67.56,70.02,66.49,70.73,75.54,79.92,77.07),
                     highdose=c(41.37,51.68,57.88,63.18,30.45,34.47,41.92,44.48,38.55,47.94,56.07,54.13))  

subject = as.numeric(rownames(lab3.1))
lab3.1=cbind(subject=subject,lab3.1)

length(lab3.1)
lab3.1$id=seq(1:length(lab3.1))##
lab3.1$id=seq(1:nrow(lab3.1))

#Convert data between wide and long format
#install.packages("tidyr")
library(tidyr)
#gather() and spread() from the tidyr package. This is a newer interface to the reshape2 package.
#melt() and dcast() from the reshape2 package.
#see more details from http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

lab3.1_long <- gather(lab3.1, dose,measurement,none,lowdose,highdose)
lab3.1_long <- lab3.1_long[order(lab3.1_long$CellLine,lab3.1_long$dose),]
lab3.1_long


#From long to wide format
lab3.1_long <- lab3.1_long[order(lab3.1_long$subject),]
lab3.1_wide <- spread(lab3.1_long,dose,measurement)

#3. CLT

curve(dnorm(x), -4, 4, col = "red")
curve(dt(x, df = 1), add = TRUE)

curve(dnorm(x), -4, 4, col = "red")
curve(dt(x, df = 26), add = TRUE)


curve(dnorm(x), -4, 4, col = "black")
curve(dt(x, df = 1), add = TRUE, col="red")
curve(dt(x, df = 2), add = TRUE, col="blue")
curve(dt(x, df = 10), add = TRUE, col="green")
legend("topright", legend=c("N(0,1)", "t(1)","t(2)","t(10)"),
       col=c("black","red","blue","green"), lty=1, cex=0.8)

curve(dnorm(x), -4, 4, col = "black")
curve(dt(x, df = 1), add = TRUE, col="red")
curve(dt(x, df = 2), add = TRUE, col="blue")
curve(dt(x, df = 10), add = TRUE, col="green")
legend("topright", legend=c("N(0,1)", "t(1)","t(2)","t(10)"),
       col=c("black","red","blue","green"), lty=1, cex=0.8,box.lty = 0)

#4. test difference between two groups 
#Slide page 30
data1 <- data.frame(id=1:28, 
                    group=c("active","active","active","active","active","active","active","active","active","active","active","active","active","active", 
                            "control","control","control","control","control","control","control","control","control","control","control","control","control","control"),
                    CV=c(13.85,1.62,21.31,1.11,5.69,6.20,18.50,1.71,0.51,23.22,2.37,1.61,10.43,3.33,
                         40.91,14.61,29.91,37.68,18.73,27.02,16.43,86.53,1.41,0.56,11.41,0.60,12.92,33.55))

boxplot(CV~group,data=data1,xlab="Treatment Group",ylab="Contusion Volume (mm3)")
title("Contusion Volume by Treatment Group")

#############################################################################################
#slide page 37
curve(dnorm(x), -4, 4, col = "red")
curve(dt(x, df = 1), add = TRUE)

curve(dnorm(x), -4, 4, col = "red")
curve(dt(x, df = 26), add = TRUE)

#slide 38
curve(dnorm(x), -4, 4, col = "black")
curve(dt(x, df = 1), add = TRUE, col="red")
curve(dt(x, df = 2), add = TRUE, col="blue")
curve(dt(x, df = 10), add = TRUE, col="green")
legend("topright", legend=c("N(0,1)", "t(1)","t(2)","t(10)"),
       col=c("black","red","blue","green"), lty=1, cex=0.8)

curve(dnorm(x), -4, 4, col = "black")
curve(dt(x, df = 1), add = TRUE, col="red")
curve(dt(x, df = 2), add = TRUE, col="blue")
curve(dt(x, df = 10), add = TRUE, col="green")
legend("topright", legend=c("N(0,1)", "t(1)","t(2)","t(10)"),
       col=c("black","red","blue","green"), lty=1, cex=0.8,box.lty = 0)

#############################################################################################
#back to "TBI and Contusion Volume"
tapply(data1$CV, data1$group, summary)
tapply(data1$CV, data1$group,function(x) format(summary(x), scientific = TRUE))

library(dplyr)

data1 %>% 
  group_by(group) %>% 
  summarize(mean = mean(CV),
            sum = sum(CV),
            sd= sd(CV),
            median=median(CV),
            q1 = quantile(CV, 0.25),
            q3 = quantile(CV,0.75),
            IQR=q3-q1,
            count = n())

qt(c(0.025),df=26,lower.tail = T);#alpha=0.05


#t-test: two-sided and equal variance

t.test(CV ~ group, data1, var.equal=TRUE)

###summary(aov(CV ~ group, data1));#ANOVA
#using long format data
##1
data1 <- data1[order(data1$id),]
data1_wide <- data.frame(
  ID=1:14,
  active=data1$CV[1:14],
  control=data1$CV[15:28]
)

t.test(data1_wide$active, data1_wide$control, alternative = "two.sided", var.equal = T)

##2
data2 <- data.frame(active=c(13.85,1.62,21.31,1.11,5.69,6.20,18.50,1.71,0.51,23.22,2.37,1.61,10.43,3.33),
                    control=c(40.91,14.61,29.91,37.68,18.73,27.02,16.43,86.53,1.41,0.56,11.41,0.60,12.92,33.55))

t.test(data2$active, data2$control, alternative = "two.sided", var.equal = T)

pt(c(-2.4693),df=26,lower.tail = T)+pt(c(2.4693),df=26,lower.tail = F)

###########################################################################################################################
library(car)
leveneTest(CV~group,data1,center=mean)

#Normality check
# Shapiro-Wilk normality test for active
with(data1, shapiro.test(CV[group == "active"]))
# Shapiro-Wilk normality test for control
with(data1, shapiro.test(CV[group == "control"])) 

#From the output, the two p-values are smaller than the significance level 0.05 
#implying that the distribution of the data are significantly different from the normal distribution. 
#In other words, we cannot assume the normality.

par(mfrow=c(1,2))
qqPlot(data1$CV[data1$group == "active"],ylab="active")
qqPlot(data1$CV[data1$group == "control"],ylab="control")

#F-test to test for homogeneity in variances
var.test(CV ~ group, data = data1)
#The p-value of F-test is p-value = 0.0007429. 
#It's smaller than the significance level alpha = 0.05. 
#In conclusion, there is significant difference between the variances of the two sets of data. 
#Therefore, we cannot use the classic t-test witch assume equality of the two variances.





##0
t.test(CV ~ group, data1, var.equal=F)

##1
t.test(data1_wide$active, data1_wide$control, alternative = "two.sided", var.equal = F)

##2
t.test(data2$active, data2$control, alternative = "two.sided", var.equal = FALSE)

#Use the natural logarithm to transform the CV measure
data1$lnCV=log(data1$CV)

library(dplyr)

data1 %>% 
  group_by(group) %>% 
  summarize(mean = mean(lnCV),
            sd= sd(lnCV),
            count = n())

library(car)
leveneTest(lnCV~group,data1,center=mean)

par(mfrow=c(1,2))
qqPlot(data1$lnCV[data1$group == "active"],ylab="active")
qqPlot(data1$lnCV[data1$group == "control"],ylab="control")

t.test(lnCV ~ group, data1, var.equal=T)

#Normality check
# Shapiro-Wilk normality test for active
with(data1, shapiro.test(lnCV[group == "active"]))
# Shapiro-Wilk normality test for control
with(data1, shapiro.test(lnCV[group == "control"])) 
