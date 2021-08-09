install.packages("ISwR")
library(ISwR)
#ventricular shortening velocity and blood glucose for type 1 diabetic patients
#load and attach data into R
data(thuesen)
attach(thuesen)

#data description
?thuesen
View(thuesen)

#for linear regression analysis, the function "lm" is used
#the argument to lm is a model formula, in which the tilde symbol(~)
# should be read by "described by".

#In fact, the lm function handles much more complicatre models than
#simple linear regression.

#Note that a multiple linear regression analysis of y on x1, x2, and x3 
#is specified as y ~ x1 + x2 + x3.

lm(short.velocity~blood.glucose)

#the desired quantities can be obtained using extractor functions
#A basic extractor function is summary:
summary(lm(short.velocity~blood.glucose))

#Since For instance, the intercept in the above analysis is hardly a meaningful quantity at all, 
#and the three-star significance of it is certainly irrelevant. 
#turn them off with options(show.signif.stars=FALSE).
options(show.signif.stars = F)
summary(lm(short.velocity~blood.glucose))

#draw the sample points and fitted line
par(mfrow=c(1,2))
plot(blood.glucose,short.velocity)
abline(lm(short.velocity~blood.glucose))
#abline draws lines based on the intercept and slope

plot(blood.glucose,short.velocity)
abline(1.098,0.022,col="red")

par(mfrow=c(1,1))

# Two further extraction functions
#store the value returned by function lm under the name lm.velo
lm.velo <- lm(short.velocity~blood.glucose)

summary(thuesen)

plot(blood.glucose,short.velocity)
abline(1.098,0.022,col="red")
abline(v=10.3,col="blue")
abline(h=1.326,col="blue")
points(blood.glucose[1],short.velocity[1], col="black",pch=15)

#The function fitted returns fitted values - 
#the y-values that you would expect for the given x-values 
#according to the best-fitting straight line: 
#in the present case 1.098+0.0220*blood.glucose.
fitted(lm.velo)

fitted(lm.velo)[1]

#The residuals shown by resid is the difference between this 
#and the observed short.velocity.
resid(lm.velo)

resid(lm.velo)[1]
short.velocity[1]-fitted(lm.velo)[1]

plot(blood.glucose,short.velocity)
lines(blood.glucose,fitted(lm.velo),col="red")
#Not working because of missing values

#the is.na function yields a vector that is TRUE wherever the argument is NA (missing).
plot(blood.glucose,short.velocity)
lines(blood.glucose[!is.na(short.velocity)],fitted(lm.velo),col="red")

#the function complete.cases can find observations 
#that are nonmissing on several variables or across an entire data frame.
cc <- complete.cases(thuesen)

options(na.action=na.exclude)
lm.velo <- lm(short.velocity~blood.glucose)
fitted(lm.velo)

#To create a plot where residuals are displayed by connecting observations
#to corresponding points on the fitted line, you can do the following. 
#Segments draws line segments with the endpoint coordinates in the order (x1, y1, x2, y2).
plot(blood.glucose,short.velocity)
lines(blood.glucose,fitted(lm.velo),col="red")
segments(blood.glucose,fitted(lm.velo),blood.glucose,short.velocity)

#A simple plot of residuals versus fitted values
plot(fitted(lm.velo),resid(lm.velo))

#whether residuals might have come from a normal distribution 
#by checking for a straight line on a Q-Q plot:
qqnorm(resid(lm.velo))

#prediction and confidence bands

#Predicted values, with or without prediction and confidence bands, 
#may be extracted with the function predict. With no arguments, 
#it just gives the fitted values:
predict(lm.velo)

#If you add interval="confidence" or interval="prediction"
#then you get the vector of predicted values augmented with limits. 
#The arguments can be abbreviated:
predict(lm.velo,int="c")

#interval="prediction"
predict(lm.velo,int="p")

summary(blood.glucose)
pred.frame <- data.frame(blood.glucose=4:20);#new data
pp <- predict(lm.velo, int="p", newdata=pred.frame)
pc <- predict(lm.velo, int="c", newdata=pred.frame)
plot(blood.glucose,short.velocity,ylim=range(short.velocity, pp, na.rm=T))
pred.gluc <- pred.frame$blood.glucose
matlines(pred.gluc, pc, lty=c(1,2,2), col="red")
matlines(pred.gluc, pp, lty=c(1,3,3), col="blue")

#Pearson correlation
cor(blood.glucose,short.velocity)

cor(blood.glucose,short.velocity,use="complete.obs")

cor(thuesen,use="complete.obs")

cor.test(blood.glucose,short.velocity)





