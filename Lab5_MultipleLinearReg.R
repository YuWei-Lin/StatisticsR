#Multiple Regression
library(ISwR)
data(cystfibr)

?cystfibr

par(mex=0.5)
pairs(cystfibr, gap=0, cex.labels=0.9)
attach(cystfibr)

View(cystfibr)
#Model specification and output
options(show.signif.stars=TRUE)

lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc)

summary(lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc))

#The 25.5 comes from "residual standard error" in the summary output.
#Note further that there is quite a large difference between the unadjusted
#and the adjusted R2, which is due to the large number of variables relative
#to the number of degrees of freedom for the variance.

#residual variance
1-25.5^2/var(pemax)

#The ANOVA table for a multiple regression analysis is obtained 
#using anova and gives a rather different picture:
anova(lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc))

#the effect of age is significant because these tests are successive.
#stepwise removal of terms from model until finally only age is left.

#The ANOVA table indicates that there is no significant improvements of
#the model once the age is included. It is possible to perform a joint test for
#whether all the other variables can be removed by adding up the sums of
#squares contributions and use the sum for an F test, that is:

######1
955.4+155.0+632.3+2862.2+1549.1+561.9+194.6+92.4

7002.9/8

875.36/648.7

1-pf(1.349407,8,15)

######2
m1<-lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc)
m2<-lm(pemax~age)
anova(m1)
anova(m2)
anova(m1,m2)


#model search
#R has the step() function for performing model searches by Akaike Information
#Criterion.
#Use simple manual variants of backward elimination
summary(lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc))

#remove tlc
summary(lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc))

#remove frc
summary(lm(pemax~age+sex+height+weight+bmp+fev1+rv))

summary(lm(pemax~age+sex+height+weight+bmp+fev1))

summary(lm(pemax~age+sex+height+weight+bmp))

summary(lm(pemax~age+height+weight+bmp))

summary(lm(pemax~height+weight+bmp))

summary(lm(pemax~weight+bmp))

summary(lm(pemax~weight))

#good idea to pay attention to "age", "weight", and "height", which are heavily
#correlated since we are dealing with children and adolescents
summary(lm(pemax~age+height+weight))

summary(lm(pemax~age+height))

summary(lm(pemax~age))

summary(lm(pemax~height))




