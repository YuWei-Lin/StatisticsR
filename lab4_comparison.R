## independent 2-group t-test
#t.test(y~x) # where y is numeric and x is a binary factor
## independent 2-group t-test
#t.test(y1,y2) # where y1 and y2 are numeric
## paired t-test
#t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric
## one sample t-test
#t.test(y,mu=3) # Ho: mu=3

#One-sample t-test

#This is an example concerning daily energy intake in kJ for 11 women
#(Altman, 1991, p. 183). First, the values are placed in a data vector:

daily.intake <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)

#summary statistics
mean(daily.intake)
sd(daily.intake)
quantile(daily.intake)

#You might wish to investigate whether the women's energy intake deviates
#systematically from a recommended value of 7725 kJ. Assuming that
#data comes from a normal distribution, the object is to test whether this
#distribution might have mean ?? = 7725. This is done with t.test, as follows:

t.test(daily.intake,mu=7725)

#Two-sample t-test

#The two-sample t test is used to test the hypothesis that two samples may
#be assumed to come from distributions with the same mean.
#install.packages("ISwR")
library(ISwR)
data(energy)
attach(energy)
View(energy)
energy

t.test(expend~stature)

t.test(expend~stature, var.equal=T)

#Comparison of Variances
var.test(expend~stature)

#Paired t-test
data(intake)
attach(intake)
intake
#The point is that the same 11 women are measured twice, so it makes sense
#to look at individual differences:
post - pre

#The paired t test is obtained as follows:
t.test(pre, post)  ## WRONG! Why?

t.test(pre, post, paired=T)

#One-way analysis of variance

#The main example is the "red cell folate" data from Altman
#(1991, p. 208). To use lm it is necessary to have the data values in
#one vector and a factor variable describing the division into groups. 
#The red.cell.folate data set contains a data frame in the proper format.

data(red.cell.folate)
attach(red.cell.folate)
summary(red.cell.folate)
anova(lm(folate~ventilation))

#pairwise comparisons and multiple testing

#If the F test shows that there is a difference between groups, the question
#quickly arises of wherein the difference lies. It becomes necessary to
#compare the individual groups.

summary(lm(folate~ventilation))
#A function called pairwise.t.test computes all possible two-group
#comparisons. It is also capable of making adjustments for multiple
#comparisons and works like this:
pairwise.t.test(folate, ventilation, p.adj="bonferroni")

#Relaxing the variance assumption
#The traditional one-way ANOVA requires an assumption of equal variances
#for all groups. There is, however, an alternative procedure that does
#not require that assumption. It is due to Welch and similar to the unequal
#variances t test. This has been implemented in the oneway.test function:

oneway.test(folate~ventilation)

#In this case with oneway.test, the p-value increased to a nonsignificant value, presumably
#related to the fact that the group that seems to differ from the other two
#also has the largest variance.

#It is also possible to perform the pairwise t tests so that they do not use
#a common pooled standard deviation. This is controlled by the argument pool.sd.

pairwise.t.test(folate,ventilation,pool.sd=F)

#Again, it is seen that the significance disappears as we remove the
#constraint on the variances.

#Granphical presentation
#There are many ways to present grouped data. Here we create
#a somewhat elaborate plot where the raw data are plotted as a stripchart
#and overlaid with an indication of means and SEM (standard of error of the mean):

xbar <- tapply(folate, ventilation, mean)
s <- tapply(folate, ventilation, sd)
n <- tapply(folate, ventilation, length)
sem <- s/sqrt(n)
stripchart(folate~ventilation,method="jitter",jitter=0.05,pch=16,vert=T)
arrows(1:3,xbar+sem,1:3,xbar-sem,angle=90,code=3,length=.1)
lines(1:3,xbar,pch=4,type="b",cex=2)



####################################################################################################

#The data set juul contains variables from an investigation performed by
#Anders Juul (Rigshospitalet, Department for Growth and Reproduction)
#concerning serum IGF-I (insulin-like growth factor) in a group of healthy
#humans, primarily school children. The data set is contained in the ISwR
#package and contains a number of variables, of which we only use igf1
#(serum IGF-I) for now, but later in the chapter we also use tanner (Tanner
#stage of puberty, a classification into five groups, based on appearance
#of primary and secondary sexual characteristics), sex, and menarche
#(indicating whether or not a girl has had her first period).

data(juul)
attach(juul)
anova(lm(igf1~tanner)) ## WRONG! Why?

#This does not describe a grouping of data but a linear regression on the
#group number! Notice the telltale 1 DF for the effect of tanner.
juul$tanner <- factor(juul$tanner,labels=c("I","II","III","IV","V"))
detach(juul);#We needed to reattach the juul data frame in order to use the changed definition.
attach(juul)
summary(tanner)
anova(lm(igf1~tanner))
