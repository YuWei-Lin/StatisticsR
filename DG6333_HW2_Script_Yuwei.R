### DG6333 HW2
library(ISwR)
#Question #1-1: Basic R-skills
vec1 <- c(1:6)
vec2 <- c(1:4, NA, 6)
is.na(vec2)
table(is.na(vec2))
vec3 <- c(1:5, 7)
table(vec1==vec3)
#Question #1-2: Basic R-skills
help("juul")
#According to data description, 2 is for girl.
objData <- juul[juul$age>=7&juul$age<=14&juul$sex==2, ]
dim(objData)

#Question #2-1: Simple linear regression
help("rmr")
attach(rmr)
plot(body.weight, metabolic.rate)
abline(lm(metabolic.rate~body.weight),col= "red")
summary(lm(metabolic.rate~body.weight)) 
# The equation is "MRate = 811.2 + 7.06*BodyW.
# If the body weight is 65 kg, MRate will be 1270.
# Then we calculate the predicted metabolic rate at 95% confidence, which is 
# cofficeint +- 2 standard error.
paste("The lower limit is ", 7.06-2*0.978, "; The higher limit is ", 7.06+2*0.978, sep="")
detach(rmr)

#Question #2-2: Simple linear regression
AgeOver <- juul[juul$age>25, ]
# Then we only need IGF-1 and age columns to see the relationship.
AgeOverIGF <- AgeOver[ ,c(1,4)]
# Remove rows that contain either of two features is NA.
AgeOverIGF <- AgeOverIGF[ complete.cases(AgeOverIGF), ]
AgeOverIGF$igf1 <- AgeOverIGF$igf1**0.5 # Replace IGF-1 with quare roots of IGF-1
attach(AgeOverIGF)
plot(age, igf1)
abline(lm(igf1~age),col= "red")
summary(lm(igf1~age))
detach(AgeOverIGF)
#Question #2-3: Simple linear regression
help("malaria")
View(malaria)
malaria$ab <- log(malaria$ab, base = 10)
attach(malaria)
plot(age, ab)
abline(lm(ab~age),col= "red")
summary(lm(ab~age))
# Yes. It is peculiar. It seems like ages should be treated as categorial independent variables.
detach(malaria)
contrasts(factor(malaria$age)) # Check the dummy variable matrix

#Question #3-1: Multiple linear regression
pairs(secher, gap=0, cex.labels=0.9)
secher <- log(secher, base = 10)
attach(secher)
summary(lm(bwt~bpd+ad))
detach(secher) # Both coefficients have good intepretation towards body weight.
#Question #3-2: Multiple linear regression
library(ISwR)
View(ISwR::tlc)
attach(ISwR::tlc)
sex <- factor(sex)
summary(lm(tlc~age+sex+height))
model.matrix(tlc~sex)
detach(ISwR::tlc)
#Question #3-3: Multiple linear regression
# Simple explain the question.
#Question #3-4: Multiple linear regression
AgeOver2 <- juul2[juul2$age>25, ]
# Then we only need IGF-1 and age columns to see the relationship.
AgeOver2IGF <- AgeOver2[ ,c(1,2,5,8)]
# Remove rows that contain at least one of features is NA.
AgeOver2IGF <- AgeOver2IGF[complete.cases(AgeOver2IGF), ]
AgeOver2IGF$igf1 <- AgeOver2IGF$igf1**0.5 # Replace IGF-1 with quare roots of IGF-1
attach(AgeOver2IGF)
summary(lm(igf1~age+height+weight))
anova(lm(igf1~age+height+weight))
summary(lm(igf1~age))
detach(AgeOver2IGF)
#Question #3-5: Multiple linear regression
attach(kfm)
View(kfm)
summary(lm(dl.milk~sex+weight+ml.suppl+mat.weight+mat.height))
# Then we try to remove unpredictable variables: Sex, ml.suppl and mat.weight
summary(lm(dl.milk~weight+mat.height))
#summary(lm(dl.milk~sex+weight+mat.height))
detach(kfm)