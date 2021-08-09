###DG6333: Final Exam code correction for Q1.1
# Question1.1
library(car)
library(tidyr)
OldA <- c(45,38,52,48,25,39,51,46,55,46)
YouA <- c(34,22,15,27,37,41,24,19,26,36)
mydf <- cbind.data.frame(OldA, YouA)
mydfLong <- gather(mydf, "group")
# If we want to use nonparametric method to test, we should check data nomarlity and equality of variance.
# Here I use levene method to test the equality of variance.
attach(mydfLong)
result1 = leveneTest(value ~ group, mydfLong)
# Also use shapiro method to test data set normality.
result2 = shapiro.test(OldA)
result3 = shapiro.test(YouA)