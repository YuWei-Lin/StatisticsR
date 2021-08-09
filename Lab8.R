#Lecture 8. Survival Analysis

#install.packages("survival")
library(survival)
library(ISwR)

attach(melanom)

names(melanom)

#View(melanom)

help(melanom)

Surv(days, status==1) # The symbol "+" means they continue to be alive supposedly.

#Kaplan-Meier
survfit(Surv(days, status==1) ~ 1)

fit_all <- survfit(Surv(days, status==1) ~ 1)
summary(fit_all)
plot(fit_all) 

#ulc:a numeric vector code, ulceration; 1: present, 2: absent.
survfit(Surv(days, status==1) ~ ulc)

#par(mfrow=c(1,1))
fit1 <- survfit(Surv(days, status==1) ~ ulc)
plot(fit1, lty = 2:3) 
legend(100, .8, c("Present", "Absent"), lty = 2:3)

fit1 <- survfit(Surv(days, status==1) ~ ulc)
plot(fit1, lty = 2:3, conf.int=T,col=c("red","blue")) 
legend(100, .8, c("Present", "Absent"), lty = 2:3,col=c("red","blue"))


survfit(Surv(days, status==1) ~ sex)

fit2 <- survfit(Surv(days, status==1) ~ sex)
plot(fit2, lty = 2:3) 
legend(100, .8, c("Female", "Male"), lty = 2:3)

fit2 <- survfit(Surv(days, status==1) ~ sex)
plot(fit2, lty = 2:3,conf.int=0.99,col=c("red","blue")) 
legend(100, .8, c("Female", "Male"), lty = 2:3,col=c("red","blue"))

fit3=survfit(Surv(days, status==1) ~ sex+ulc)
plot(fit3, lty=1:4,col=c("red","pink","green","blue"))
fit3
#log-rank test
survdiff(Surv(days,status==1)~sex)

survdiff(Surv(days,status==1)~sex+strata(ulc))

survdiff(Surv(days,status==1)~sex+ulc)

#Cox proportional hazard model
summary(coxph(Surv(days,status==1)~sex))

summary(coxph(Surv(days,status==1)~sex+log(thick)+strata(ulc)))

survfit(coxph(Surv(days,status==1)~log(thick)+sex+strata(ulc)))
plot(survfit(coxph(Surv(days,status==1)~log(thick)+sex+strata(ulc))),lty=2:3, col=c("red","blue"))
