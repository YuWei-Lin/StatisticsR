### Homework 3
# Question 1
library(ISwR)
View(thuesen)
write.table(thuesen, "C:/Users/YLin21/Desktop/THUESEN.txt", sep = "\t", row.names = F)
thuesen.edited <- read.table("C:/Users/YLin21/Desktop/THUESEN_Edited.txt", header = T, sep = "\t")
write.table(thuesen.edited, "C:/Users/YLin21/Desktop/THUESEN_Editedout.txt", sep = "\t", row.names = F)

# Question 2
View(malaria)
attach(malaria)
mylogit <- glm(mal ~ age + log(ab, base = 10), data = malaria, family = "binomial")
summary(mylogit)
# We can use "wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = "position of categorical estimates")" 
# if we have categorical estimates.
mylogit2 <- glm(mal ~ log(ab, base = 10), data = malaria, family = "binomial")
summary(mylogit2)

# Question 3.1
View(graft.vs.host)
?graft.vs.host
attach(graft.vs.host)
Surv(time, dead==1) # The symbol "+" means they continue to be alive supposedly.
#Kaplan-Meier
install.packages("survminer")
library("survminer")
survfit(Surv(time, dead==1) ~ gvhd)
fit <- survfit(Surv(time, dead==1) ~ gvhd)
summary(fit)
ggsurvplot(
  fit, data = graft.vs.host, # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  conf.int.style = "step",  # customize style of confidence intervals
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 200,     # break X axis in time intervals by 200.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = 
    c("GVHD-NO", "GVHD-YES"),    # change legend labels.
  palette = 
    c("#E7B800", "#2E9FDF") # custom color palettes.
)
# Test the hypothesis whether the survival is the same in both groups.
surv_diff <- survdiff(Surv(time, dead==1) ~ gvhd)
surv_diff
# Extend to other predictors.
fit_all <- survfit(Surv(time, dead==1) ~ gvhd + preg + type)
summary(fit_all)
# Plot survival curves by gvhd and facet by preg and type
ggsurvplot(
  fit_all, data = graft.vs.host, # survfit object with calculated statistics.
  pval = T,             # show p-value of log-rank test.
  conf.int = F,         # show confidence intervals for 
  # point estimaes of survival curves.
  conf.int.style = "step",  # customize style of confidence intervals
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 200,     # break X axis in time intervals by 200.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations # in legend of risk table.
  ncensor.plot = F,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.  
)
# Question 3.2
View(stroke)
?stroke
attach(stroke)
sex.cox <- coxph(Surv(obsmonths, dead) ~ sex, data = stroke) # Univariate
sex.cox
age.cox <- coxph(Surv(obsmonths, dead) ~ age, data = stroke) # Univariate
age.cox
res.cox <- coxph(Surv(obsmonths, dead) ~ sex + age, data = stroke) # multivariate
res.cox

# Question 4.1
View(react)
?react
length(react)
hist(react) # Yes, it looks like normal distributed.
# Then we should do one sample t-test to see if this distribution is different from zero.
t.test(react) # We reject H0 and accept H1. It is significantly different from zero.

# Question 4.2
View(vitcap)
?vitcap
Grp1 <- vitcap[ vitcap$group == 1, 3]
Grp3 <- vitcap[ vitcap$group == 3, 3]
t.test(Grp1, Grp3, paired = F, conf.level = 0.99)
plot(density(Grp3), col = "green")
lines(density(Grp1), col = "red")

# Question 4.3
# For react data, we can do one sample Wilcoxon-test if we don't assume normality.
# Our goal is to tell if these measurements are statistcally different. (m == 0 ???)
wilcox.test(react, y=NULL, paired = F, mu = 0, alternative = "two.sided")
wilcox.test(react, y=NULL, paired = F, mu = 0, alternative = "greater")
wilcox.test(react, y=NULL, paired = F, mu = 0, alternative = "less")
# For vitcap data, we can do Wilcoxon-test rank-sum method if we don't assume normality.
# Our goal is to tell if these two independent groups are statistcally different in vital capacity.
MatGrp <- cbind.data.frame(sort(Grp1), sort(Grp3))
colnames(MatGrp) <- c("Exposed10yr", "Not_Exposed")
wilcox.test(MatGrp$Exposed10yr, MatGrp$Not_Exposed, paired = F, alternative = "two.sided")
