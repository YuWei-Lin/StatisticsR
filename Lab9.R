#The Sign Test
##slide 14:Assess the significance of the skin ointment data using R.
#n=40 and x=18
prop.test(18,40,p=0.5,alternative = "two.sided", correct=TRUE)

##slide 16: Assess the statistical significance of the results for eye drops using R.
#n < 20 
binom.test(8, 10, p = 0.5,alternative = "two.sided",conf.level = 0.95)

##slide 29: create data from table on slide 21
d <- c(-8,-7,-7,-7,-6,-6,-5,-5,-4,-3,-3,-3,-3,-3,-2,-2,-2,-2,-1,-1,-1,-1,3,3,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1)
length(d)
wilcox.test(d,y=NULL,alternative="two.sided",mu=0,paired=FALSE,exact=NULL,correct=TRUE,conf.int=FALSE)

# Practice 1
binom.test(1, 10, p = 0.5, alternative = "two.sided",conf.level = 0.95)
# Practice 2
d <- c(-0.238,-0.085,-0.215,-0.227,-0.037,0.09,-0.736,-0.365,-0.179,-0.048)
length(d)
wilcox.test(d,y=NULL,alternative="two.sided",mu=0,paired=FALSE,exact=NULL,correct=TRUE,conf.int=FALSE)

#slide 38
#import lab9.1.txt
wilcox.test(VA ~ g, alternative = "two.sided", mu = 0, paired = FALSE, exact = NULL, correct = TRUE, conf.int = FALSE, data=lab9_1)

#import lab9.2.txt
wilcox.test(lab9_2$DOM, lab9_2$SL, alternative = "two.sided", mu = 0, paired = FALSE, exact = NULL, correct = TRUE, conf.int = FALSE)


