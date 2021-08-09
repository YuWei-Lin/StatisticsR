
library(readr)
lab1_1 <- read_delim(file.choose(), "\t", escape_double = FALSE, trim_ws = TRUE)
NEWCON <- sub("'", "", colnames(lab1_1))
NEWCON <- sub("'", "", NEWCON)
colnames(lab1_1) <- NEWCON
DATAFR <- lab1_1[ , 1:4]
DATAFR[DATAFR$creat_68 == 9.99, 4] <- NA
DATAFR <- DATAFR[!(is.na(DATAFR$creat_68)), ]
### Extrac rows that have specific group number.
GP1 <- DATAFR[DATAFR$group == 1, 4]
mean(GP1$creat_68, na.rm = T)
sd(GP1$creat_68, na.rm = T)
GP2 <- DATAFR[DATAFR$group == 2, 4]
mean(GP2$creat_68, na.rm = T)
sd(GP2$creat_68, na.rm = T)
GP3 <- DATAFR[DATAFR$group == 3, 4]
mean(GP3$creat_68, na.rm = T)
sd(GP3$creat_68, na.rm = T)
### Boxplot
boxplot(DATAFR$creat_68~DATAFR$group, data=DATAFR, id.method="y")
### Since group size is >2, we use ANOVA to calculate F-score.
DATAFR <- DATAFR[order(DATAFR$group), ]
flow <- DATAFR$creat_68
group <- factor(DATAFR$group)
result <- lm(flow~group)
anova(result) #print anova table from object result
### Then try tranditional way to see if matched.
### Calculate SS, MS
GrandM <- mean(DATAFR$creat_68, na.rm = T)
GP1M <- mean(GP1$creat_68)
GP2M <- mean(GP2$creat_68)
GP3M <- mean(GP3$creat_68)
# Create a SS matix
SSSumary <- matrix(NA, nrow = 3, ncol = 3)
SSSumary <- as.data.frame(SSSumary)
colnames(SSSumary) <- c("SumofSquare","degreeoffreedom","Mean Square")
rownames(SSSumary) <- c("Between_GP", "Within_GP", "Total")
SSSumary[1,1] <- sum(nrow(GP1)*((GP1M-GrandM)**2), nrow(GP2)*((GP2M-GrandM)**2), nrow(GP3)*((GP3M-GrandM)**2))
SSSumary[2,1] <- sum(sum((GP1$creat_68-GP1M)**2), sum((GP2$creat_68-GP2M)**2), sum((GP3$creat_68-GP3M)**2))
SSSumary[3,1] <- sum(sum((GP1$creat_68-GrandM)**2), sum((GP2$creat_68-GrandM)**2), sum((GP3$creat_68-GrandM)**2))
### k = 3, therefore:
k <- 3
SSSumary[1,2] <- k-1
SSSumary[2,2] <- nrow(DATAFR)-k
SSSumary[3,2] <- nrow(DATAFR)-1
SSSumary[1,3] <- SSSumary[1,1]/SSSumary[1,2]
SSSumary[2,3] <- SSSumary[2,1]/SSSumary[2,2]
Fscore <- SSSumary[1,3]/SSSumary[2,3]
