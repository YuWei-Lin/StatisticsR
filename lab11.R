setwd("C:/Users/jinyoung/Desktop/2020/Teaching2020/MDACC2020/MDACC_SHP_2020/DG6333_2020/Lectures/Lecture11_Clustering")
dta <- read.table("lab11.txt",header=TRUE)
k<-length(dta)
n<-nrow(dta)
k;n
sum(is.na(dta))

#############################################################################################
######################################### PCA using Eigen ###################################
#############################################################################################
# 1.1. Perform PCA using the spectral decomposition of a matrix (R-mode PCA)
pc.eigen.cor <- princomp(dta,cor=TRUE);#with correlation matrix
scores.eigen <- pc.eigen.cor$scores ; #the principal components;

# 1.2. Visualize the first two scores
plot(scores.eigen[,1],scores.eigen[,2], xlab="PCA 1", ylab="PCA 2")
title(main="Principal Components Analysis using princomp in R", col.main="black", font.main=1)

#############################################################################################
####################################### PCA using svd #######################################
#############################################################################################
# 2.1. Perform PCA using the singular value decompostion of a matrix (Q-mode PCA)
##prcomp includes the scaled step. The option "center" sets TRUE as default but "scale" sets FALSE as default
pc.scale.svd <- prcomp(dta,scale.=TRUE);# compare to princomp(dta,cor=TRUE)
scores.svd <- pc.scale.svd$x ; #the principal components


# 2.2. Compare results from the two analyses to demonstrate equivalency.
#Maximum differences should be close to zero if the two approaches are equivalent.
range(abs(scores.eigen)  - abs(scores.svd))

# 2.3. Visualize the first two scores
plot(scores.svd[,1],scores.svd[,2], xlab="PCA 1", ylab="PCA 2")
title(main="Principal Components Analysis using prcomp in R", col.main="black", font.main=1)


