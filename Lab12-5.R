# NCI60 Data Example
#setwd("C:/Users/jinyoung/Desktop/2020/Teaching2020/MDACC2020/MDACC_SHP_2020/DG6333_2020/Lectures/Lecture12_RF_CART/")

#library(readr)
#nci60data <- read_csv("nci60.csv")

library(ISLR)
help("NCI60")

ncilabels=NCI60$labs
nci60data=NCI60$data

#1. PCA 
pcaRes <- prcomp(nci60data, scale=TRUE)
biplot(pcaRes)

# make colors as factors. 
labs <- as.character(unlist(as.list(ncilabels)))

cellColor <- function(vec)
{
  uvec <- unique(vec)
  cols = rainbow(length(uvec))
  colvec <- cols[as.numeric(as.factor(vec))]
  list(colvec=colvec, cols=cols, labels= uvec)
}

par(mfrow=c(1,2))

colres <- cellColor(labs)

plot(pcaRes$x[,1:2],col=colres$colvec, xlab = "z1", ylab="z2", pch=19)
legend("bottomright", legend = colres$labels, text.col = colres$cols, 
       bty="n", cex=0.80)
plot(pcaRes$x[,c(1,3)], col=colres$colvec, xlab="z1", ylab="z3", pch=19)
legend("topright",  legend = colres$labels,text.col = colres$cols, 
       bty ="n" , cex=0.80)

par(mfrow=c(1,1))

library(dendextend)

sdata <- scale(nci60data)
d <- dist(sdata)
labs <- as.character(unlist(as.list(ncilabels)))
comp_clust <- hclust(d)
dend <- as.dendrogram(comp_clust)
leaves <- labs[order.dendrogram(dend)]
labels_colors(dend, labels=TRUE) <- cellColor(leaves)$colvec
labels(dend) <- leaves
plot(dend, main ="Clustering using Complete Linkage")

###############################################################################
#Exercise:
#1.Perform hierarchical clustering using average and single linkage.
#2.Interpret the difference in the dendrograms.
#3.Can you observe some patterns from these dendrograms? (hint: use cutree) 
###############################################################################
#1. The plots can be made with the following code
plot(hclust(d, method="average"), labels= labs,
     main ="Clustering using Average Linkage" , xlab="", ylab="" )

plot(hclust(d, method="single"), labels= labs, 
     main ="Clusteringg using Single Linkage" , xlab="", ylab="" )

#2. 
#Briefly, complete linkage provides maximal inter cluster dissimilarity, 
#single linkage results in minimal inter cluster dissimilarity and average results in mean inter cluster dissimilarity. 
#We see that the results are affected by the choice of the linkage. 
#Single linkage tend to yield trailing clusters while complete and average linkage leads to more balanced and attractive clusters.

#3.
#For our example, we see that the cell lines within a single cancer cell type do not cluster together. 
#But if we consider complete linkage and cut the tree at height=4 ( we tried different heights to observe patterns) 
#we observe some clear patterns like the leukemia cell lines fall in cluster 2 and the breast cancer cell lines spread out.
hc <- cutree(comp_clust, 4)
table(hc, labs)

