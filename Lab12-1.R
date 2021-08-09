#Principal Component Analysis
#install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

#1. Simple example using iris data
View(iris)
?iris
View(iris3)
#iris <- iris
head(iris,3);#display a few rows of data
head(iris)

#Species should be removed before PCA analysis. Why?
?PCA
PCA
##1.1
iris.pca <- PCA(iris[,-5], graph = FALSE)
names(iris.pca)
iris.pca$eig

##1.2
iris.pca1 <- PCA(iris[,1:4], graph = FALSE)
names(iris.pca1)
iris.pca1$eig

#keep using iris.pca
print(iris.pca)
#summary(iris.pca)

#Visualization and Interpretation
#you can easily extract and visualize the results of PCA using R functions provided in the factoextra R package.
#to extract the eigenvalues/variances of principal components(pcs)
eig.val <- get_eigenvalue(iris.pca)
eig.val
sum(eig.val[,1])
#[1] 4; why?

eig.val[1,1]/sum(eig.val[,1])
#[1] 0.7296245; what can you observe?

##Try this!
eig.val[2,1]/sum(eig.val[,1])
eig.val[2,1]/sum(eig.val[,1])+eig.val[1,1]/sum(eig.val[,1])

#to visualize the eigenvalues
fviz_eig(iris.pca, addlabels = TRUE, ylim = c(0, 100))

#To extract the results for individuals and variables, respectively.
ind <- get_pca_ind(iris.pca); ind 
var <- get_pca_var(iris.pca); var

#coordinates
head(var$coord)

#Cos2: quality on the factor map
head(var$cos2)

#Contributions to the principal components
head(var$contrib)

##Correlation circle
# Coordinates of variables
head(var$coord, 4)

#To visualize the results individuals and variables, respectively.
fviz_pca_ind(iris.pca); fviz_pca_var(iris.pca)

fviz_pca_var(iris.pca, col.var="red")

#To make a biplot of individuals and variables.
fviz_pca_biplot(iris.pca)

head(var$cos2)

#install.packages("corrplot")
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

#Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(iris.pca, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
#variables with low cos2 values will be colored in "#00AFBB"
#variables with mid cos2 values will be colored in "#E7B800"
#variables with high cos2 values will be colored in "#FC4E07"
fviz_pca_var(iris.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
             )

# Change the transparency by cos2 values
fviz_pca_var(iris.pca, alpha.var = "cos2")

#Contribution of variables to pcs
head(var$contrib)
#head(var$contrib, 4)
corrplot(var$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(iris.pca, choice = "var", axes = 1)
# Contributions of variables to PC2
fviz_contrib(iris.pca, choice = "var", axes = 2)
###Note that if data contains many variables, 
#you can retain the top contributing variables 
#with option "top=10" that displays only the top10 varuables contributing to the pcs.

#par(mfrow=c(1,2))    
# set the plotting area into a 1*2 array

#Total contribution to PC1 and PC2
fviz_contrib(iris.pca, choice = "var", axes = 1:2)

#Note that The red dashed line on the graph above indicates the expected average contribution.

#The most important (or, contributing) variables can be highlighted on the correlation plot as follow:
fviz_pca_var(iris.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
             )

#Here again to change the transparency by contrib values
fviz_pca_var(iris.pca, alpha.var = "contrib")


#Color by a custom continuous variable
# Create a random continuous variable of length 4
set.seed(123)
my.cont.var <- rnorm(4);#change the num. of pcs
# Color variables by the continuous variable
fviz_pca_var(iris.pca, col.var = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")

#Color by groups
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
iris.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(iris.km$cluster)
# Color variables by groups
fviz_pca_var(iris.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

#Dimension description to identify the most significantly associated variables with a given principal component
iris.desc <- dimdesc(iris.pca, axes = c(1,2), proba = 0.05)
#Description of dimension 1
iris.desc$Dim.1

#Description of dimension 2
iris.desc$Dim.2

#Graph of individuals/samples
#for individuals/samples, results can be extracted using get_pca_ind()
#the function is similar to get_pca_var().

ind <- get_pca_ind(iris.pca)
ind

# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
head(ind$contrib)

#Plots: quality and contribution
#The fviz_pca_ind() is used to produce the graph of individuals.
fviz_pca_ind(iris.pca)

fviz_pca_ind(iris.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
             )
#Note that individuals that are similar are groupded together.

#To change the point size according the cos2 of the corresponding individuals:
fviz_pca_ind(iris.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
             )

#To change both point size and color by cos2, try this:
fviz_pca_ind(iris.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
             )

#To create a bar plot of the quality of representation (cos2) of individuals on the factor map, 
#you can use the function fviz_cos2() as previously described for variables:
fviz_cos2(iris.pca, choice = "ind")

#To visualize the contribution of individuals to the first two principal components.
fviz_contrib(iris.pca, choice = "ind", axes = 1:2)

# individuals can be colored by any custom continuous variable by specifying the argument col.ind.
# Create a random continuous variable of length 150,
# Same length as the number of active individuals in the PCA
set.seed(123)
n <- nrow(iris)
my.cont.var <- rnorm(n)
# Color individuals by the continuous variable
fviz_pca_ind(iris.pca, col.ind = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")



###########################################################
#Color by groups
fviz_pca_ind(iris.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
             )

#To remove the group mean point, specify the argument mean.point = FALSE.
#If you want confidence ellipses instead of concentration ellipses, use ellipse.type = "confidence".

# Add confidence ellipses
fviz_pca_ind(iris.pca, geom.ind = "point", col.ind = iris$Species, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups"
             )

#To use the jco (journal of clinical oncology) color palette
fviz_pca_ind(iris.pca,
             label = "none", # hide individual labels
             habillage = iris$Species, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             palette = "jco"
             )

###########################################################
#To visualize them on dimensions 2 and 3 instead of 1 and 2
# Variables on dimensions 2 and 3
fviz_pca_var(iris.pca, axes = c(2, 3))
# Individuals on dimensions 2 and 3
fviz_pca_ind(iris.pca, axes = c(2, 3))

#plot elements with either point or text
# Show variable points and text labels
fviz_pca_var(iris.pca, geom.var = c("point", "text"))

# Show individuals text labels only
fviz_pca_ind(iris.pca, geom.ind =  "text")

#Size and shape of plot elements
# Change the size of arrows an labels
fviz_pca_var(iris.pca, arrowsize = 1, labelsize = 5, 
             repel = TRUE)
# Change points size, shape and fill color
# Change label size
fviz_pca_ind(iris.pca, 
             pointsize = 3, pointshape = 21, fill = "lightblue",
             labelsize = 5, repel = TRUE)

#Ellipses
# Add confidence ellipses
fviz_pca_ind(iris.pca, geom.ind = "point", 
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups"
)
# Convex hull
fviz_pca_ind(iris.pca, geom.ind = "point",
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "convex",
             legend.title = "Groups"
)

#Group mean points
fviz_pca_ind(iris.pca,
             geom.ind = "point", # show points only (but not "text")
             group.ind = iris$Species, # color by groups
             legend.title = "Groups",
             mean.point = FALSE)

#Axis lines
fviz_pca_var(iris.pca, axes.linetype = "blank")

#To change easily the graphical of any ggplots, you can use the function ggpar() [ggpubr package]
ind.p <- fviz_pca_ind(iris.pca, geom = "point", col.ind = iris$Species)
ggpubr::ggpar(ind.p,
              title = "Principal Component Analysis",
              subtitle = "Iris data set",
              caption = "Source: factoextra",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco"
              )

#To make a simple biplot of individuals and variables
fviz_pca_biplot(iris.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
                )

#Adding group
fviz_pca_biplot(iris.pca, 
                col.ind = iris$Species, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

#To customize individuals and variable colors, we use the helper functions fill_palette() and color_palette() [in ggpubr package].
fviz_pca_biplot(iris.pca, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = iris$Species,
                col.ind = "black",
                # Color variable by groups
                col.var = factor(c("sepal", "sepal", "petal", "petal")),
                
                legend.title = list(fill = "Species", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
                )+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors

#To color individuals by groups (discrete color) and variables by their contributions to the principal components (gradient colors). 
#Additionally, we'll change the transparency of variables by their contributions using the argument alpha.var.
fviz_pca_biplot(iris.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = iris$Species, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
                )


























