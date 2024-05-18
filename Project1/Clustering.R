# Only work with numerical data (one with the cleaned numerical data and another with the dissimMatrix and another )
setwd("/home/daniel/Escritorio/Uni/4-1Qt/MD/Proyecto1")
df <- read.csv("speeddatingCleaned.csv", TRUE)

numericalVariablesPosition <- c(3,8,9,10,14,15,16,17,18,19,20,21,22)

d <- dist(df[numericalVariablesPosition])

h1 <- hclust(d,method="ward.D")
plot(h1)

nc = 2 # The objective variable is binary

c1 <- cutree(h1,nc)

pairs(df[,numericalVariablesPosition], col=c1)



#Second Clustering with distMatrix and num vars
library(cluster)
dissimMatrix <- daisy(df[,numericalVariablesPosition], metric = "gower", stand=TRUE)
distMatrix<-dissimMatrix^2

h2 <- hclust(distMatrix,method="ward.D")
plot(h2)

c2 <- cutree(h2,2)

pairs(df[,numericalVariablesPosition], col=c2)


#Third Clustering with distMatrix and num+cat vars
library(cluster)
variablesNonBinary <- c(3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
df[,3] <- as.numeric(df[,3])
dissimMatrix <- daisy(df[,variablesNonBinary], metric = "gower", stand=TRUE)
distMatrix<-dissimMatrix^2

h3 <- hclust(distMatrix,method="ward.D")
plot(h3)

c3 <- cutree(h2,4)

pairs(df[,variablesNonBinary], col=c3)

