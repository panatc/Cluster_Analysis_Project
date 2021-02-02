###############################
### Robust Cluster Analysis ###
###############################
# Pan Charoensuk
# Songwan Joun
# Stat 454/556 - Robust Statistics
# Project 1 - Robust Clustering
###############################


###############################
######### EXAMPLES ############
###############################


###############################
#KMEANS
###############################
iris
set.seed(5)
k_iris = kmeans(iris[,c(1,3)], centers =3)
dim(iris)
par(mfrow=c(1,1))
#Let's see 1st and 2nd column
plot(iris[,1], iris[,3], col=k_iris$cluster, pch=19,
     xlab="Sepal.Length", ylab="Petal.Length", main="K-means (k=3)")
points(k_iris$centers[,1], k_iris$centers[,2], col="blue", pch=20, cex=5)
idx_kmeans = which(as.numeric(iris$Species)!=k_iris$cluster)
points(iris[idx_kmeans,1], iris[idx_kmeans,3], col=iris[idx_kmeans,5], cex=2, pch=1)
legend("bottomright", legend = c("True", "Kmeans","Cluster center"), 
       pch=c(1,19,19), col=c("black","black","blue"))
text(k_iris$centers, c("51","58","41"),col="white")
sum(k_iris$cluster==1)
sum(k_iris$cluster==2)
sum(k_iris$cluster==3)
sum(k_iris$cluster==4)

###############################
#Trimming
###############################
#install.packages("tclust")
library ("tclust")
set.seed(5)
tk_iris <- tkmeans (iris[,c(1,3)], k = 3, alpha = 0.1)
par(mfrow=c(1,1))
plot (tk_iris, xlab="Sepal.Length", ylab="Petal.Length", main="Trimmed K-means (k=3)")

points(tk_iris$centers[1,], tk_iris$centers[2,], col="black", pch=20, cex=5)
text(tk_iris$centers[1,],tk_iris$centers[2,], c(sum(tk_iris$cluster==1),sum(tk_iris$cluster==2),sum(tk_iris$cluster==3)),col="white")

idx_tkmeans = which(as.numeric(iris$Species)!=tk_iris$cluster & tk_iris$cluster!=0)
points(iris[idx_tkmeans,1], iris[idx_tkmeans,3], col=rep(c("green","blue"), c(5,11)),
     pch=1, cex=2)
legend("bottomright", legend = c("True", "Kmeans"), pch=c(1,19))

sum(tk_iris$cluster==1)
sum(tk_iris$cluster==2)
sum(tk_iris$cluster==3)

par(mfrow=c(2,2))
  for(alpha in c(0.05, 0.1, 0.2, 0.4)){
    tk_iris <- tkmeans (iris[,c(1,3)], k = 3, alpha = alpha)
    plot (tk_iris, xlab="Sepal.Length", ylab="Petal.Length", main="Trimmed K-means (k=3)")
    points(tk_iris$centers[1,], tk_iris$centers[2,], col="black", pch=20, cex=5)
    text(tk_iris$centers[1,],tk_iris$centers[2,], c(sum(tk_iris$cluster==1),sum(tk_iris$cluster==2),sum(tk_iris$cluster==3)),col="white")
    
  }

###############################
#TCLUST
###############################
 
#constraint on eigenvalues
set.seed(0)
a = tclust(iris[,c(1,3)], alpha=0, rest=c("eigen"), restr.fact=1)
b = tclust(iris[,c(1,3)], alpha=0, rest=c("eigen"), restr.fact=2.5)
c = tclust(iris[,c(1,3)], alpha=0, rest=c("eigen"), restr.fact=30)
par(mfrow=c(2,2))
plot(iris[,c(1,3)], col=c(rep("green", sum(iris$Species=="setosa")),
                          rep("red", sum(iris$Species=="versicolor")),
                          rep("blue", sum(iris$Species=="virginica"))),
     main = "Actual cluster",xlim=c(3,9), ylim=c(0,9))
plot(a, main="Relative size of the axes",xlim=c(3,9), ylim=c(0,9))
plot(b, main="Relative size of the axes",xlim=c(3,9), ylim=c(0,9))
plot(c, main="Relative size of the axes",xlim=c(3,9), ylim=c(0,9))


#constraint on determinants
set.seed(0)
a = tclust(iris[,c(1,3)], alpha=0, rest=c("deter"), restr.fact=1)
b = tclust(iris[,c(1,3)], alpha=0, rest=c("deter"), restr.fact=2.5)
c = tclust(iris[,c(1,3)], alpha=0, rest=c("deter"), restr.fact=10)
par(mfrow=c(2,2))
plot(iris[,c(1,3)], col=c(rep("green", sum(iris$Species=="setosa")),
                          rep("red", sum(iris$Species=="versicolor")),
                          rep("blue", sum(iris$Species=="virginica"))),
     main = "Actual cluster",xlim=c(3,9), ylim=c(0,9))
plot(a, main="Relative volumnes of the axes",xlim=c(3,9), ylim=c(0,9), col=c("red","blue","green")) #default had bad cluster colors
plot(b, main="Relative volumnes of the axes",xlim=c(3,9), ylim=c(0,9))
plot(c, main="Relative volumnes of the axes",xlim=c(3,9), ylim=c(0,9))


#forcing clusters to be the same
set.seed(0)
a = tclust(iris[,c(1,3)], alpha=0, rest="sigma")
par(mfrow=c(1,2))
plot(iris[,c(1,3)], col=c(rep("green", sum(iris$Species=="setosa")),
                          rep("blue", sum(iris$Species=="versicolor")),
                          rep("red", sum(iris$Species=="virginica"))),
     main = "Actual cluster",xlim=c(3,9), ylim=c(0,9))
plot(a, main="Exact same clusters",xlim=c(3,9), ylim=c(0,9))
#TCLUST gives an error..this explains why this example did not show the correct result in our the presentation...


#################################
######### SIMULATION ############
#################################
par(mfrow=c(1,1))
library(mvtnorm)

#generate multivariate normal data
set.seed (10)
sigma1 <- diag (2) ## EigenValues: 1, 1
sigma2 <- diag (2) * 8 - 2 ## EigenValues: 8, 4
sigma3 <- diag (2) * 50 ## EigenValues: 50, 50
sigma4 <- diag(2) * 10-4
mixt <- rbind (rmvnorm (360, mean = c (0.0, 0), sigma = sigma1),
  rmvnorm (540, mean = c (5.0, 10), sigma = sigma2),
  rmvnorm (100, mean = c (2.5, 5), sigma = sigma3),
  rmvnorm(75, mean = c(20,35), sigma = sigma4))

plot(mixt, main = "Simulation", xlab = bquote(x[1]), ylab = bquote(x[2]))

mixt2 = data.frame(cbind(mixt, true_clust = c(rep(2,360), rep(1,540), rep(0,100))))
names(mixt2) = c("x","y","true_clust")

#select k , alpha
#ctl-curves
plot(ctlcurves(mixt, k = 1:4, alpha = seq(0, 0.20, by =  0.05)))

#kmeans 4 clusters
sim_kmeans4 <- tclust(mixt, k = 4, alpha = 0, equal.weights = TRUE, restr.fact = 1)
plot(sim_kmeans4,main = "K-means", xlab = bquote(x[1]), ylab = bquote(x[2]), ylim = c(-15,50),
     xlim = c(-20,30))

#calculate misclassification rate
k_means4_mr=sum(sim_kmeans4$cluster!=mixt2$true_clust)/length(mixt2$true_clust)

#kmeans 3 clusters
sim_kmeans3 <- tclust(mixt, k = 3, alpha = 0, equal.weights = TRUE, restr.fact = 1)
plot(sim_kmeans3,main = "K-means", xlab = bquote(x[1]), ylab = bquote(x[2]), ylim = c(-15,50),
     xlim = c(-20,30))

#calculate misclassification rate
k_means3_mr=sum(sim_kmeans3$cluster!=mixt2$true_clust)/length(mixt2$true_clust)



#trimmed kmeans
sim_tkmeans <- tclust(mixt, k = 3, alpha = 0.05, equal.weights = TRUE, restr.fact = 1)
plot(sim_tkmeans, main = "Trimmed K-means")

#calculate misclassification rate
tkmeans_mr <- sum(sim_tkmeans$cluster!=mixt2$true_clust)/length(mixt2$true_clust)


#tclust
## eigen, restr.fact = 5
sim_tclust_eigen1 <- tclust (mixt, k = 3, alpha = 0.05, restr.fact = 5)
plot (sim_tclust_eigen1)

#calculate misclassification rate
tclust_eigen_mr1 <- sum(sim_tclust_eigen1$cluster!=mixt2$true_clust)/length(mixt2$true_clust)

## eigen, restr.fact = 50
sim_tclust_eigen2 <- tclust (mixt, k = 3, alpha = 0.05, restr.fact = 50)
plot (sim_tclust_eigen2)

#calculate misclassification rate
tclust_eigen_mr2 <- sum(sim_tclust_eigen2$cluster!=mixt2$true_clust)/length(mixt2$true_clust)

## misclassification rate table
misclass_table <- cbind(k_means4_mr, k_means3_mr, tkmeans_mr, tclust_eigen_mr1, tclust_eigen_mr2)
colnames(misclass_table) <- c("K-means(k=4)", "K-means(k=3)", "TkMeans", "TCLUST(upp_bound=5)", "TCLUST(upp_bound=50)")
misclass_table



