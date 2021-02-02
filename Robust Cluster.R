###############################
### Robust Cluster Analysis ###
###############################
# Pan Charoensuk
# Stat 454 - Robust Statistics
# Project 2 - Robust Clustering
###############################


#####################################
#install.packages("tclust")
#install.packages("NbClust")
#install.packages("mvtnorm")
#####################################




######################################################################################
#################################### SIMULATIONS #####################################
######################################################################################


sigma1 <- diag (2)
sigma2 <- diag (2) * 8 - 2
sigma3 <- diag (2) * 1000
sigma4 <- diag(2) * 10-4
mixt <- rbind(rmvnorm (22, mean = c (0.0, 0), sigma = sigma1),
              rmvnorm (22, mean = c (5,5), sigma = sigma2),
              rmvnorm (5, mean = c (25,25), sigma = sigma4))


mixt2 <- rbind(rmvnorm (22, mean = c (0.0, 0), sigma = sigma1),
               rmvnorm (22, mean = c (5,5), sigma = sigma2),
               rmvnorm (6, mean = c (25,25), sigma = sigma4))


mixt3 <- rbind(rmvnorm (67, mean = c (0.0, 0), sigma = sigma1),
               rmvnorm (67, mean = c (5,5), sigma = sigma2),
               rmvnorm (10, mean = c (25,25), sigma = sigma4))

# Determining number of clusters k
plot(ctlcurves(mixt, k = 1:4, alpha = seq(0, 0.5, by =  0.1), restr.fact = 50))
res <- NbClust(data = mixt, distance = "euclidean", min.nc = 2, max.nc = 5, method = "kmeans", index = "all")

# Discovering the breakdown point for trimmed k-means
mixt_k <- tclust(mixt, k = 2, alpha = 0, restr.fact = 1, equal.weights = TRUE)
plot(mixt_k, main = "(a) K-Means", xlim = c(-5,35), ylim = c(-5,35), 
     xlab = bquote(x[1]), ylab = bquote(x[2]))
mixt_tk <- tclust(mixt, k=2, alpha = 0.1, restr.fact = 1, equal.weights = TRUE)
plot(mixt_tk, main = "(b) Trimmed K-Means", xlim = c(-5,35), ylim = c(-5,35),
     xlab = bquote(x[1]), ylab = bquote(x[2]))
mixt2_tk <- tclust(mixt2, k=2, alpha = 0.1, restr.fact = 1, equal.weights = TRUE)
plot(mixt2_tk, main = "(c) Trimmed K-Means", xlim = c(-5,35), ylim = c(-5,35),
     xlab = bquote(x[1]), ylab = bquote(x[2]))


# Background noise is added

mixt3_tk = tclust(mixt3, k=2, alpha = 0.1, restr.fact = 1, equal.weights = TRUE)
plot(mixt3_tk, main = "(a) Trimmed K-Means",
     xlab = bquote(x[1]), ylab = bquote(x[2]))

mixt3_tclust = tclust(mixt3, k = 2, alpha = 0.1, restr.fact = 50, equal.weights = FALSE)
plot(mixt3_tclust, main = "(b) TCLUST", xlab = bquote(x[1]), ylab = bquote(x[2]))

# TCLUST correctly clusters the data while trimmed k-means does not.




######################################################################################
############################## MULTIDIMENSIONAL IRIS #################################
######################################################################################

iris
set.seed(5)
library(tclust)
library(NbClust)
library(mvtnorm)


# plotting CTL-Curves to determine k and alpha
plot(ctlcurves(iris[,1:4], k = 1:5, alpha = seq(0,0.3, by = 0.03)))
#no significant improvement when increase k from 3 to 4,5. Choose k = 3

# defining true clusters
true_iris = data.frame(cbind(iris[,1:4], true_clust = c(rep(2,50), rep(1,50), rep(3,50))))
names(true_iris) = c("x1","x2","x3","x4","true_clust")

# 10 simulations
num_sim <- 10
misclass_table1 <- matrix(NA, nrow = num_sim, ncol = 4)
for(i in 1:num_sim){
  set.seed(i)
  #kmeans 3 clusters
  iris_kmeans <- tclust(iris[,1:4], k = 3, alpha = 0, equal.weights = TRUE, restr.fact = 1)
  #calculate misclassification rate
  k_means_mr=sum(iris_kmeans$cluster!=true_iris$true_clust)/length(true_iris$true_clust)
  
  
  #trimmed kmeans
  iris_tkmeans <- tclust(iris[,1:4], k = 3, alpha = 0.03, equal.weights = TRUE, restr.fact = 1)
  #calculate misclassification rate
  tk_means_mr=sum(iris_tkmeans$cluster!=true_iris$true_clust)/length(true_iris$true_clust)
  
  #tclust
  ## eigen, restr.fact = 5
  iris_tclust.5 <- tclust(iris[,1:4], k = 3, alpha = 0, equal.weights = FALSE, restr.fact = 5)
  #calculate misclassification rate
  tclust.5_mr=sum(iris_tclust.5$cluster!=true_iris$true_clust)/length(true_iris$true_clust)
  
  ## eigen, restr.fact = 10
  iris_tclust.10 <- tclust(iris[,1:4], k = 3, alpha = 0, equal.weights = FALSE, restr.fact = 10)
  #calculate misclassification rate
  tclust.10_mr=sum(iris_tclust.10$cluster!=true_iris$true_clust)/length(true_iris$true_clust)
  
  ## misclassification rate table
  misclass_table1[i,1] <- k_means_mr
  misclass_table1[i,2] <- tk_means_mr
  misclass_table1[i,3] <- tclust.5_mr
  misclass_table1[i,4] <- tclust.10_mr

}

colnames(misclass_table1) <- c("K-means", "TkMeans", "TCLUST(upp_bound=5)", "TCLUST(upp_bound=10)")
rownames(misclass_table1) <- c("Run 1", "Run 2", "Run 3", "Run 4", "Run 5", "Run 6", "Run 7", "Run 8", 
                              "Run 9", "Run 10")
misclass_table1

# TkMeans and TCLUST gives inconsistent results. We will now increase nstart to 300 for
# TCLUST with upper bound = 10

misclass_table2 <- matrix(NA, nrow = num_sim, ncol = 4)
for(i in 1:num_sim){
  set.seed(i)
  #kmeans 3 clusters
  iris_kmeans <- tclust(iris[,1:4], k = 3, alpha = 0, equal.weights = TRUE, restr.fact = 1)
  #calculate misclassification rate
  k_means_mr=sum(iris_kmeans$cluster!=true_iris$true_clust)/length(true_iris$true_clust)
  
  
  #trimmed kmeans
  iris_tkmeans <- tclust(iris[,1:4], k = 3, alpha = 0.03, equal.weights = TRUE, restr.fact = 1)
  #calculate misclassification rate
  tk_means_mr=sum(iris_tkmeans$cluster!=true_iris$true_clust)/length(true_iris$true_clust)
  
  #tclust
  ## eigen, restr.fact = 5
  iris_tclust.5 <- tclust(iris[,1:4], k = 3, alpha = 0, equal.weights = FALSE, restr.fact = 5)
  #calculate misclassification rate
  tclust.5_mr=sum(iris_tclust.5$cluster!=true_iris$true_clust)/length(true_iris$true_clust)
  
  ## eigen, restr.fact = 10 with nstart = 300
  iris_tclust.10 <- tclust(iris[,1:4], k = 3, alpha = 0, equal.weights = FALSE, restr.fact = 10,
                           nstart = 300)
  #calculate misclassification rate
  tclust.10_mr=sum(iris_tclust.10$cluster!=true_iris$true_clust)/length(true_iris$true_clust)
  
  ## misclassification rate table
  misclass_table2[i,1] <- k_means_mr
  misclass_table2[i,2] <- tk_means_mr
  misclass_table2[i,3] <- tclust.5_mr
  misclass_table2[i,4] <- tclust.10_mr
  
}

colnames(misclass_table2) <- c("K-means", "TkMeans", "TCLUST(upp_bound=5)", "TCLUST(upp_bound=10)")
rownames(misclass_table2) <- c("Run 1", "Run 2", "Run 3", "Run 4", "Run 5", "Run 6", "Run 7", "Run 8", 
                              "Run 9", "Run 10")
misclass_table2 # results TCLUST upper bound = 10 are now consistent



######################################################################################
################################### CARS DATASET #####################################
######################################################################################

## Discriminant Factors to determine the appropriate k

# cars dataset is taken from Kaggle, https://www.kaggle.com/abineshkumark/carsdata
cars <- read.csv("cars.csv")
variables <- cars[,1:7]
colSums(is.na(variables))

# remove NA's in the variables.
variables <- variables[complete.cases(variables), ]
variables <- scale(variables)

cars <- cars[complete.cases(cars), ]
colSums(is.na(variables))

library(tclust)
par(mfrow = c(1,1))
plot(ctlcurves(variables, k = 1:5, alpha = seq(0, 0.3, by =  0.06), restr.fact = 50))

# TCLUST, discriminant factors function

x1 <- tclust(variables, k = 4, alpha = 0)
plot(DiscrFact(x1, threshold = 0.0001), enum.plots = TRUE)


x2 <- tclust(variables, k = 3, alpha = 0)
plot(DiscrFact(x2, threshold = 0.0001), enum.plots = TRUE)