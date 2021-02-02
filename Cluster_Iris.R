###############################
### Robust Cluster Analysis ###
###############################
# Pan Charoensuk
# Songwan Joun
# Stat 454/556 - Robust Statistics
# Project 1 - Robust Clustering
###############################





######################################################################################
############################## MULTIDIMENSIONAL IRIS #################################
######################################################################################

iris
set.seed(5)

# plotting CTL-Curves to determine k and alpha
plot(ctlcurves(iris[,1:4], k = 1:5, alpha = seq(0,0.3, by = 0.03)))
#no significant improvement when increase k from 3 to 4,5. Choose k = 3

# defining true clusters
true_iris = data.frame(cbind(iris[,1:4], true_clust = c(rep(2,50), rep(1,50), rep(3,50))))
names(true_iris) = c("x1","x2","x3","x4","true_clust")

#10 simulations
num_sim <- 10
misclass_table <- matrix(NA, nrow = num_sim, ncol = 4)
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
  misclass_table[i,1] <- k_means_mr
  misclass_table[i,2] <- tk_means_mr
  misclass_table[i,3] <- tclust.5_mr
  misclass_table[i,4] <- tclust.10_mr

}

colnames(misclass_table) <- c("K-means", "TkMeans", "TCLUST(upp_bound=5)", "TCLUST(upp_bound=10)")
misclass_table

#TkMeans and TCLUST gives inconsistent results. We will investigate more.