####################################################################
# Implement G-means algorithm                                      #
####################################################################

# Install necessary packages

#cleanup before start
rm(list=ls(all=T))

packages <- list('pracma', 'nortest', 'ellipse', 'cluster', 'fpc') 


InstallAll <- function(packageName){
  if(!(packageName %in% installed.packages())){
    install.packages(packageName,dependencies = T)
  }
}

sapply(packages, InstallAll)

library(pracma)
library(nortest)
library(ellipse)
library(cluster)
library(fpc)

set.seed(100)

######## synthetic data ########
x1 <- rnorm(100, mean = 20, sd = 2)
y1 <- rnorm(100, mean = 20, sd = 2)
z1 <- rnorm(100, mean = 20, sd = 2)

x2 <- rnorm(100, mean = 60, sd = 2)
y2 <- rnorm(100, mean = 60, sd = 2)
z2 <- rnorm(100, mean = 60, sd = 2)

x3 <- rnorm(100, mean = 100, sd = 2)
y3 <- rnorm(100, mean = 100, sd = 2)
z3 <- rnorm(100, mean = 100, sd = 2)

x4 <- rnorm(100, mean = 140, sd = 2)
y4 <- rnorm(100, mean = 140, sd = 2)
z4 <- rnorm(100, mean = 140, sd = 2)

x <- c(x1, x2, x3)
y <- c(y1, y2, y3)
z <- c(z1, z2, z3)

points <- data.frame()

points <- read.csv("hw45-r3b-test-data.csv", header = TRUE, sep = ',')
plot(points)

# X is the input dataset(it is a dataframe contain multiple cols reading from csv file)
Gmeans <- function(X, alpha = 0.0001, k=1) {
  globalK <- 0
  while(nrow(X) != 0) {
    km <- kmeans(X, centers = k)
    X["cluster"] <- km$cluster
    for(i in unique(X$cluster)) {
      s <- subset(X, cluster == i)
      if(nrow(s) <= 7) {
        X <- X[!(X$cluster == i),]
        next
      }
      km2 <- kmeans(s, centers = 2)
      c1 <- km2$centers[1, ] # first centroid
      c2 <- km2$centers[2, ] # second centroid
      v = c1 - c2
      projections <- rep(0, nrow(s))
      for(j in 1:nrow(s)) {
        # project each point on the vector of d-dim v
        projections[j] <- dot(unlist(s[j, ]), v)/sum(v^2)
      }
      scaled_projs <- scale(projections)
      #print(ad.test(scaled_projs))
      if(ad.test(scaled_projs)$p.value < alpha) {
        k <- k + 1
      } else {
        X <- X[!(X$cluster == i),]
      }
    }
  }
  return(k)
}

clusters <- Gmeans(points)
print(paste("Number of clusters found: " , clusters))

km <- kmeans(points, centers = clusters)
points["cluster"] <- km$cluster


plot(0, type="p", xlim=c(0,300), ylim=c(0,300), xlab = "x1", ylab = "x2")
colors <- c("red", "blue", "green", "purple", "black", "yellow", "pink", "cyan")
for(i in 1:clusters) {
  s <- subset(points, cluster == i)
  points(s[, c("x1", "x2")], col=colors[i])
}

plot(0, type="p", xlim=c(0,300), ylim=c(0,300), xlab = "x2", ylab = "x3")
colors <- c("red", "blue", "green", "purple", "black", "yellow", "pink", "cyan")
for(i in 1:clusters) {
  s <- subset(points, cluster == i)
  points(s[, c("x2", "x3")], col=colors[i])
}


plot(0, type="p", xlim=c(0,300), ylim=c(0,300), xlab = "x1", ylab = "x3")
colors <- c("red", "blue", "green", "purple", "black", "yellow", "pink", "cyan")
for(i in 1:clusters) {
  s <- subset(points, cluster == i)
  points(s[, c("x1", "x3")], col=colors[i])
}

# discussed with ibobra
