#-------------------------------------------#
# title: 判别分析                           #
# Author: Hongwei Shi                       #
# Ref: 薛毅，陈立萍 (2007). 统计建模与R语言 #
# Date: 2021-11-17                          #
#-------------------------------------------#


#----------距离判别----------#

## 两总体距离判别
discriminiant.distance <- function(TrnX1, TrnX2, 
                                   TstX = NULL, 
                                   var.equal = FALSE){
   # TrnX1, TrnX2: X1类, X2类训练样本
   # TstX: 待测样本
   # 如果不输入待测样本TstX, 待测样本为两个训练样本之和
   if (is.null(TstX) == TRUE) {TstX <- rbind(TrnX1,TrnX2)}
   if (is.vector(TstX) == TRUE)  {TstX <- t(as.matrix(TstX))}
   if (is.matrix(TstX) != TRUE) {TstX <- as.matrix(TstX)}
   if (is.matrix(TrnX1) != TRUE) {TrnX1 <- as.matrix(TrnX1)}
   if (is.matrix(TrnX2) != TRUE) {TrnX2 <- as.matrix(TrnX2)}

   nx <- nrow(TstX)
   blong <- matrix(rep(0, nx), nrow=1, byrow=TRUE, 
                   dimnames=list("blong", 1:nx))
   mu1 <- colMeans(TrnX1); mu2 <- colMeans(TrnX2) 
   if (var.equal == TRUE  || var.equal == T) {
      S <- var(rbind(TrnX1,TrnX2))
      w <- mahalanobis(TstX, mu2, S)-mahalanobis(TstX, mu1, S)
   } else {
      S1 <- var(TrnX1); S2 <- var(TrnX2)
      w <- mahalanobis(TstX, mu2, S2)-mahalanobis(TstX, mu1, S1)
   }
   for (i in 1:nx){
      blong[i] <- ifelse(w[i]>0, 1, 2)
   }
   return(blong)
}

# example8.1
classX1 <- data.frame(
   x1=c(6.60,  6.60,  6.10,  6.10,  8.40,  7.2,   8.40,  7.50,  
        7.50,  8.30,  7.80,  7.80),
   x2=c(39.00, 39.00, 47.00, 47.00, 32.00,  6.0, 113.00, 52.00,
        52.00,113.00,172.00,172.00),
   x3=c(1.00,  1.00,  1.00,  1.00,  2.00,  1.0,   3.50,  1.00,
        3.50,  0.00,  1.00,  1.50),
   x4=c(6.00,  6.00,  6.00,  6.00,  7.50,  7.0,   6.00,  6.00,
        7.50,  7.50,  3.50,  3.00),
   x5=c(6.00, 12.00,  6.00, 12.00, 19.00, 28.0,  18.00, 12.00,
        6.00, 35.00, 14.00, 15.00),
   x6=c(0.12,  0.12,  0.08,  0.08,  0.35,  0.3,   0.15,  0.16,
        0.16,  0.12,  0.21,  0.21),
   x7=c(20.00, 20.00, 12.00, 12.00, 75.00, 30.0,  75.00, 40.00,
        40.00,180.00, 45.00, 45.00)
)
classX2 <-data.frame(
   x1=c(8.40,  8.40,  8.40,  6.3, 7.00,  7.00,  7.00,  8.30,
        8.30,   7.2,   7.2,  7.2, 5.50,  8.40,  8.40,  7.50,
        7.50,  8.30,  8.30, 8.30, 8.30,  7.80,  7.80),
   x2=c(32.0 ,32.00, 32.00, 11.0, 8.00,  8.00,  8.00, 161.00,
        161.0,   6.0,   6.0,  6.0, 6.00,113.00,113.00,  52.00,
        52.00, 97.00, 97.00,89.00,56.00,172.00,283.00),
   x3=c(1.00,  2.00,  2.50,  4.5, 4.50,  6.00,  1.50,  1.50,
        0.50,   3.5,   1.0,  1.0, 2.50,  3.50,  3.50,  1.00,
        1.00,  0.00,  2.50, 0.00, 1.50,  1.00,  1.00),
   x4=c(5.00,  9.00,  4.00,  7.5, 4.50,  7.50,  6.00,  4.00,
        2.50,   4.0,   3.0,  6.0, 3.00,  4.50,  4.50,  6.00,
        7.50,  6.00,  6.00, 6.00, 6.00,  3.50,  4.50),
   x5=c(4.00, 10.00, 10.00,  3.0, 9.00,  4.00,  1.00,  4.00,
        1.00,  12.0,   3.0,  5.0, 7.00,  6.00,  8.00,  6.00,
        8.00,  5.00,  5.00,10.00,13.00,  6.00,  6.00),
   x6=c(0.35,  0.35,  0.35,  0.2, 0.25,  0.25,  0.25,  0.08,
        0.08,  0.30,   0.3,  0.3, 0.18,  0.15,  0.15,  0.16,
        0.16,  0.15,  0.15, 0.16, 0.25,  0.21,  0.18),
   x7=c(75.00, 75.00, 75.00,  15.0, 30.00, 30.00, 30.00, 70.00,
        70.00,  30.0,  30.0,  30.0, 18.00, 75.00, 75.00, 40.00,
        40.00,180.00,180.00,180.00,180.00, 45.00, 45.00)
)
res.equal <- discriminiant.distance(classX1, classX2, var.equal=TRUE)
res <- discriminiant.distance(classX1, classX2)

# 错判概率
mistake <- function(real, pred) {
   if (is.matrix(real) != TRUE) {real <- as.matrix(real)}
   if (is.matrix(pred) != TRUE) {real <- as.matrix(pred)}
   if (nrow(real) == 1) {real <- t(real)}
   if (nrow(pred) == 1) {pred <- t(pred)}
   return(mean(real[,1] != pred[,1]))
   # return(real[,1] == pred[,1])
}
real <- rep(c(1, 2), c(nrow(classX1), nrow(classX2)))
cat("variance is equal,", mistake(real, res.equal)*100, "%", "\n",
    "variance is not equal,", mistake(real, res)*100, "%")


library(MASS)
# set.seed(1117)
n1 <- 3000; n2 <- 5000; p <- 10
Sig <- diag(1, p)
mu1 <- sample(c(0, 1), p, replace=TRUE)
mu2 <- sample(c(0, 3, 4), p, replace=TRUE)
X1 <- mvrnorm(n=n1, mu=mu1, Sigma=Sig)
X2 <- mvrnorm(n=n2, mu=mu2, Sigma=Sig)
res.equal <- discriminiant.distance(X1, X2, var.equal=TRUE)
res <- discriminiant.distance(X1, X2, var.equal=FALSE)
real <- c(rep(1, nrow(X1)), rep(2, nrow(X2)))
cat("variance is equal,", mistake(real, res.equal)*100, "%", "\n",
    "variance is not equal,", mistake(real, res)*100, "%")

# 设置mu1和mu2很接近
mu1 <- sample(c(0, 1), p, replace=TRUE)
mu2 <- c(mu1[1]+0.1, mu1[-1])
X1 <- mvrnorm(n=n1, mu=mu1, Sigma=Sig)
X2 <- mvrnorm(n=n2, mu=mu2, Sigma=Sig)
res.equal <- discriminiant.distance(X1, X2, var.equal=TRUE)
res <- discriminiant.distance(X1, X2, var.equal=FALSE)
real <- c(rep(1, nrow(X1)), rep(2, nrow(X2)))
cat("variance is equal,", mistake(real, res.equal)*100, "%", "\n",
    "variance is not equal,", mistake(real, res)*100, "%")

## 多总体距离判别
distinguish.distance <- function(TrnX, TrnG, TstX = NULL, var.equal = FALSE){
   # TrnX: 训练样本
   # TrnG: 因子变量, 表示输入训练样本的分类情况
   # TstX: 待测样本
   # 如果不输入待测样本TstX, 待测样本为训练样本
   
   # 为了兼容二分类问题
   if (is.factor(TrnG) == FALSE){
      mx <- nrow(TrnX); mg <- nrow(TrnG)
      TrnX <- rbind(TrnX, TrnG)
      TrnG <- factor(rep(1:2, c(mx, mg)))
   }
   if (is.null(TstX) == TRUE) {TstX <- TrnX}
   if (is.vector(TstX) == TRUE) {TstX <- t(as.matrix(TstX))}
   if (is.matrix(TstX) != TRUE) {TstX <- as.matrix(TstX)}
   if (is.matrix(TrnX) != TRUE) {TrnX <- as.matrix(TrnX)}
   
   nx <- nrow(TstX)
   blong <- matrix(rep(0, nx), nrow=1, dimnames=list("blong", 1:nx))
   
   g <- length(levels(TrnG))
   mu <- matrix(0, nrow=g, ncol=ncol(TrnX))
   for (i in 1:g) {mu[i,] <- colMeans(TrnX[TrnG==i,])}
   D <- matrix(0, nrow=g, ncol=nx)
   if (var.equal == TRUE  || var.equal == T){
      for (i in 1:g) {
         D[i,]<- mahalanobis(TstX, mu[i,], var(TrnX))
      }
   } else {
      for (i in 1:g) {
         D[i,]<- mahalanobis(TstX, mu[i,], var(TrnX[TrnG==i,]))
      }
   }
   for (j in 1:nx){
      dmin <- Inf
      for (i in 1:g)
         if (D[i,j] < dmin){
            dmin <- D[i,j]; blong[j] <- i
         }
   }
   return(blong)
}

# example8.2
X <- iris[,1:4]
levels(iris$Species) <- 1:length(levels(iris$Species))
G <- iris$Species
res.equal <- distinguish.distance(X, G, var.equal=TRUE)
res <- distinguish.distance(X, G, var.equal=FALSE)
cat("variance is equal,", mistake(G, res.equal)*100, "%", "\n",
    "variance is not equal,", mistake(G, res)*100, "%")


#----------Bayes判别----------#

## 两总体Bayes判别
discriminiant.bayes <- function(TrnX1, TrnX2, 
                                rate=1, TstX = NULL, var.equal = FALSE){
   # TrnX1, TrnX2: X1类, X2类训练样本
   # rate: 临界值
   # TstX: 待测样本
   # 如果不输入待测样本TstX, 待测样本为两个训练样本之和
   if (is.null(TstX) == TRUE) {TstX <- rbind(TrnX1,TrnX2)}
   if (is.vector(TstX) == TRUE)  {TstX<-t(as.matrix(TstX))}
   if (is.matrix(TstX) != TRUE) {TstX<-as.matrix(TstX)}
   if (is.matrix(TrnX1) != TRUE) {TrnX1<-as.matrix(TrnX1)}
   if (is.matrix(TrnX2) != TRUE) {TrnX2<-as.matrix(TrnX2)}
   
   nx <- nrow(TstX)
   blong <- matrix(rep(0, nx), nrow=1, byrow=TRUE, 
                   dimnames=list("blong", 1:nx))
   mu1 <- colMeans(TrnX1); mu2 <- colMeans(TrnX2) 
   if (var.equal == TRUE  || var.equal == T){
      S <- var(rbind(TrnX1, TrnX2)); beta <- 2*log(rate)
      w <- mahalanobis(TstX, mu2, S) - mahalanobis(TstX, mu1, S)
   }
   else{
      S1 <- var(TrnX1); S2 <- var(TrnX2)
      beta <- 2*log(rate) + log(det(S1)/det(S2))
      w <- mahalanobis(TstX, mu2, S2) - mahalanobis(TstX, mu1, S1)
   }
   
   for (i in 1:nx){
      blong[i] <- ifelse(w[i]>beta, 1, 2)
   }
   return(blong)
}

# example8.3
TrnX1 <- matrix(
   c(24.8, 24.1, 26.6, 23.5, 25.5, 27.4, 
     -2.0, -2.4, -3.0, -1.9, -2.1, -3.1),
   ncol=2)
TrnX2 <- matrix(
   c(22.1, 21.6, 22.0, 22.8, 22.7, 21.5, 22.1, 21.4, 
     -0.7, -1.4, -0.8, -1.6, -1.5, -1.0, -1.2, -1.3),
   ncol=2)

res.equal <- discriminiant.bayes(TrnX1, TrnX2, rate=8/6, var.equal=TRUE)
res <- discriminiant.bayes(TrnX1, TrnX2, rate=8/6, var.equal=FALSE)
real <- rep(c(1, 2), c(nrow(TrnX1), nrow(TrnX2)))
cat("variance is equal,", mistake(real, res.equal)*100, "%", "\n",
    "variance is not equal,", mistake(real, res)*100, "%")

## 多总体Bayes判别
distinguish.bayes <- function(TrnX, TrnG, p=rep(1, length(levels(TrnG))), 
                              TstX = NULL, var.equal = FALSE){
   # TrnX: 训练样本
   # TrnG: 因子变量, 表示输入训练样本的分类情况
   # p: 先验概率向量
   # TstX: 待测样本
   # 如果不输入待测样本TstX, 待测样本为训练样本
   
   # 为兼容二分类问题
   if (is.factor(TrnG) == FALSE){
      mx <- nrow(TrnX); mg<-nrow(TrnG)
      TrnX <- rbind(TrnX, TrnG)
      TrnG <- factor(rep(1:2, c(mx, mg)))
   }
   if (is.null(TstX) == TRUE) {TstX <- TrnX}
   if (is.vector(TstX) == TRUE) {TstX <- t(as.matrix(TstX))}
   if (is.matrix(TstX) != TRUE) {TstX <- as.matrix(TstX)}
   if (is.matrix(TrnX) != TRUE) {TrnX <- as.matrix(TrnX)}
   
   nx <- nrow(TstX)
   blong <- matrix(rep(0, nx), nrow=1, dimnames=list("blong", 1:nx))
   g <- length(levels(TrnG))
   mu <- matrix(0, nrow=g, ncol=ncol(TrnX))
   for (i in 1:g) {
      mu[i,] <- colMeans(TrnX[TrnG==i,]) 
   }
   D <- matrix(0, nrow=g, ncol=nx)
   if (var.equal == TRUE  || var.equal == T) {
      for (i in 1:g){
         d2 <- mahalanobis(TstX, mu[i,], var(TrnX))
         D[i,] <- d2 - 2*log(p[i])
      }
   } else {
      for (i in 1:g) {
         S <- var(TrnX[TrnG==i,])
         d2 <- mahalanobis(TstX, mu[i,], S)
         D[i,] <- d2 - 2*log(p[i]) - log(det(S))
      }
   }
   for (j in 1:nx){
      dmin <- Inf
      for (i in 1:g)
         if (D[i,j] < dmin){
            dmin<-D[i,j]; blong[j]<-i
         }
   }
   return(blong)
}

# example8.4
X <- iris[,1:4]
levels(iris$Species) <- 1:length(levels(iris$Species))
G <- iris$Species
res.equal <- distinguish.bayes(X, G, var.equal=TRUE)
res <- distinguish.bayes(X, G, var.equal=FALSE)
cat("variance is equal,", mistake(G, res.equal)*100, "%", "\n",
    "variance is not equal,", mistake(G, res)*100, "%")

# #----------Fisher判别----------#
# discriminiant.fisher <- function(TrnX1, TrnX2, TstX = NULL){
#    # TrnX1, TrnX2: X1类, X2类训练样本
#    # TstX: 待测样本
#    # 如果不输入待测样本TstX, 待测样本为两个训练样本之和
#    if (is.null(TstX) == TRUE) {TstX <- rbind(TrnX1,TrnX2)}
#    if (is.vector(TstX) == TRUE)  {TstX <- t(as.matrix(TstX))}
#    if (is.matrix(TstX) != TRUE) {TstX <- as.matrix(TstX)}
#    if (is.matrix(TrnX1) != TRUE) {TrnX1 <- as.matrix(TrnX1)}
#    if (is.matrix(TrnX2) != TRUE) {TrnX2 <- as.matrix(TrnX2)}
#    
#    nx <- nrow(TstX)
#    blong <- matrix(rep(0, nx), nrow=1, byrow=TRUE, 
#                    dimnames=list("blong", 1:nx))
#    n1 <- nrow(TrnX1); n2 <- nrow(TrnX2) 
#    mu1 <- colMeans(TrnX1); mu2 <- colMeans(TrnX2) 
#    
#    S <- (n1-1)*var(TrnX1) + (n2-1)*var(TrnX2)
#    mu <- n1/(n1+n2)*mu1 + n2/(n1+n2)*mu2
#    w <- (TstX - rep(1,nx)%o%mu) %*% solve(S, mu2-mu1)
#    for (i in 1:nx){
#       blong[i] <- ifelse(w[i]<=0, 1, 2)
#    }
#    return(blong)
# }
# res <- discriminiant.fisher(classX1, classX2)
# real <- rep(c(1, 2), c(nrow(classX1), nrow(classX2)))
# cat(mistake(real, res)*100, "%")

