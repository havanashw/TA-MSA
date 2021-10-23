#-----------------------#
# title: 上机课作业示例 #
# Author: Hongwei Shi   #
# Date: 2021-09-29      #
#-----------------------#

rm(list = ls())
library(MASS)

## 第二次上机作业
# Compound symmetry structure: all off-diagonal elements are rho
rho <- 0.5 # choose rho for 0-1
p <- 5; n <- 10
Sig.cs <- outer(rep(rho, p), rep(1, p), "*"); diag(Sig.cs) <- 1
x <- mvrnorm(n, mu=rep(0, p), Sigma=Sig.cs)

# banded matrix: 仅当|i−j|<k时，元素非0，其他的都是0
Sig.band <- matrix(0, p, p)
k <- p
for (i in 1:nrow(Sig.band)) {
  for (j in 1:ncol(Sig.band)) {
    # if(abs(i-j) < k) { Sig.band[i,j] <- rho }
    if(abs(i-j) < k) { Sig.band[i,j] <- rho^(abs(i-j)) }
    diag(Sig.band) <- 1
  }
}

# auto regression structure: all off-diagonal elements are rho^|i-j|
Sig.ar <- toeplitz(rho^seq(0, p-1))
x <- mvrnorm(n, mu=rep(0, p), Sigma=Sig.ar)


## 第一次上机作业
beta_var <- function(n=100, p=50, q=20, part=TRUE, sparse=TRUE, loops=1000) {
  beta.est.mat <- matrix(NA, loops, q)
  var.ture.list <- list()
  for (loop in 1:loops) {
    # set.seed(proc.time()[1]*1000 + loop)
    # set.seed(20200929 + loop)
    
    # 多元正态分布
    # Sig <- diag(1, p)
    # x <- mvrnorm(n, mu=rep(0, p), Sigma=Sig)
    
    # 均匀分布
    x <- matrix(0, n, p)
    for (ii in 1:p) {
      x[,ii] <- runif(n, -sqrt(2), sqrt(2))
    }
    
    if(sparse == TRUE) { beta.ture <- c(1, rep(0, p-1)) }
    if(sparse == FALSE) { 
      # beta.ture <- scale(sample(1:(10*p), p)) 
      beta.ture <- sample(1:(10*p), p)
    }
    beta.ture <- beta.ture/sum(beta.ture^2)
    error <- rnorm(n)
    y <- x%*%beta.ture + error
    # part.pos <- sample(1:p, q)
    part.pos <- 1:q
    
    if(part == TRUE) {
      fit.full <- lm(y ~ x[,part.pos]+0) # 是否去掉截距项
      beta.est <- coef(fit.full)
      var.ture <- solve(t(x[,part.pos])%*%(x[,part.pos]))
    }
    if(part == FALSE) {
      fit.full <- lm(y ~ x+0)
      beta.est <- coef(fit.full)[part.pos]
      var.ture <- solve(t(x)%*%x)[part.pos,part.pos] # ginv()
    }
    beta.est.mat[loop,] <- beta.est
    var.ture.list[[loop]] <- var.ture
    cat(loop, "\r")
  }
  # var.hat <- apply(beta.est.mat, 2, var)
  var.hat <- cov(beta.est.mat)
  var.ture <- Reduce("+",var.ture.list)/loops
  return(list(var.ture=var.ture, var.hat=var.hat))
  # return(list(res1=sum(diag(var.ture)), res2=sum(diag(var.hat))))
}

# 非稀疏的情形
beta_var(n=100, p=2, q=1, part=TRUE, sparse=FALSE, loops=1000)
beta_var(n=100, p=2, q=1, part=FALSE, sparse=FALSE, loops=1000)

# 稀疏的情形
beta_var(n=100, p=5, q=2, part=TRUE, sparse=TRUE, loops=1000)
beta_var(n=100, p=5, q=2, part=FALSE, sparse=TRUE, loops=1000)
