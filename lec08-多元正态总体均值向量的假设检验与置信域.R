#-----------------------------------------------#
# title: 多元正态总体均值向量的假设检验与置信域       #
# Author: Hongwei Shi                           #
# Date: 2021-11-03                              #
#-----------------------------------------------#

rm(list = ls())

##############################################
####################置信域####################
##############################################

#--------------单总体均值向量检验--------------#
## Sigma已知

# 生成数据
library(MASS)
mu <- c(0, 0)
Sig <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
n <- 1000
mv.data <- mvrnorm(n, mu, Sig)
colnames(mv.data) <- c("X1", "X2")
mu.hat <- colMeans(mv.data)
Sig.hat <- cov(mv.data)

# ellipse() in package mixtools
library(mixtools)
plot(mv.data)
mixtools::ellipse(mu=mu.hat, sigma=Sig, alpha=0.05, col='red')
points(t(mu.hat), col='red', pch=16)

# stat_ellipse()
library(ggplot2)
qplot(data=data.frame(mv.data), x=X1, y=X2) +
  stat_ellipse(level=0.95, color="red") +
  geom_point(aes(x=mu.hat[1], y=mu.hat[2]), color="red", size=2) +
  theme_bw()

# dataEllipse() in package car
library(car)
par(mfrow = c(1, 2))
X1 <- mv.data[,1]; X2 <- mv.data[,2]
dataEllipse(X1, X2, levels=0.95, fill=FALSE)
points(t(mu.hat), col='red', pch = 8)

dataEllipse(X1, X2, levels=seq(0.05, 0.95, len=5), 
            ellipse.label=seq(0.05, 0.95, len=5), 
            lty=2, fill=TRUE, fill.alpha=0.1) 
points(t(mu.hat), col='red', pch=8) 

# 进一步探索
par(mfrow = c(1, 2))
n <- nrow(mv.data); p <- ncol(mv.data)
chi.stat <- sqrt(1/n*qchisq(0.95, p))
f.stat <-  sqrt((p/n)*((n-1)/(n-p)) * qf(0.95, p, n-p))
dataEllipse(X1, X2, levels=0.95, fill=FALSE, col="red")
car::ellipse(center=mu.hat, shape=Sig, radius=sqrt(n)*chi.stat, add=TRUE)

dataEllipse(X1, X2, levels=0.95, fill=FALSE, col="red")
car::ellipse(center=mu.hat, shape=Sig.hat, radius=sqrt(n)*chi.stat, add=TRUE)

# ellipse{ellipse}
library(ellipse)
n <- nrow(mv.data); p <- ncol(mv.data); df <- data.frame(x=X1, y=X2)
ell <- ellipse::ellipse(x=Sig, centre=mu.hat, level=0.95)
qplot(data=df, x=x, y=y) +
  geom_path(data=data.frame(ell), aes(x=x, y=y)) +
  stat_ellipse(level=0.95, color="red") +
  theme_bw()

# 对比ellipse{ellipse}和car{ellipse}
par(mfrow = c(1, 1))
ell <- ellipse::ellipse(x=Sig, centre=mu.hat, level=0.95)
plot(mv.data)
car::ellipse(center=mu.hat, shape=Sig, radius=sqrt(n)*chi.stat, add=TRUE)
points(ell, col="red", pch=16)

rm(list = ls())
## Sigma未知
# 生成数据
library(MASS)
mu <- c(0, 2)
Sig<- matrix(c(1, 0.7, 0.7, 1), 2, 2)
n <- 100
mv.data <- mvrnorm(n, mu, Sig)

# 自定义函数构造置信域
conf_ellipse <- function(mv.data, alpha){
  n <- nrow(mv.data); p <- ncol(mv.data)
  if(p != 2) stop("Only for bivariate normal")
  colnames(mv.data) <- paste0("X", 1:p)
  mu.hat <- colMeans(mv.data)
  Sig.hat <- cov(mv.data)
  eig.Sig <- eigen(Sig.hat)
  e1 <- eig.Sig$vec %*% diag(sqrt(eig.Sig$val))
  # r1 <- sqrt(p*(n-1)/(n*(n-p)) * qf(1-alpha, p, n-p)) # 半径
  r1 <- sqrt(p*(n-1)/(n-p) * qf(1-alpha, p, n-p)) # 半径
  theta <- seq(0, 2*pi, len=300)
  v1 <- cbind(r1*cos(theta), r1*sin(theta))
  pts <- t(mu.hat-(e1%*%t(v1)))
  plot(pts, type="l", col='red', lwd=3,
       main="Confidence Region for Bivariate Normal",
       xlab=colnames(mv.data)[1], ylab=colnames(mv.data)[2], asp=1)
  grid(lty=1, equilogs=FALSE)
  segments(-10, mu.hat[2], mu.hat[1], mu.hat[2], lty=2, lwd=2)
  segments(mu.hat[1], -10, mu.hat[1], mu.hat[2], lty=2, lwd=2)
  
  
  # 添加轴线
  eig.vec <- eig.Sig$vec
  slope.l <- eig.vec[2,1]/eig.vec[1,1]  # 长轴
  slope.s <- eig.vec[2,2]/eig.vec[1,2]  # 短轴
  abline(a=mu.hat[2]-slope.l*mu.hat[1], b=slope.l, lty=4, lwd=2)
  abline(a=mu.hat[2]-slope.s*mu.hat[1], b=slope.s, lty=4, lwd=2)
  
  # 添加中心
  points(x=mu.hat[1], y=mu.hat[2], pch=15, col="red")
  # th2 <- c(0, pi/2, pi, 3*pi/2)
  # v2 <- cbind(r1*cos(th2), r1*sin(th2))
  # pts2 <- t(mu.hat-(e1%*%t(v2)))
  # segments(pts2[1,1], pts2[1,2], pts2[3,1], pts2[3,2], lty=4, lwd=2)  
  # segments(pts2[2,1], pts2[2,2], pts2[4,1], pts2[4,2], lty=4, lwd=2)
  ell <- ellipse::ellipse(x=Sig.hat, centre=mu.hat, level=1-alpha)
  points(ell, pch=10, col="blue")
}
conf_ellipse(mv.data, alpha=0.05)

#--------------两正态总体均值向量的检验--------------#
## Sigma未知
# 生成数据
library(MASS)
Sig <- matrix(c(1, 0.7, 0.7, 1), 2, 2)
n <- 100
mv.data1 <- mvrnorm(n, mu=c(10, 5), Sig)
mv.data2 <- mvrnorm(n, mu=c(5, 7), Sig)

diff_conf_ellipse <- function(mv.data1, mv.data2, alpha){
  n <- nrow(mv.data1); p1 <- ncol(mv.data1)
  m <- nrow(mv.data2); p2 <- ncol(mv.data2)
  if(p1 != p2) stop("Only for the same dimension")
  if(p1 != 2) stop("Only for bivariate normal")
  if(p2 != 2) stop("Only for bivariate normal")
  p <- p1
  mu1.hat <- colMeans(mv.data1); Sig1.hat <- cov(mv.data1)
  mu2.hat <- colMeans(mv.data2); Sig2.hat <- cov(mv.data2)
  S.pool <- ((n-1)*p/(n+m-p-1))*Sig1.hat + ((m-1)*p/(n+m-p-1))*Sig2.hat
  S <- (1/n+1/m)*S.pool
  eig.Sig <- eigen(S)
  e1 <- eig.Sig$vec %*% diag(sqrt(eig.Sig$val))
  r1 <- sqrt(p*(n+m)/((n+m-p-1)*n*m) * qf(1-alpha, p, n+m-p-1))
  theta <- seq(0,2*pi, len=300)
  v1 <- cbind(r1*cos(theta), r1*sin(theta))
  pts <- t(mu1.hat - mu2.hat - (e1%*%t(v1)))
  plot(pts, type="l", col='red', lwd=3,
       main="Confidence Region for the difference of two mean vector",
       xlab=expression(delta[1]), ylab=expression(delta[2]), asp=1)
  grid(lty=1, equilogs=FALSE)
  
  # 添加轴线
  delta1 <- mu1.hat[1]-mu2.hat[1]
  delta2 <- mu1.hat[2]-mu2.hat[2]
  eig.vec <- eig.Sig$vec
  slope.l <-  eig.vec[2,1]/eig.vec[1,1]  # 长轴
  slope.s <-  eig.vec[2,2]/eig.vec[1,2]  # 短轴
  abline(a=delta2-slope.l*delta1, b=slope.l, lty=4, lwd=2)
  abline(a=delta2-slope.s*delta1, b=slope.s, lty=4, lwd=2)
  
  # 添加中心
  points(x=delta1, y=delta2, pch=15, col="red")
}
diff_conf_ellipse(mv.data1, mv.data2, alpha=0.05)

##############################################
###################假设检验###################
##############################################

## H0: mu=mu0, Sigma已知
# 生成数据
library(MASS)
alpha <- 0.05
n <- 100; p <- 20
mu0 <- rep(0, p)
Sigma0 <- toeplitz(0.3^seq(0, p-1))

# chisq test
pval.vec <- c()
loops <- 1000
for(i in 1:loops){
  mv.data <- mvrnorm(n, mu0, Sigma0) 
  X.bar <- colMeans(mv.data)
  chi.stat <- n*t(X.bar-mu0)%*%solve(Sigma0)%*%(X.bar-mu0)
  pval.vec[i] <- 1 - pchisq(chi.stat, p)
  cat(i, "\r")
}
mean(pval.vec < alpha)

mutest_known <- function(data, mu0, Sigma0, alpha=0.05) {
  # H0: mu=mu0 when Sigma0 is known
  # This is a Chisq testing
  
  #----------input----------#
  # data: design matrix
  # mu0: mu0 for null hypothesis
  # Sigma0: the known variance matrix
  # alpha: the significant level, default value=0.05
  #----------output----------#
  # Reject.area: reject region
  # p.value: p-value
  
  data <- as.matrix(data) # 将数据框转化为矩阵
  n <- nrow(data); p <- ncol(data)
  X.bar <- colMeans(data)
  chi.stat <- n*t(X.bar-mu0)%*%solve(Sigma0)%*%(X.bar-mu0)
  
  low.q <- qchisq(1-alpha, p) # 求下侧分位点, 上侧: lower.tail=FALSE
  
  reject <- matrix(c(chi.stat, low.q), nrow=1) # 按行排
  rownames(reject) <- c("Reject") # 行名
  colnames(reject) <- c("Obs", paste0("> ", 1-alpha)) # 列名
  
  pval <- 1 - pchisq(chi.stat, p)
  return(list(Reject.area=reject, p.value=pval))
}
mv.data <- mvrnorm(n, mu0, Sigma0) 
mutest_known(mv.data, mu0, Sigma0, alpha)

mv.data <- mvrnorm(n, mu0, Sigma0) 
mutest_known(mv.data, mu0, Sigma0, alpha)

## H0: mu=mu0, Sigma未知
# 生成数据
library(MASS)
alpha <- 0.05
n <- 500; p <- 2
mu0 <- rep(0, p)
Sigma0 <- diag(1, p)

# F test
pval.vec <- c()
loops <- 1000
for(i in 1:loops){
  mv.data <- mvrnorm(n, mu0, Sigma0) 
  X.bar <- colMeans(mv.data)
  S <- cov(mv.data)
  f.stat <- n*(n-p)/((n-1)*p)*t(X.bar-mu0)%*%solve(S) %*%(X.bar-mu0)
  pval.vec[i] <- 1 - pf(f.stat, p, n-p)
  cat(i, "\r")
}
mean(pval.vec < alpha)

mutest_unknown <- function(data, mu0, alpha=0.05) {
  # H0: mu=mu0 when Sigma0 is unknown
  # This is a F testing
  
  #----------input----------#
  # data: design matrix
  # mu0: mu0 for null hypothesis
  # alpha: the significant level, default value=0.05
  #----------output----------#
  # Reject.area: reject region
  # p.value: p-value
  
  data <- as.matrix(data) # 将数据框转化为矩阵
  n <- nrow(data); p <- ncol(data)
  X.bar <- colMeans(data)
  S <- cov(data)
  f.stat <- n*(n-p)/((n-1)*p)*t(X.bar-mu0)%*%solve(S) %*%(X.bar-mu0)
  
  low.q <- pf(1-alpha, p, n-p) # 求下侧分位点, 上侧: lower.tail=FALSE
  
  reject <- matrix(c(f.stat, low.q), nrow=1) # 按行排
  rownames(reject) <- c("Reject") # 行名
  colnames(reject) <- c("Obs", paste0("> ", 1-alpha)) # 列名
  
  pval <- 1 - pf(f.stat, p, n-p)
  return(list(Reject.area=reject, p.value=pval))
}
mv.data <- mvrnorm(n, mu0, Sigma0) 
mutest_unknown(mv.data, c(10, rep(0, p-1)), alpha)

# 对比chisq test 和 F-test
alpha <- 0.05; loops <- 1000
res <- data.frame(n=NA, p=NA, chi_power=NA, f_power=NA, chi_size=NA, f_size=NA)
k <- 1
for (n in c(20, 50, 100, 500, 1000)) {
  for (p in c(2, 5, 10, 15)) {
    mu0 <- rep(0, p)
    Sigma0 <- diag(1, p)
    chi.pval <- c(); f.pval <- c()
    chi.pval.size <- c(); f.pval.size <- c()
    for(loop in 1:loops){
      mv.data <- mvrnorm(n, mu0, Sigma0)
      chi.pval[loop] <- mutest_known(mv.data, c(0.1, rep(0, p-1)), Sigma0, alpha)$p.value
      f.pval[loop] <- mutest_unknown(mv.data, c(0.1, rep(0, p-1)), alpha)$p.value
      chi.pval.size[loop] <- mutest_known(mv.data, mu0, Sigma0, alpha)$p.value
      f.pval.size[loop] <- mutest_unknown(mv.data, mu0, alpha)$p.value
    }
    res[k,] <- c(n, p, 
                 round(mean(chi.pval<alpha), 3), round(mean(f.pval<alpha), 3),
                 round(mean(chi.pval.size<alpha), 3), round(mean(f.pval.size<alpha),3))
    k <- k + 1
  }
  cat("n = ", n, "\n")
}
res

## H0: mu1-mu2=0, Sigma未知
diff_mutest_unknown <- function(data1, data2, alpha=0.05) {
  # H0: mu1-mu2=0 when Sigma0 is unknown
  # This is a F testing
  
  #----------input----------#
  # data1 and data2: design matrix
  # mu1 and mu2: mu1 - mu2 = 0 for null hypothesis
  # alpha: the significant level, default value=0.05
  #----------output----------#
  # Reject.area: reject region
  # p.value: p-value
  
  data1 <- as.matrix(data1) # 将数据框转化为矩阵
  data2 <- as.matrix(data2)
  n <- nrow(data1); m <- nrow(data2)
  p <- ncol(data1)
  X.bar <- colMeans(data1); Y.bar <- colMeans(data2)
  V1 <- (n-1)*cov(data1); V2 <- (m-1)*cov(data2)
  f.stat <- n*m*(n+m-p-1)/((n+m)*p)*t(X.bar-Y.bar)%*%solve(V1+V2)%*%(X.bar-Y.bar)

  low.q <- pf(1-alpha, p, n+m-p-1) # 求下侧分位点, 上侧: lower.tail=FALSE
  
  reject <- matrix(c(f.stat, low.q), nrow=1) # 按行排
  rownames(reject) <- c("Reject") # 行名
  colnames(reject) <- c("Obs", paste0("> ", 1-alpha)) # 列名
  
  pval <- 1 - pf(f.stat, p, n+m-p-1)
  return(list(Reject.area=reject, p.value=pval))
}

# 生成数据
library(MASS)
alpha <- 0.05
n <- 500; m <- 1000; p <- 30
Sigma0 <- toeplitz(0.3^seq(0, p-1))
mu1 <- rep(0, p); mu2 <- c(1, 1, rep(0, p-2))
# mu1 <- rep(0, p); mu2 <- mu1

# F test
pval.vec <- c()
loops <- 100
for(i in 1:loops){
  mv.data1 <- mvrnorm(n, mu1, Sigma0) 
  mv.data2 <- mvrnorm(m, mu2, Sigma0) 
  pval.vec[i] <- diff_mutest_unknown(mv.data1, mv.data2, alpha)$p.value
  cat(i, "\r")
}
mean(pval.vec < alpha)

# 做对比, 自己尝试

# Alternative using package ICSNP
library(MASS)
n <- 500; p <- 10
mu0 <- rep(0, p)
Sigma0 <- diag(1, p)
mv.data <- mvrnorm(n, mu0, Sigma0)
mutest_unknown(mv.data, mu0, alpha)

library(ICSNP)
HotellingsT2(mv.data, mu=mu0) 

mutest_unknown(mv.data, c(0.1, rep(0, p-1)), alpha)
HotellingsT2(mv.data, mu=c(0.1, rep(0, p-1))) 

# Hotelling T2 test for the diffence of two mean vector
n <- 500; m <- 1000; p <- 30
Sigma0 <- toeplitz(0.3^seq(0, p-1))
mu1 <- rep(0, p); mu2 <- mu1
mv.data1 <- mvrnorm(n, mu1, Sigma0) 
mv.data2 <- mvrnorm(m, mu2, Sigma0) 
diff_mutest_unknown(mv.data1, mv.data2, alpha=0.05)
HotellingsT2(mv.data1, mv.data2, mu=rep(0, p))

mu1 <- rep(0, p); mu2 <- c(0.1, rep(0, p-1))
mv.data1 <- mvrnorm(n, mu1, Sigma0) 
mv.data2 <- mvrnorm(m, mu2, Sigma0) 
diff_mutest_unknown(mv.data1, mv.data2, alpha=0.05)
HotellingsT2(mv.data1, mv.data2, mu=rep(0, p))

## 多个正态总体均值向量的检验---多元方差分析
# 生成数据, group=3
n <- 300; p <- 10
n1 <- n*0.3; n2 <- n*0.2; n3 <- n - n1 - n2
mu0 <- rep(0, p)
Sigma0 <- diag(1, p)
mv.data <- mvrnorm(n, mu0, Sigma0)
group <- as.factor(c(rep("1", n1), rep("2", n2), rep("3", n3)))
gp.data <- data.frame(mv.data, group)

SST <- (nrow(mv.data)-1)*cov(mv.data)
# Compute SSA
SSA_func <- function(data){
  n <- nrow(data); l <- ncol(data)                
  r <- length(unique(data[,l])) # group的个数
  SSA <- 0
  for (i in 1:r){
    n_group <- sum(data[,l] == i)
    SSA <- SSA + (n_group-1)*cov(subset(data, data[,l]==i)[,-l])
  }
  return(SSA)
}
SSA <- SSA_func(gp.data)

Lambda <- det(SSA)/det(SST)
f.stat <- (n-p-2)/p*(1-sqrt(Lambda))/sqrt(Lambda)
pval <- 1 - pf(f.stat, 2*p, 2*(n-p-2))
data.frame(Lambda=Lambda, f_stat=f.stat, pval=pval)

# Using the function manova()
l <- ncol(gp.data)
X <- as.matrix(gp.data[,-l])
fit <- manova(X ~ group)
summary(fit, test="Wilks")
