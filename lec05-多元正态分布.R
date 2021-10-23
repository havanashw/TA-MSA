#-----------------------#
# title: 多元正态分布     #
# Author: Hongwei Shi   #
# Date: 2021-10-13      #
#-----------------------#

rm(list = ls())
setwd("~/Desktop/2021秋季助教_多元/上机课/lec05")

## 二元正态分布的密度函数和等高线
# 参考1: http://www.ejwagenmakers.com/misc/Plotting_3d_in_R.pdf
# 参考2: https://bookdown.org/xiangyun/msg/gallery.html#sec:persp

## 密度函数
require(mvtnorm)
require(latex2exp)
rho <- 0.5; mu <- c(0, 0); 
s11 <- 1; s22 <- 1
Sigma <- matrix(c(s11, rho*sqrt(s11*s22), rho*sqrt(s11*s22), s22), 2)
x1 <- seq(-10, 10, length=41); x2 <- x1
f1 <- function(x1, x2) {
  dmvnorm(cbind(x1, x2), mu, Sigma) # Rpackage: mvtnorm
}

f_func <- function(x1, x2, mu=mu, s11=s11, s22=s22, rho=rho) {
  term1 <- 1/(2*pi * sqrt(s11*s22*(1-rho^2)))
  term2 <- -1/(2*(1-rho^2))
  term3 <- (x1-mu[1])^2 / s11
  term4 <- (x2-mu[2])^2 / s22
  term5 <- 2*rho * ((x1-mu[1])*(x2-mu[2])) / (sqrt(s11)*sqrt(s22))
  return(term1*exp(term2 * (term3+term4-term5)))
}

# f1(2,1)

f <- outer(x1, x2, function(x, y)f_func(x, y, mu=mu, s11=s11, s22=s22, rho=rho))
# f <- f_func(x1, x2)

persp(x1, x2, f, 
      col="lightgreen", 
      theta=30, # theta参数给出了主要方向，控制三维图的左右
      phi=30, # phi给出纬度
      r=50,
      d=0.1,
      expand=0.5, # expand 控制三维图的立体性
      ltheta=90, 
      lphi=180,
      shade=0.75,
      ticktype="detailed", # 坐标轴上刻度数字
      nticks=5, # 刻度间隔数目
      # xlab=TeX('$x_{1}$'), # latex2exp 其实是使用 LaTeX 语法将 LaTeX 公式翻译为 R 能接受的表达式形式
      # ylab=TeX('$x_{2}$'),
      # zlab=TeX('$f(x_{1},x_{2})$'),
      xlab="\n x1",
      ylab="\n x2",
      zlab="\n\n f(x1,x2)",
      main="Two dimensional Normal Distribution")

# 这种展示LaTex的方式过于复杂
mtext(expression(list(
  mu[1] == 0, mu[2] == 0, sigma[11] == 10,
  sigma[22] == 10, sigma[12] == 15, rho == 0.5
)), side = 3)

mtext(expression(italic(f)~group("(", list(x[1], x[2]), ")") ==
                 frac(1, 2~pi~sqrt(sigma[11]~sigma[22]~(1-rho^2)))~exp~
                 bgroup("{", paste(-frac(1, 2(1-rho^2))*phantom(0),
                   bgroup("[", frac((x[1]~-~mu[1])^2, sigma[11]) ~
                            -~2~rho~frac(x[1]~-~mu[1], sqrt(sigma[11])) ~
                            frac(x[2]~-~mu[2], sqrt(sigma[22])) ~
                            +~frac((x[2]~-~mu[2])^2, sigma[22]), "]")), "}")
                 ), side=1, line=3)

## LaTex friendly
# require(tikzDevice)
# dev.set()
# tikz("myplot.tex", width = 6, height = 4, pointsize = 30, standAlone = TRUE)
# persp(x1, x2, f, 
#       col="lightgreen", 
#       theta=30, # theta参数给出了主要方向，控制三维图的左右
#       phi=30, # phi给出纬度
#       r=50,
#       d=0.1,
#       expand=0.5, # expand 控制三维图的立体性
#       ltheta=90, 
#       lphi=180,
#       shade=0.75,
#       ticktype="detailed", # 坐标轴上刻度数字
#       nticks=5, # 刻度间隔数目
#       xlab="$x_{1}$",
#       ylab="$x_{2}$",
#       zlab="$f(x_{1},x_{2})$",
#       main="Two dimensional Normal Distribution")
# 
# mtext("$\\mu_1=0, \\mu_2=0, \\sigma_{11}=10, \\sigma_{22}=10, \\sigma_{12}=15, \\rho=0.5$", side=3)
# 
# mtext("$f(x_{1},x_{2})=\\frac{1}{2\\pi\\sqrt{\\sigma_{11}\\sigma_{22}(1-\\rho^2)}}\\exp
#       \\big\\{-\\frac{1}{2(1-\\rho^2)}[\\frac{(x_1 - \\mu_1)^2}{\\sigma_{11}} - 
#       2\\rho\\frac{(x_1 - \\mu_1)(x_2 - \\mu_2)}{\\sqrt{\\sigma_{11}}\\sqrt{\\sigma_{22}}} + 
#       \\frac{(x_2 - \\mu_2)^2}{\\sigma_{22}}]\\big\\}$",
#       side=1, line=2, cex=1.5)
# dev.off()

## 等高线
contour(x1, x2, f) # 等高线图

image(x1, x2, f) # 色阵图
contour(x1, x2, f, add=TRUE) # 等高线图
legend("topright", legend="rho=0.5")

## 3D图
# 参考1: https://bookdown.org/xiangyun/msg/system.html#sec:rgl
# 参考2: https://bookdown.org/xiangyun/r4ds/interactive-graphics.html
# persp() 函数可以绘制三维透视图，但其视角是固定的，无法进行交互
require(rgl)
persp3d(x1, x2, f, theta=45, phi=25, col='lightgreen')


## 不同的rho得到的密度函数
for(rho in seq(-0.9, 0.9, by=0.1)) {
  f <- outer(x1, x2, function(x1, x2)f_func(x1, x2, mu=mu, s11=s11, s22=s22, rho=rho))
  persp(x1, x2, f, 
        col="lightgreen", 
        theta=30, # theta参数给出了主要方向，控制三维图的左右
        phi=30, # phi给出纬度
        r=50,
        d=0.1,
        expand=0.5, # expand 控制三维图的立体性
        ltheta=90, 
        lphi=180,
        shade=0.75,
        main=paste("rho=", as.character(rho)))
  locator(1)
}

## 不同的s11
for(s11 in seq(1, 100, by=9)) {
  f <- outer(x1, x2, function(x1, x2)f_func(x1, x2, mu=mu, s11=s11, s22=s22, rho=rho))
  persp(x1, x2, f, 
        col="lightgreen", 
        theta=30, # theta参数给出了主要方向，控制三维图的左右
        phi=30, # phi给出纬度
        r=50,
        d=0.1,
        expand=0.5, # expand 控制三维图的立体性
        ltheta=90, 
        lphi=180,
        shade=0.75,
        main=paste("s11=",as.character(s11)))
  locator(1)
}

## Contour Plot
par(mfrow=c(2, 2))
f <- outer(x1, x2, function(x1, x2)f_func(x1, x2, mu=c(0, 0), 
                                          s11=10, s22=15, rho=0))
contour(f, main="independent")
f <- outer(x1, x2, function(x1, x2)f_func(x1, x2, mu=c(0, 0), 
                                          s11=10, s22=15, rho=0.5))
contour(f, main="s11=10, rho=0.5")
f <- outer(x1, x2, function(x1, x2)f_func(x1, x2, mu=c(0, 0), 
                                          s11=30, s22=15, rho=0.5))
contour(f, main="s11=30, rho=0.5")
f <- outer(x1, x2, function(x1, x2)f_func(x1, x2, mu=c(0, 0), 
                                          s11=30, s22=15, rho=-0.9))
contour(f, main="s11=30, rho=-0.9")

## Simulate a bivariate normal distribution
# method1
require(MASS)
require(mvtnorm)
mu <- c(1, 2)
Sig<- matrix(c(1, 0.5, 0.5, 1), 2)
n <- 1000
mv.data1 <- mvrnorm(n, mu, Sig) # MASS
# mv.data1 <- rmvnorm(n, mu, Sig) # mvtnorm

# method2: X=mu+Sig^{1/2}Y, Y~N(0,1)
Sig.eig <- eigen(Sig)
Sig.sqrt <- Sig.eig$vectors%*%diag(sqrt(Sig.eig$values))%*%t(Sig.eig$vectors)
mv.data2 <- mu + t(matrix(rnorm(2*n), ncol=2)%*%Sig.sqrt)
mv.data2 <- t(mv.data2)

par(mfrow=c(2, 2))
hist(mv.data1[,1],main="Histogram of X1")
hist(mv.data1[,2],main="Histogram of X2")

hist(mv.data2[,1],main="Histogram of X1")
hist(mv.data2[,2],main="Histogram of X2")

# 构造置信区间
require(mixtools)
par(mfrow=c(1,2))
plot(mv.data1, pch=16, cex=0.5, xlab="X1", ylab="X2", main="data1")
ellipse(mu=colMeans(mv.data2), sigma=cov(mv.data2), alpha=0.05, col="red")
ellipse(mu=colMeans(mv.data2), sigma=cov(mv.data2), alpha=0.01, col="blue")
points(t(mu), col='red', pch=19)

plot(mv.data2, pch=16, cex=0.5, xlab="X1", ylab="X2", main="data2")
ellipse(mu=colMeans(mv.data2), sigma=cov(mv.data2), alpha=0.05, col="red")
points(t(mu), col='red', pch=19)

## 绘制动图对比不同rho, mu, Sigma
require(animation)
mu <- c(0, 0)
Sig <- diag(2)

# mu1
saveHTML({ 
  for(i in 1:30){
    mu[1] <- mu[1] + 0.1
    x1 <- x2 <- seq(-10, 10, length=40)
    # f <- matrix(apply(expand.grid(x1, x2), 1, function(mv.data){dmvnorm(mv.data, mu, Sig)}), nrow=40, byrow=T)
    f <- outer(x1, x2, function(x1, x2)f_func(x1, x2, mu=mu, s11=Sig[1,1], s22=Sig[2,2], 
                                              rho=Sig[1,2]/sqrt(Sig[1,1]*Sig[2,2])))
    persp(x1, x2, f, 
          col="lightgreen", 
          theta=30, # theta参数给出了主要方向，控制三维图的左右
          phi=30, # phi给出纬度
          r=50,
          d=0.1,
          expand=0.5, # expand 控制三维图的立体性
          ltheta=90, 
          lphi=180,
          shade=0.75,
          main=paste("mu[1] =", round(mu[1], 3)))
  }
})

# mu1, Sigma22
mu <- c(0, 0)
Sig <- diag(2)
saveHTML({ 
  for(i in 1:30){
    mu[1] <- mu[1] + 0.1
    Sig[2,2] <- Sig[2,2] + 2
    x1 <- x2 <- seq(-10, 10, length=40)
    # f <- matrix(apply(expand.grid(x1, x2), 1, function(mv.data){dmvnorm(mv.data, mu, Sig)}), nrow=40, byrow=T)
    f <- outer(x1, x2, function(x1, x2)f_func(x1, x2, mu=mu, s11=Sig[1,1], s22=Sig[2,2], 
                                              rho=Sig[1,2]/sqrt(Sig[1,1]*Sig[2,2])))
    persp(x1, x2, f, 
          col="lightgreen", 
          theta=30, # theta参数给出了主要方向，控制三维图的左右
          phi=30, # phi给出纬度
          r=50,
          d=0.1,
          expand=0.5, # expand 控制三维图的立体性
          ltheta=90, 
          lphi=180,
          shade=0.75,
          main=paste(paste("mu[1] =", round(mu[1],3)), paste("Sig[2,2] =", round(Sig[2,2],3))))
  }
})

# rho, 大家自己完成
