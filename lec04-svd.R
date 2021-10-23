rm(list = ls())
require(corpcor) # fast svd for small n, big p


svd_func <- function(A) {
  eig.val <- eigen(t(A)%*%A)$values
  index <- (eig.val > 10e-5)
  eig.val <- eig.val[index]
  d <- sqrt(eig.val)
  v <- eigen(t(A)%*%A)$vectors[,index]
  u <- matrix(NA, nrow(A), length(eig.val))
  for (i in 1:ncol(v)) { u[,i] <- (A%*%v[,i])/d[i] }
  return(list(d=d, u=u, v=v))
}

n <- 3; p <- 10
A <- matrix(sample(1:(10*n*p), n*p), n, p)
svd(A)
svd_func(A)
fast.svd(A) # require(corpcor)

## svd在图像压缩中的应用
rm(list = ls())
library(RSpectra)
library(jpeg)
library(animation)
library(corpcor)

# 对三个图层分别实现奇异值分解
svd_message <- function(pic) {
  r <- fast.svd(pic[,,1])  # 提取r层所有像素点
  g <- fast.svd(pic[,,2])  # 提取g层所有像素点
  b <- fast.svd(pic[,,3])  # 提取b层所有像素点
  return(list(r=r, g=g, b=b))
}

# 截取前m个奇异值对应的空间代表原空间
trunc_pic <- function(pic, m, is.plot=TRUE) {  
  pic <- svd_message(pic)
  # 将前m个奇异值对应的空间进行叠加，即Unm %*% Dmm %*% Vmp
  r <- pic$r$u[,1:m] %*% diag(pic$r$d[1:m]) %*% t(pic$r$v[,1:m])
  g <- pic$g$u[,1:m] %*% diag(pic$g$d[1:m]) %*% t(pic$g$v[,1:m])
  b <- pic$b$u[,1:m] %*% diag(pic$b$d[1:m]) %*% t(pic$b$v[,1:m])
  
  # 将新的rgb层重新组合为图像
  comp.pic <- array(0, c(nrow(r), ncol(r), 3))  
  comp.pic[,,1] <- r
  comp.pic[,,2] <- g
  comp.pic[,,3] <- b
  writeJPEG(comp.pic, paste0(round(100*m/length(pic$r$d), 2), "%comp_pic.png"))
  
  if(is.plot == TRUE) {
    require(ggplot2)
    require(patchwork)
    require(tidyverse)
    
    # 绘制次序奇异值大小-大小变化
    p1 <- data.frame(x=1:length(pic$r$d), y=pic$r$d) %>%
      ggplot(aes(x=x, y=y)) +
      geom_point(size=0.5) +
      labs(x="m", y="singular value", title="Singular Values") +
      theme_bw() +
      theme(plot.title=element_text(family="Arial-Black", size=rel(1), hjust=0.5),
            axis.title=element_text(size=rel(1)))

    # 绘制部分奇异值对累计奇异值的占比-解释能力
    p2 <- data.frame(x=1:length(pic$r$d), y=cumsum(pic$r$d)/sum(pic$r$d)) %>%
      ggplot(aes(x=x, y=y)) +
      geom_point(size=0.5) +
      labs(x="m", y="cumulative percent", title="Cumulative Percent of Total Sigmas") +
      theme_bw() +
      theme(plot.title=element_text(family="Arial-Black", size=rel(1), hjust=0.5),
            axis.title=element_text(size=rel(1)))
    p1 + p2
    
    # 尝试将这两个值展示在一张图上
  }
}


# 读取图片信息
mypic <- readJPEG("~/Desktop/2021秋季助教_多元/上机课/lec04/pic_demo.jpeg")
str(mypic)
# trunc_pic(mypic, m=30, is.plot=TRUE)
len <- length(svd_message(mypic)$r$d)

# 输出压缩后的照片
setwd("~/Desktop/2021秋季助教_多元/上机课/lec04/compress_demo/")
m.vec <- seq(10, len, length.out=5)
for (m in m.vec) {
  png()
  trunc_pic(mypic, m, is.plot=FALSE)
}
dev.off()

# gif动图
bm.files <- paste0(round(100*m.vec/len, 2), "%comp_pic.png")
# Please install ImageMagick first or put its bin path into the system PATH variable
im.convert(files=bm.files, output="Compresspics.gif")
