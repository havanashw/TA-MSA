#----------------------------------#
# title: logistic regression model #
# Author: Hongwei Shi & Wenjun Xia #
# Date: 2021-11-10                 #
#----------------------------------#
rm(list = ls())
library(nnet)
winedata <- read.csv("/Users/shihongwei/Desktop/2021秋季助教_多元/上机课/lec09/logistic/wine_data.csv")
mydata <- winedata
mydata$type <- ifelse(mydata$type=="red", 1, 0)

## logistic 拟合
# 选取训练集和测试集
library(caret)
set.seed(1110)
Index <- sample(x=1:2, size=nrow(mydata), replace=TRUE, prob=c(0.7, 0.3))
train <- mydata[Index==1, ]
test <- mydata[Index==2, ]
table(train$type)

# Index <- createDataPartition(mydata$type, p=0.7, list=FALSE)  # 70% training data
# train <- mydata[Index, ]
# table(train$type)
# test <- mydata[-Index, ]
# table(test$type)
# 
# # 选择良性训练集
# # Down Sample
# set.seed(100)
# down.train <- downSample(x=subset(train, select=-c(type)),
#                          y=as.factor(train$type))
# table(down.train$Class)
# 
# # Up Sample.
# set.seed(100)
# up.train <- upSample(x=subset(train, select=-c(type)),
#                      y=as.factor(train$type))
# 
# table(up.train$Class)

logistic.model <- glm(type~., data=train, family=binomial(link="logit"))
train.predict <- predict(logistic.model, train, type="response")
train.predict <- ifelse(train.predict>0.5, 1, 0)
train.table <- table(actual=train$type, pre=train.predict)

test.predict <- predict(logistic.model, test, type="response")
test.predict <- ifelse(test.predict>0.5, 1, 0)
test.table <- table(actual=test$type, pre=test.predict)

cat("train accuracy:", sum(diag(train.table)/sum(train.table)), "\n", 
    "test accuracy:", sum(diag(test.table)/sum(test.table)))


## 手动写一下logistic
# 梯度下降法
logisticGradientDescent <- function(input_data, maxIterion, epslion, learningRate){
  # input_data最后一列为标签
  eita <- learningRate
  p <- ncol(input_data); n <- nrow(input_data)
  
  # 添加截距项
  X <- input_data[,-p]
  intercpt <- rep(1, n)
  
  X <- as.matrix(cbind(intercpt, X))
  Y <- input_data[,p]
  
  # 初始权重
  w <- rep(0, p)
  k <- 0

  repeat{
    # print(k)
    # 计算概率
    px <- exp(X%*%w)/(1+exp(X%*%w))
    # 计算梯度
    gradient <- (t(X)%*%(px-Y))/n
    
    # 权重更新
    w <- w-eita*gradient
    k <- k+1
    
    if(k > maxIterion){
      cat("I have reached my maxiterions, and still not converge")
      break
    }
    if(max(abs(eita*gradient)) < epslion){
      cat("After", k, "iterions, I have been converged!")
      break
    }
    cat(k, "\r")
  }
  return(w)
}

w <- logisticGradientDescent(input_data=train, maxIterion=1e5, epslion=1e-4, learningRate=0.001)

# 评估准确率
X_train<- as.matrix(cbind(rep(1, nrow(train)), subset(train, select=-c(type))))
train.predict <- exp(X_train%*%w)/(1+exp(X_train%*%w))
train.predict <- ifelse(train.predict>0.5, 1, 0)
train.table <- table(train.predict, train$type)
sum(diag(train.table))/sum(train.table)

X_test <- as.matrix(cbind(rep(1, nrow(test)), subset(test, select=-c(type))))
test.predict <- exp(X_test%*%w)/(1+exp(X_test%*%w))
test.predict <- ifelse(test.predict>0.5, 1, 0)
test.table <- table(test.predict, test$type)
sum(diag(test.table))/sum(test.table)

cat("train accuracy:", sum(diag(train.table)/sum(train.table)), "\n", 
    "test accuracy:", sum(diag(test.table)/sum(test.table)))


## 牛顿迭代
logisticNewton <- function(input_data, maxIterion, epslion){
  p <- ncol(input_data); n <- nrow(input_data)
  
  # 添加截距项
  X <- input_data[,-p]
  intercpt <- rep(1, n)
  
  X <- as.matrix(cbind(intercpt, X))
  Y <- input_data[,p]
  
  # 初始权重
  w <- rep(0, p)
  k <- 0
  repeat{
    # print(k)
    # 计算概率
    px <- 1/(1+exp(-X%*%w))
    
    # 计算二阶导
    gra1 <- px*(1-px)
    gra2 <- apply(X, 2, function(x){x*gra1})
    Hessian <- (t(X) %*% gra2)/n
    gradient <- (t(X) %*% (px-Y))/n
    
    # 计算变动值
    change <- solve(Hessian)%*%gradient # ginv()
    
    # 更新权重
    w <- w - change
    k <- k+1
    
    if(k > maxIterion){
      cat("I have reached my maxiterions, and still not converge")
      break
    }
    if(max(abs(change)) < epslion){
      cat("After", k, "iterions, I have been converged!")
      break
    }
    cat(k, "\r")
  }
  return(w)
}

w <- logisticNewton(input_data=train, maxIterion=1e5, epslion=1e-4)

# 和使用库函数得到的结果完全一致
w
logistic.model$coefficients

## 多分类logit回归
data(iris)
# set.seed(1)
Index <- sample(x=1:2, size=nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
train <- iris[Index==1, ]
test <- iris[Index==2, ]
multilogistic.model <- multinom(Species~., data=train)

train.predict <- predict(multilogistic.model, train, type='class')
train.table <- table(actual=train$Species, pre=train.predict)

test.predict <- predict(multilogistic.model, test, type='class')
test.table <- table(actual=test$Species, pre=test.predict)

cat("train accuracy:", sum(diag(train.table)/sum(train.table)), "\n", 
    "test accuracy:", sum(diag(test.table)/sum(test.table)))

rm(list = ls())
require(optimx)
require(numDeriv)
require(dplyr)
winedata <- read.csv("/Users/shihongwei/Desktop/2021秋季助教_多元/上机课/lec09/wine_data.csv")
mydata <- winedata
mydata$type <- ifelse(mydata$type=="red", 1, 0)



# Define -log-likelihood function for logistic regression model
negll <- function(w){
  p <- ncol(input_data); n <- nrow(input_data)

  # 添加截距项
  X <- input_data[,-p]
  intercpt <- rep(1, n)

  X <- as.matrix(cbind(intercpt, X))
  Y <- input_data[,p]

  px <- 1/(1+exp(-X%*%w))

  #Set high values for 0 < px < 1
  if(any(px > 1) | any(px < 0)) {
    nlog.like <- 1e+200
  } else {
    nlog.like <- -mean(Y*log(px)+(1-Y)*log(1-px))
  }
  return(nlog.like)
}

negll.grad <- function(w){
  p <- ncol(input_data); n <- nrow(input_data)

  # 添加截距项
  X <- input_data[,-p]
  intercpt <- rep(1, n)

  X <- as.matrix(cbind(intercpt, X))
  Y <- input_data[,p]

  px <- 1/(1+exp(-X%*%w))
  gradient <- (t(X)%*%(px-Y))/n
  return(gradient)
}

negll.grad2 <- function(w){
  p <- ncol(input_data); n <- nrow(input_data)
  
  # 添加截距项
  X <- input_data[,-p]
  intercpt <- rep(1, n)
  
  X <- as.matrix(cbind(intercpt, X))
  Y <- input_data[,p]
  
  px <- 1/(1+exp(-X%*%w))
  gra1 <- px*(1-px)
  gra2 <- apply(X, 2, function(x){x*gra1})
  Hessian <- (t(X) %*% gra2)/n
  return(Hessian)
}

input_data <- mydata
p <- ncol(input_data)

negll.grad(w=rep(0, p))
grad(func=negll, x=rep(0, p))
negll.grad2(w=rep(0, p))

opt <- optimx(par=rep(0, p),
              fn=negll,
              gr=negll.grad,
              hess=negll.grad2,
              control=list(trace=0, all.methods=TRUE))

# print results of optimization
res.allopt <- summary(opt, order="convcode") %>%
  select(-c(value, niter, gevals, fevals))

# 评估不同优化方法的准确率
pred_acc <- function(data, w) {
  X.data <- as.matrix(cbind(rep(1, nrow(data)), subset(data, select=-c(type))))
  data.predict <- exp(X.data%*%w)/(1+exp(X.data%*%w))
  data.predict <- ifelse(data.predict>0.5, 1, 0)
  data.table <- table(data.predict, data$type)
  return(sum(diag(data.table))/sum(data.table))
}

input_data <- mydata; p <- ncol(input_data)
res <- res.allopt[,1:p]
for (i in 1:nrow(res.allopt)) {
  res$accurancy[i] <- pred_acc(input_data, as.numeric(res[i, 1:p]))
}

