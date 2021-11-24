#-----------------------#
# title: Fisher判别分析 #
# Author: Hongwei Shi   #
# Date: 2021-11-24      #
#-----------------------#
rm(list = ls())

#-----------Fisher discriminant analysis------------#

## 准备数据, 分为训练集和测试集
library(tidyverse)
library(caret)
data("iris")
set.seed(1124)
training.samples <- iris$Species %>%
   createDataPartition(p=0.7, list=FALSE)
train.data <- iris[training.samples, ]
test.data <- iris[-training.samples, ]

## Normalize the data. Categorical variables are automatically ignored.
# Estimate pre-processing parameters
preproc.param <- train.data %>% 
   preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.trans <- preproc.param %>% predict(train.data)
test.trans <- preproc.param %>% predict(test.data)

## LDA based on R package MASS
library(MASS)
# Compute LDA, k=3, p=4, 投影方向的个数<=2
model <- lda(Species~., data=train.trans)
model

FDA_proj <- function(data, cls) {
   if (is.matrix(data) != TRUE) {data <- as.matrix(data)}
   cls <- as.factor(as.vector(cls))
   cls.num <- length(unique(cls))
   levels(cls) <- 1:cls.num
   p <- ncol(data)
   X.bar <- colMeans(data)
   mu <- matrix(NA, cls.num, p)
   for (j in 1:cls.num) {
      Xj <- data[cls==j,]
      mu[j,] <- colMeans(Xj)
   }
   SB <- matrix(0, p, p)
   for (j in 1:cls.num) {
      nj <- nrow(data[cls==j,])
      SB <- SB + (mu[j,]-X.bar)%*%t(mu[j,]-X.bar)*nj
   }
   
   SW <- matrix(0, p, p)
   for (j in 1:cls.num) {
      Xj <- data[cls==j,]
      nj <- nrow(Xj)
      SW.temp <- matrix(0, p, p)
      for (i in 1:nj) {
         SW.temp <- SW.temp + (as.numeric(Xj[i,])-mu[j,])%*%t(as.numeric(Xj[i,])-mu[j,])
      }
      SW <- SW + SW.temp
   }
   a.num <- min((cls.num-1), p)
   eig <- eigen(ginv(SW)%*%SB)
   a <- eig$vectors[,1:a.num] # p*(k-1)
   a <- Re(a) # 求一个复数的实部
   if (is.vector(a) == TRUE) {a <- as.matrix(a)}
   colnames(a) <- paste0("LDA", 1:a.num)
   return(a)
}

## 2个变量, 2类数据
df <- iris[iris$Species!="virginica",]
a <- FDA_proj(data=df[,3:4], cls=df$Species)
proj <- (as.matrix(df[,3:4])%*%a)%*%t(a)/as.numeric(t(a)%*%a)
plt.df <- data.frame(df[,c(3:4,5)], proj)
ggplot(plt.df, aes(Petal.Length, Petal.Width)) +
   geom_point(aes(color=Species)) +
   geom_point(aes(X1, X2, color=Species)) +
   theme_bw()


## 2个变量, 3类数据
a <- FDA_proj(iris[,3:4], iris$Species)
# sum(a[,1]^2)
proj <- (as.matrix(iris[,3:4])%*%a[,1])%*%t(a[,1])/as.numeric(t(a[,1])%*%a[,1])
# proj1 <- (as.matrix(iris[,3:4])%*%a[,1])*a[1,1]/as.numeric(t(a[,1])%*%a[,1])
# proj2 <- (as.matrix(iris[,3:4])%*%a[,1])*a[2,1]/as.numeric(t(a[,1])%*%a[,1])
plt.df <- data.frame(iris[,c(3:4, 5)], proj)
ggplot(plt.df, aes(Petal.Length, Petal.Width)) +
   geom_point(aes(color=Species)) +
   geom_point(aes(X1, X2, color=Species)) +
   theme_bw()

## 4个变量, 3类数据, 降为2维
a <- FDA_proj(iris[,1:4], iris$Species)
# sum(a[,1]^2)
proj <- as.matrix(iris[,1:4])%*%a
plt.df <- data.frame(iris[,c(1:4, 5)], proj)
ggplot(plt.df) +
   geom_point(aes(LDA1, LDA2, color=Species)) +
   theme_bw()

## 对比lda得到的scaling和自定义函数FDA_proj()得到的投影方向
model <- lda(Species~., data=train.data)
a.scale <- model$scaling
a <- FDA_proj(data=train.data[,1:4], cls=train.data$Species)
a.scale[,1]/sqrt(sum(a.scale[,1]^2)); a[,1]
a/a.scale

## 解释lda得到的结果
model <- lda(Species~., data=train.data)
model

# FDA_proj <- function(data, cls) {
   data <- train.data[,1:4]; cls <- train.data$Species
   if (is.matrix(data) != TRUE) {data <- as.matrix(data)}
   cls <- as.factor(as.vector(cls))
   cls.num <- length(unique(cls))
   levels(cls) <- 1:cls.num
   p <- ncol(data)
   X.bar <- colMeans(data)
   mu <- matrix(NA, cls.num, p)
   for (j in 1:cls.num) {
      Xj <- data[cls==j,]
      mu[j,] <- colMeans(Xj)
   }
   SB <- matrix(0, p, p)
   for (j in 1:cls.num) {
      nj <- nrow(data[cls==j,])
      SB <- SB + (mu[j,]-X.bar)%*%t(mu[j,]-X.bar)*nj
   }
   
   SW <- matrix(0, p, p)
   for (j in 1:cls.num) {
      Xj <- data[cls==j,]
      nj <- nrow(Xj)
      SW.temp <- matrix(0, p, p)
      for (i in 1:nj) {
         SW.temp <- SW.temp + (as.numeric(Xj[i,])-mu[j,])%*%t(as.numeric(Xj[i,])-mu[j,])
      }
      SW <- SW + SW.temp
   }
   a.num <- min((cls.num-1), p)
   eig <- eigen(ginv(SW)%*%SB)
   a <- eig$vectors[,1:a.num] # p*(k-1)
   a <- Re(a)
   eig$values/sum(eig$values)
   # if (is.vector(a) == TRUE) {a <- as.matrix(a)}
   # colnames(a) <- paste0("LDA", 1:a.num)
   # return(a)
# }

model <- lda(Species~., data=train.trans)
# Make predictions
pred <- model %>% predict(test.trans)
# Model accuracy
mean(pred$class==test.trans$Species)
# the location of the misclassification
loc <- pred$class!=test.trans$Species 
cbind(test.data[loc,], pred_Species=pred$class[loc])

lda.data <- cbind(train.trans, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
   geom_point(aes(color = Species)) +
   theme_bw()

## 对比自定义FDA_proj()的结果
a <- FDA_proj(train.trans[,1:4], train.trans$Species)
proj <- as.matrix(train.trans[,1:4])%*%(-a) # 这里的负号只是为了和lda输出的方向完全一致
plt.df <- data.frame(train.trans[,c(1:4, 5)], proj)
ggplot(plt.df) +
   geom_point(aes(LDA1, LDA2, color=Species)) +
   theme_bw()

## 一些结果可视化的工具
lda.model <- lda(Species~., data=train.trans)
plot(lda.model, abbrev=T, col=as.numeric(train.trans$Species))


library(klaR)
partimat(Species~., data=train.trans, method="lda") 
partimat(Species~., data=train.trans, method = "lda", 
         plot.matrix=TRUE, imageplot = FALSE)

lda.pred <- lda.model %>% predict(test.trans)
ldahist(data=lda.pred$x[,1], g=test.trans$Species)

## 绘制判别函数的决策线
# 参考: 公众号bnulgr, 判别分析(R程序)


#--------LDA的扩展及对比--------#

## QDA: Quadratic discriminant analysis, 不需要假设协方差相等
library(MASS)
# Fit the model
qda.model <- qda(Species~., data=train.trans)
qda.model
# Make predictions
qda.pred <- qda.model %>% predict(test.trans)
# Model accuracy
mean(qda.pred$class == test.trans$Species)

## MDA: Mixture discriminant analysis
library(mda)
# Fit the model
mda.model <- mda(Species~., data=train.trans)
mda.model
# Make predictions
mda.pred <- mda.model %>% predict(test.trans)
# Model accuracy
mean(mda.pred == test.trans$Species)

## Comparison of LDA, QDA, and MDA 
# ref: https://www.r-bloggers.com/2013/07/a-brief-look-at-mixture-discriminant-analysis/
library(MASS)
library(mvtnorm)
library(mda)
library(ggplot2)
n <- 300
x11 <- rmvnorm(n=n, mean=c(-5, -5))
x12 <- rmvnorm(n=n, mean=c(0, 5))
x13 <- rmvnorm(n=n, mean=c(5, -5))
x21 <- rmvnorm(n=n, mean=c(-5,4))
x22 <- rmvnorm(n=n, mean=c(5, 5)) 
x23 <- rmvnorm(n=n, mean=c(0, 0))
x31 <- rmvnorm(n=n, mean=c(-5, 0))
x32 <- rmvnorm(n=n, mean=c(0, -4))
x33 <- rmvnorm(n=n, mean=c(4, 0))

x <- rbind(x11, x12, x13, x21, x22, x23, x31, x32, x33)
train.data <- data.frame(x, y=gl(3, 3*n))

# 训练分类器
lda.out <- lda(y~., data=train.data)
qda.out <- qda(y~., data=train.data)
mda.out <- mda(y~., data=train.data)

contour.data <- expand.grid(X1=seq(-8, 8, length=300), X2=seq(-8, 8, length=300))
# expand.grid(1:5, 3:7)
lda.pred <- data.frame(contour.data, y=as.numeric(predict(lda.out, contour.data)$class))
qda.pred <- data.frame(contour.data, y=as.numeric(predict(qda.out, contour.data)$class))
mda.pred <- data.frame(contour.data, y=as.numeric(predict(mda.out, contour.data)))

p <- ggplot(train.data, aes(x=X1, y=X2, color=y)) +
   geom_point() +
   theme_bw()
p + stat_contour(aes(x=X1, y=X2, z=y), data=lda.pred) +
   ggtitle("LDA Decision Boundaries") 
p + stat_contour(aes(x=X1, y=X2, z=y), data=qda.pred) +
   ggtitle("QDA Decision Boundaries") 
p + stat_contour(aes(x=X1, y=X2, z=y), data=mda.pred) +
   ggtitle("MDA Decision Boundaries")
ggplot(mda.pred, aes(x=X1, y=X2, color=as.factor(y))) +
   geom_point() +
   theme_bw()


## 更多改进的FDA package
# pfda: Probabilistic Fisher Discriminant Analysis
# lfda: Local Fisher Discriminant Analysis
   

#---------------------------------#
# title: KNN & Naive Bayes        #
# Author: Hongwei Shi, Wenjun Xia #
# Date: 2021-11-24                #
#---------------------------------#
rm(list = ls())

#-----------KNN------------#
library(class)

## 读取数据
mydata <- read.table("~/Desktop/2021秋季助教_多元/上机课/lec11//wdbc.data", sep=",", header=FALSE)

## 数据基本认识
head(mydata)
# 第一列为编号, 删除
mydata <- mydata[,-1]
str(mydata)
summary(mydata)

# 数据归一化
normalize <- function(x){
   return((x-min(x))/(max(x)-min(x)))
}
X <- data.frame(sapply(mydata[,2:ncol(mydata)], normalize))
Y <- mydata[,1]
table(Y) # 数据较为均衡
newmydata <- cbind(Y, X)


# 划分训练集、验证集和测试集
index <- sample(x=1:3, size=nrow(newmydata), replace=TRUE, prob=c(0.6, 0.2, 0.2))
train <- newmydata[index == 1,]
test <- newmydata[index == 2,]
valid <- newmydata[index == 3,]

?knn()
# 用验证集确定k个数
for (i in 1:round(sqrt(nrow(train)))){
   model <- knn(train=train[,-1], test=valid[,-1], cl=train$Y, k=i)
   Freq <- table(valid[,1], model)
   cat("k=", i, "\t", "error:", 1-sum(diag(Freq))/sum(Freq), "\n")
}

# choose k=6, 对应的分类误差率最低, 为0.01785714, 故取k=6

fit <- knn(train=train[,-1], test=test[,-1], cl=train$Y, k=5)

# 查看模型预测与实际类别的列联表
Freq <- table(test[,1], fit)
sum(diag(Freq))/sum(Freq)

## KNN 的维度灾难
# 维度增大以后 数据变得稀疏, 且集中在超球体的边缘, 导致距离的判断变难
library(MASS)
determinK <- function(train, valid){
   min <- 1
   minindex <- 1
   for (i in 1:round(sqrt(nrow(train)))){
      model <- knn(train=train[,-1], test=valid[,-1], cl=train[,1], k=i)
      Freq <- table(valid[,1], model)
      currerror <- 1-sum(diag(Freq))/sum(Freq)
      if (currerror < min){
         minindex <- i
         min <- currerror
      }
   }
   return(minindex)
}

dimension <- rep(1:50)*10
result <- c()
for (i in 1:length(dimension)) {
   n <- 1000; p <- dimension[i]
   mu <- rep(0, p); Sigma <- diag(p)
   set.seed(i)
   X <- mvrnorm(n, mu, Sigma)
   Y <- X %*% rnorm(p)
   Y <- exp(Y)/(1+exp(Y))
   Y <- ifelse(Y >= 0.5, 1, 0)
   
   data <- data.frame(cbind(y=as.factor(Y), X))
   
   index <- sample(x=1:3, size=nrow(X), replace=TRUE, prob=c(0.6, 0.2, 0.2))
   train <- data[index == 1, ]
   test <- data[index == 2, ]
   valid <- data[index == 3, ]
   
   K <- determinK(train, valid)
   fit <- knn(train=train[,-1], test=test[,-1], cl=train[,1], k=K)
   Freq <- table(test[,1], fit)
   
   result[i] <- sum(diag(Freq))/sum(Freq)
   cat("p=", p, "accuracy", sum(diag(Freq))/sum(Freq), "\n")
}

plot(x=dimension, y=result, type="o", 
     ylim=c(0.5, 0.9), ylab="accuracy")
abline(h=0.5, col="red")

#-----------Naive bayes------------#

#----------------------最常见应用场景----------------------#
# 文本分类/垃圾文本过滤/情感判别:这大概会朴素贝叶斯应用做多的地方了, 
# 即使在现在这种分类器层出不穷的年代, 在文本分类场景中, 朴素贝叶斯依旧
# 坚挺地占据着一席之地.原因嘛, 大家知道的, 因为多分类很简单, 同时在
# 文本数据中, 分布独立这个假设基本是成立的.而垃圾文本过滤(比如垃圾邮件识别)
# 和情感分析(微博上的褒贬情绪)用朴素贝叶斯也通常能取得很好的效果.

library(tm)
library(e1071)
library(class)
library(wordcloud2)

mydata <- read.csv("~/Desktop/2021秋季助教_多元/上机课/lec11/spamdata_T2.csv", header=FALSE)
head(mydata)

names(mydata) <- c("label", "contents")
table(mydata$label)
doc <- mydata$contents

## 构建语料库
doc_corpus <- Corpus(VectorSource(doc))
doc_corpus[["1"]]
doc_corpus[["1"]]$content
doc_corpus[["1"]]$meta

# 构建词频矩阵
# 确定分词标准, 可以有自己的停用词库等
control <- list(stopwords=TRUE, removePunctuation=TRUE, 
                removeNumbers=TRUE, minDocFreq=1)
doc_tdm <- TermDocumentMatrix(doc_corpus, control)
word_matrix <- t(as.matrix(doc_tdm)) # 词文本矩阵, 即这个词在某个文本中出现的次数 
ncol(word_matrix); nrow(word_matrix)
word_matrix[1:3,1:6]

## 绘制词云
wordCloudPlot<- function(word_matrix){
   word_counts <- colSums(word_matrix) #每一列和, 即单词在所有文本中出现的总次数
   word_counts <- data.frame(cbind(names(word_counts), as.numeric(word_counts)))
   names(word_counts) <- c("word", "freq")
   word_counts[,2] <- as.numeric(word_counts[,2])
   # head(word_counts)
   
   for (i in 1:nrow(word_counts)) {
      word_counts[i,1] <- iconv(word_counts[i,1], "UTF-8", "UTF-8", sub="") # replace any non UTF-8 by ""
   }
   wordcloud2(word_counts, minRotation=-pi/2, maxRotation=-pi/2)  # 词云
}


?wordcloud2
# data: 包含每列中的word和freq的数据帧, 按照word出现的顺序由内向外画图(可以按照freq降序美化wordcloud).
# size: 字体大小, 默认为1.较大的大小意味着较大的单词.
# fontFamily: 要使用的字体.
# fontWeight: 字体重量, 例如normal, bold or 600
# color: 文本的颜色, 可以使用关键字random-dark和random-light.也支持颜色矢量.
# minSize: 字幕的字符串
# backgroundColor: 背景的颜色.
# gridSize: 用于标记画布可用性的网格大小, 网格大小越大, 单词之间的差距越大.
# minRotation: 文本应该旋转的最小旋转(以rad为单位).
# maxRotation: 文本应旋转的最大旋转(以rad为单位).
# rotateRatio: 单词旋转的概率.将数字设置为1以始终旋转.
# shape: 绘制"云"的形状. "circle" (default), "cardioid" (心形", 苹果或心形曲线, 最知名的极坐标方程), "diamond" (菱形), 
#        "triangle-forward"(三角形前移), "triangle"(三角形), "pentagon"(五角形), and "star"
# ellipticity: 平坦度
# figPath: 画布路径

# 绘制总的词云
wordCloudPlot(word_matrix)

# 看一下正常短信和垃圾短信的词频有没有什么区别
spam_index <- (mydata$label == "spam")
word_matrix_spam <- word_matrix[spam_index, ]
word_matrix_ham <- word_matrix[-spam_index, ]

wordCloudPlot(word_matrix_spam)
wordCloudPlot(word_matrix_ham)

# 有的短信文本所有词频数均为零, 直接删除 (有可能会有的bug)
noZeroIndex <- (rowSums(word_matrix) != 0)
word_matrix <- word_matrix[noZeroIndex, ]
nrow(word_matrix); ncol(word_matrix)

## 特征筛选
# 特征工程
# 根据相关系数筛选重要的特征
data_words_count <- cbind("label"=mydata$label[noZeroIndex], 
                          data.frame(word_matrix))
mydata[1:3,]
data_words_count[1:3,1:10]
Y <- data_words_count$label
Y <- ifelse(Y=="ham", 0, 1)

SIS <- abs(apply(data_words_count[,2:ncol(data_words_count)], 2, function(x){return(cor(x,Y))}))

# 按相关系数大小降序排列, 取前50大的特征
good_word <- sort(SIS, decreasing=TRUE)[1:50] 
data.frame(good_word)

newmydata <- data_words_count[,names(good_word)]
newmydata <- cbind("label"=as.factor(data_words_count$label), newmydata)

table(newmydata$label) #数据不平衡, 很多分类算法都失效

## 建立模型
library(caret)
set.seed(1124)
training.samples <- newmydata$label %>%
   createDataPartition(p=0.7, list=FALSE)
train <- newmydata[training.samples, ]
test <- newmydata[-training.samples, ]

# 朴素贝叶斯
?naiveBayes
naiveBayes.model <- naiveBayes(label~., data=train)
train_predict <- naiveBayes.model %>% predict(train)
train_table <- table(actual=train$label, predict=train_predict)

test_predict <- naiveBayes.model %>% predict(test)
test_table <- table(actual=test$label, predict=test_predict)

cat("train accuracy:", sum(diag(train_table)/sum(train_table)), "\n",
    "test accuracy:", sum(diag(test_table)/sum(test_table)))

# 精确率和召回率
test_table
cat("precision:", test_table[2,2]/(test_table[1,2]+test_table[2,2]), "\n", # 针对预测结果,在所有预测结果为spam的样本中, 预测正确的概率有多少
    "recall:", test_table[2,2]/(test_table[2,1]+test_table[2,2])) # 针对原始样本, 在真实为spam的样本中, 预测正确的概率有多少
# 虽然准确率很高, 但是precision和recall都很低

## R package klaR
library(klaR)
# Build the model, k-fold cross-validation
naiveBayes.model <- train(label~., data=train, method="nb", 
                          trControl=trainControl("cv", number = 10))
# Make predictions
pred.test <- naiveBayes.model %>% predict(test)
# Model n accuracy
mean(pred.test == test$label)

## KNN 失效
knn(train=train[,-1], test=test[,-1], cl=train[,1], k=10, use.all=FALSE)


## How about tf-idf
TF <- word_matrix/rowSums(word_matrix)
IDF <- apply(word_matrix, 2, function(x){return((log(nrow(word_matrix)/length(x[x!=0]))))})
TF_IDF <- t(apply(TF, 1, function(x){return(x*IDF)}))

data_words_TFIDF <- cbind("label"=mydata$label[noZeroIndex], 
                          data.frame(TF_IDF))
SIS <- abs(apply(data_words_TFIDF[,2:ncol(data_words_TFIDF)], 2, function(x){return(cor(x,Y))}))

good_word <- sort(SIS, decreasing=TRUE)[1:50]

newmydata <- data_words_TFIDF[,names(good_word)]
newmydata <- cbind("label"=as.factor(data_words_TFIDF$label), newmydata)

## 建立模型
library(caret)
set.seed(1124)
training.samples <- newmydata$label %>%
   createDataPartition(p=0.7, list=FALSE)
train <- newmydata[training.samples, ]
test <- newmydata[-training.samples, ]

# 朴素贝叶斯
naiveBayes.model <- naiveBayes(label~., data=train)
train_predict <- naiveBayes.model %>% predict(train)
train_table <- table(actual=train$label, predict=train_predict)

test_predict <- naiveBayes.model %>% predict(test)
test_table <- table(actual=test$label, predict=test_predict)

cat("train accuracy:", sum(diag(train_table)/sum(train_table)), "\n",
    "test accuracy:", sum(diag(test_table)/sum(test_table)))

# 精确率和召回率
test_table
cat("precision:", test_table[2,2]/(test_table[1,2]+test_table[2,2]), "\n", # 针对预测结果,在所有预测结果为spam的样本中, 预测正确的概率有多少
    "recall:", test_table[2,2]/(test_table[2,1]+test_table[2,2])) # 针对原始样本, 在真实为spam的样本中, 预测正确的概率有多少

# 结果更差了, 但这是一种很常见的文本数据处理方法
# 其他可以尝试更好的词向量构建 word2vec, autoencoder等等

## 参考
# https://blog.csdn.net/clebeg/article/details/41015185
# https://rstudio-pubs-static.s3.amazonaws.com/479612_8cda7868fd31497796121d445ed15db8.html
