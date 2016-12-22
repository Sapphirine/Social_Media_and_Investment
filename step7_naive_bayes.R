library('sparklyr')
sc <- spark_connect(master = "local")
spark <- spark_session(sc)

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = "/Users/Van/spark")
}
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
sparkR.session(master = "local[*]", sparkConfig = list(spark.driver.memory = "2g"))


test_data=read.csv('/Users/Van/Desktop/test_M1.csv',sep=',',header=F)
data<-as.data.frame(test_data)
data<-data[1:1000,]
colnames(data)=c('Y','X1','X2','X3')
data$Y=as.factor(data$Y)
data$X1=as.factor(data$X1)
data$X2=as.factor(data$X2)
data$X3=as.factor(data$X3)
dataDF<-createDataFrame(data)
model <- spark.naiveBayes(dataDF, Y ~ X1 + X3 + X2, smoothing = 0)

data<-as.data.frame(test_data)
data<-data[1000:1100,]
colnames(data)=c('Y','X1','X2','X3')
data$Y=as.factor(data$Y)
data$X1=as.factor(data$X1)
data$X2=as.factor(data$X2)
data$X3=as.factor(data$X3)
dataDF<-createDataFrame(data)

pred<-predict(model,dataDF)
# summary(model)
# path<-'/Users/Van/Desktop/model'
# write.ml(model,path)
# savedModel<-read.ml(path)
# summary(savedModel)
showDF(pred["prediction"])
a=as.data.frame(select(pred,'prediction'))
summary(a[,1]==data[,1])

