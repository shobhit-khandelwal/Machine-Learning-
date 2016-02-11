#Installing the package e1071
install.packages("e1071")
require(e1071)

###############Question 3

#Installing the package "mlbench" and the breast cancer data from the package.
install.packages("mlbench")
require(mlbench)

data(BreastCancer, package="mlbench")

#Displaying some information about the data.
head(BreastCancer)
class(BreastCancer)
summary(BreastCancer)

#Removing the Id column as it does not help in analysis.
BreastCancer$Id<-NULL

#Removing the NA values in the data.
BreastCancer <- na.omit(BreastCancer)

#Counting the number of unique values in the column "Class" and the number of times these
#values appear in the data.
as.data.frame(table(BreastCancer[,11]))

#Converting categorical data to numerical.
#temp<-BreastCancer
#temp<-temp(-which(temp.Bare.nuclei=NA))
#temp$Class2<-rep(c(1),times=699)
#for(p in 1:699){if(temp[p,10]=="benign")temp[p,11]<-0}
#temp$Class<-NULL
#colnames(temp)[10]<-"Class"
#BreastCancer<-temp

#p<-list()
#q<-list()
#m<-0
#s<-0

correctfrac<-list()
#Running the code 50 times.
for(z in 1:50)
{
  #Fraction of data in the training set.
  #t=0.5+(0.05*z)
  t=0.7
  
  #Creating a column in the BreastCancer data for row numbers.
  BreastCancer$no<-c(1:nrow(BreastCancer))
  
  #Assigning data to the training set.
  train <- BreastCancer[sample(1:nrow(BreastCancer),t*nrow(BreastCancer),replace=FALSE),]
  #head(train)
  
  #Counting the number of unique values in the column "Class" and the number of times these
  #values appear in the training data.
  #as.data.frame(table(train[,11]))
  
  #Determining the test data from complete data minus the training data.
  test<-BreastCancer[!(BreastCancer$no %in% train$no),]
  
  #Counting the number of unique values in the column "Class" and the number of times these
  #values appear in the test data.
  #as.data.frame(table(test[,11]))
  
  #Dropping the column added for row numbers from the three data sets.
  BreastCancer$no<-NULL
  train$no<-NULL
  test$no<-NULL
  
  
  #Tuning parameters and building a classifier using cross validation.
  tunedmodel<-tune.svm(Class~.,data=train,gamma=10^(-3:-1),cost=10^(-1:2))
  model<-svm(Class~.,data=train, gamma=tunedmodel$best.parameters[1],cost=tunedmodel$best.parameters[2])
  
  #Pridicting the performance on the test data.
  pred<-predict(model,test[,-10])
  
  tab<-table(pred=pred,true=test[,10])
  
  correct<-tab[1,1]+tab[2,2]
  
  
  correctfrac[[z]]<-(correct/nrow(test))
  
  
  #summary(pred)
  
}
correctfrac
df<-as.numeric(correctfrac)
m<-list()
m[5]<-mean(df)

s<-sd(df)
dev<-1.96*s/sqrt(nrow(test))
upp<-list()
low<-list()
upp[5]<-as.numeric(m[5])+dev
upp[5]
low[5]<-as.numeric(m[5])-dev
low
tp<-c(2,3,4,5,6)
class(tp)
mean(df)
sd(df)
for(i in 1:10)
{
  res[i,1]<-p[[i]]
  res[i,2]<-q[[i]]
}
seq(0.5,0.95,length=10)
res$t<-c(seq(0.5,0.95,length=10))
res
res$sd<-NULL
res <- subset(res, select=c(2,1))
install.packages("psych")
require(psych)
error.bars(res)
plot(res$t,res$mean,add=TRUE)
help(error.bars)