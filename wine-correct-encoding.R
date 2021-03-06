#metafora tou dataset apo to Excel stin R
options(digits=10) #aparaitito gia na min ginoun anepithimites strogilopoiseis
options(scipen=100) #�������� �� scientific notation ��� ������� �� �������� ��� 100 �����

#eisagogi tou winequality-red.txt ���� ��� Import Dataset
RedWine$alcohol=as.numeric(RedWine$alcohol)#���������� ��� � ��������� Alcohol �������� ����� �� Factor
attach(RedWine)
library(pastecs) #��� ����� ��� ������� stat.desc
library(plotrix)
library(psych)
library(car)
library(MASS)
library(e1071)#SVM
library(caret)
sum1<-summary(RedWine) #1� ������ ���������� ��� dataset 
write.csv(sum1,'Dataset Summary.csv')
stat1<-stat.desc(RedWine,p=0.95) #���������� ����� ��� ��� ��������� ����������
write.csv(stat1,'statdesc.csv')

#���������


#������������


hist(fixed.acidity,col="lightgreen",freq=FALSE,main="������� �������", xlab="�����",ylab="���������" )
curve(dnorm(x, mean=mean(fixed.acidity), sd=sd(fixed.acidity)), add=TRUE)

hist(volatile.acidity,col="lightgreen",freq=FALSE,main="������� �������", xlab="�����",ylab="���������" )
curve(dnorm(x, mean=mean(volatile.acidity), sd=sd(volatile.acidity)), add=TRUE)

hist(citric.acid,col="lightgreen",freq=FALSE,main="������� ���", xlab="�����",ylab="���������" )
curve(dnorm(x, mean=mean(citric.acid), sd=sd(citric.acid)), add=TRUE)

hist(residual.sugar,col="lightgreen",freq=FALSE,main="��������� ��������", xlab="�����",ylab="���������" )
curve(dnorm(x, mean=mean(residual.sugar), sd=sd(residual.sugar)), add=TRUE)

hist(chlorides,col="lightgreen",freq=FALSE,main="��������", xlab="�����",ylab="���������" )
curve(dnorm(x, mean=mean(chlorides), sd=sd(chlorides)), add=TRUE)

hist(free.sulfur.dioxide ,col="lightgreen",freq=FALSE,main="�������� ������ ���������", xlab="�����",ylab="���������" )
curve(dnorm(x, mean=mean(free.sulfur.dioxide ), sd=sd(free.sulfur.dioxide )), add=TRUE)

hist(total.sulfur.dioxide ,col="lightgreen",freq=FALSE,main="�������� ������ ���������", xlab="�����",ylab="���������" )
curve(dnorm(x, mean=mean(total.sulfur.dioxide ), sd=sd(total.sulfur.dioxide )), add=TRUE)

hist(density ,col="lightgreen",freq=FALSE,main="��������� �������", xlab="�����",ylab="���������" )
curve(dnorm(x, mean=mean(density ), sd=sd(density )), add=TRUE)

hist(pH ,col="lightgreen",freq=FALSE,main="p�", xlab="�����",ylab="���������" )
curve(dnorm(x, mean=mean(pH ), sd=sd(pH )), add=TRUE)

hist(sulphates ,col="lightgreen",freq=FALSE,main="������", xlab="�����",ylab="���������" )
curve(dnorm(x, mean=mean(sulphates ), sd=sd(sulphates )), add=TRUE)

hist(alcohol ,col="lightgreen",freq=FALSE,main="�������", xlab="�����",ylab="���������" )
curve(dnorm(x, mean=mean(alcohol ), sd=sd(alcohol )), add=TRUE)

hist(quality ,col="lightgreen",breaks=10,main="��������", xlab="�����",ylab="���������")
# pinakas syxnotitwn
qfreq <- c(sum(quality==3),sum(quality==4),sum(quality==5),sum(quality==6),sum(quality==7),sum(quality==8))
qlabel <- c("�������� 3","�������� 4","�������� 5","�������� 6","�������� 7","�������� 8")
qper <- qfreq*(100/1599)
qfreqtable <- cbind(qlabel,qfreq,qper)
write.csv(qfreqtable,'freqtable.csv')

#Thikogramata
#�� ��������� ��������� ��� ������ Error in plot.new() : figure margins too large
#� ���� ����� �� �������� ��� plot viewer
colors = c(1:50) #���������� ���������� ��������
par(mfrow=c(2,2)) 
boxplot(fixed.acidity~quality,col=(c("gold","darkgreen")),ylab="������� �������",xlab="��������",pch=19 )
boxplot(volatile.acidity~quality,col=(c("gold","darkgreen")),ylab="������� �������",xlab="��������" )
boxplot(citric.acid~quality,col=(c("gold","darkgreen")),ylab="������� ���",xlab="��������" )
boxplot(residual.sugar~quality,col=(c("gold","darkgreen")),ylab="��������� ��������",xlab="��������" )
boxplot(chlorides~quality,col=(c("gold","darkgreen")),ylab="��������",xlab="��������" )
boxplot(free.sulfur.dioxide~quality,col=(c("gold","darkgreen")),ylab="�������� ������ ���������",xlab="��������" )
boxplot(total.sulfur.dioxide~quality, col=(c("gold","darkgreen")),ylab="�������� ������ ���������",xlab="��������" )
boxplot(density~quality,  col=(c("gold","darkgreen")),ylab="���������",xlab="��������" )
boxplot(pH~quality,col=(c("gold","darkgreen")),ylab="pH",xlab="��������" )
boxplot(sulphates~quality,col=(c("gold","darkgreen")),ylab="������",xlab="��������" )
boxplot(alcohol~quality,col=(c("gold","darkgreen")),ylab="�������",xlab="��������" )


#corellation 
p.cor<-cor(RedWine[,-12]) #��������� Pearson
write.csv(p.cor,'correlationmatrix.csv')#������� �� Excel

#A������� Outliers. ������ �������� ����������� ��� Q3 + 1.5IQR
limout <- rep(0,11)

for (i in 1:11){
  
  t1 <- quantile(RedWine[,i], 0.75) #������ Q3
  
  t2 <- IQR(RedWine[,i], 0.75) #������ IQR (������ ������ ��� �������������)
  
  limout[i] <- t1 + 1.5*t2
  
}

winequalityIndex <- matrix(0, 1599, 11)

for (i in 1:1599)
  
  for (j in 1:11){
    
    if (RedWine[i,j] > limout[j]) winequalityIndex[i,j] <- 1
    
  }

RWInd <- apply(winequalityIndex, 1, sum)

RedWineTemp <- cbind(RWInd, RedWine)

Indexes <- rep(0, 208)

j <- 1

for (i in 1:1599){
  
  if (RWInd[i] > 0) {Indexes[j]<- i
                     
                     j <- j + 1}
  
  else j <- j
  
}

RedWineLib <-RedWine[-Indexes,] 

indexes = sample(1:nrow(RedWineLib), size=0.5*nrow(RedWineLib))

#����������� ��� ��� dataset �� Train ��� Test 
#Dataset �� �� outliers (wo=with outliers)
data = RedWine
dim(data) #������ ���������� ������

indexes1 = sample(1:nrow(data), size=0.3*nrow(data)) #���������� �� 0.3, ������ ��� �� ������ ������� ��� dataset

test.wo = data[indexes1,] #����������� ��� dataset
dim(test.wo)  
train.wo = data[-indexes1,]
dim(train.wo) 
test.wo.1<-test.wo[,-12]

#Dataset ����� Outliers
data = RedWineLib
dim(data) #������ ���������� ������

indexes2 = sample(1:nrow(data), size=0.3*nrow(data)) #���������� �� 0.3, ������ ��� �� ������ ������� ��� dataset

test.no = data[indexes2,] #����������� ��� dataset
dim(test.no)  
train.no = data[-indexes2,]
dim(train.no) 
test.no.1<-test.no[,-12]
#�������� multiple regression

#�) ��� set �� �� outliers
lm.wout <- step(lm(quality ~ 1, RedWine), scope=list(lower=~1,upper = ~alcohol*(fixed.acidity+volatile.acidity+citric.acid+residual.sugar++chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates), direction="forward"))
# �������� ��� quality ~ volatile.acidity + alcohol + sulphates + chlorides + 
#total.sulfur.dioxide + residual.sugar + free.sulfur.dioxide + 
#alcohol:sulphates + alcohol:free.sulfur.dioxide + alcohol:total.sulfur.dioxide
#To ����� ��� �� �������
lm.wo.f<-lm(quality ~ volatile.acidity + alcohol + sulphates + chlorides + total.sulfur.dioxide + residual.sugar + free.sulfur.dioxide + alcohol:sulphates + alcohol:free.sulfur.dioxide + alcohol:total.sulfur.dioxide,train.wo)
summary(lm.wo.f)
lmFitWineQuality.wo <- predict(lm.wo.f, newdata = test.wo1) # ���� ��� ��������,
correct_predictions.wo <- sum(round(lmFitWineQuality.wo) == test.wo) #������ ����������
accuracy.wo <- correct_predictions.wo/(length(test.wo[[1]])) #������ ���������
accuracy.wo

#Montelo me pr<0.05
lm.wo.f6<-lm(quality ~ volatile.acidity  + sulphates + chlorides + alcohol:sulphates + alcohol:total.sulfur.dioxide,train.wo)
summary(lm.wo.f6)
lmFitWineQuality.wo <- predict(lm.wo.f6, newdata = test.wo1) # ���� ��� ��������,
correct_predictions.wo <- sum(round(lmFitWineQuality.wo) == test.wo) #������ ����������
accuracy.wo <- correct_predictions.wo/(length(test.wo[[1]])) #������ ���������
accuracy.wo


#�) ��� set ����� �� outliers                
lm.nout <- step(lm(quality ~ 1, RedWineLib), scope=list(lower=~1,upper = ~alcohol*(fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates), direction="forward"))
#�������� ��� quality ~ sulphates + volatile.acidity + alcohol + chlorides + 
#total.sulfur.dioxide + pH + residual.sugar + free.sulfur.dioxide + 
#fixed.acidity
#�� ����� ��� �� �������
lm.no.f<-lm(quality ~ sulphates + volatile.acidity + alcohol + chlorides + total.sulfur.dioxide + pH + residual.sugar + free.sulfur.dioxide + fixed.acidity, train.no)
summary(lm.no.f)
lmFitWineQuality <- predict(lm.no.f, newdata = test.no1) # ���� ��� ��������,
correct_predictions.no <- sum(round(lmFitWineQuality) == test.no) #������ ����������
accuracy.no <- correct_predictions.no/(length(test.no[[1]])) #������ ���������
accuracy.no

lm.no.f2<-lm(quality ~ sulphates + volatile.acidity + alcohol +  total.sulfur.dioxide + pH + free.sulfur.dioxide, train.no)
summary(lm.no.f2)
lmFitWineQuality <- predict(lm.no.f2, newdata = test.no1) # ���� ��� ��������,
correct_predictions.no <- sum(round(lmFitWineQuality) == test.no) #������ ����������
accuracy.no <- correct_predictions.no/(length(test.no[[1]])) #������ ���������
accuracy.no
#SVM

#Dhmiourgia katigoroiopoihshs gia quality<=5, quality=>6

newq1 <- ifelse (train.wo$quality<=5,"������","����")
newq2 <- ifelse (train.no$quality<=5,"������","����")
newq3 <- ifelse (test.wo$quality<=5,"������","����")
newq4 <- ifelse (test.no$quality<=5,"������","����")
strain.wo<-data.frame(train.wo[-12],newq1)
strain.no<-data.frame(train.no[-12],newq2)
stest.wo<-data.frame(test.wo[-12],newq3)
stest.no<-data.frame(test.no[-12],newq4)
names(stest.wo)[12]<-"quality"
names(stest.no)[12]<-"quality"
names(strain.wo)[12]<-"quality"
names(strain.no)[12]<-"quality"

#Tuning-������� cost ��� gamma
svm1<-svm(quality~.,data=strain.no,kernel ="linear",cross=10)
svm2<-svm(quality~.,data=strain.no,kernel ="polynomial",cross=10)
svm3<-svm(quality~.,data=strain.no,kernel ="radial",cross=10)
summary(svm1)
summary(svm2)
summary(svm3)
#�������� ��� ��� �� ��������� ����� �� ������ Radial
tune.out<-tune(svm,quality~.,data=strain.no,kernel ="radial",ranges=list(cost=c(0.1 ,1 ,10 ,100 ,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
#�������� ������������ ���  cost=1 gamma=0.5
svm4<- svm(quality~.,data=strain.no,kernel ="radial",cost=1 ,gamma=0.5)
predict.no <- predict(svm4, newdata = stest.no) # ���� ��� ��������,
table(predict.no,stest.no$quality)
conmatrix<-table(predict.no,stest.no$quality)
confusionMatrix(conmatrix, positive = NULL, prevalence = NULL)



#me 
svm5<-svm(quality~.,data=strain.wo,kernel ="linear",cross=10)
svm6<-svm(quality~.,data=strain.wo,kernel ="polynomial",cross=10)
svm7<-svm(quality~.,data=strain.wo,kernel ="radial",cross=10)
summary(svm5)
summary(svm6)
summary(svm7)
#�������� ��� ��� �� ��������� ����� �� ������ Radial
tune.out2<-tune(svm,quality~.,data=strain.wo,kernel ="radial",ranges=list(cost=c(0.1 ,1 ,10 ,100 ,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out2)
#�������� ������������ ���  cost=1 gamma=0.5
svm8<- svm(quality~.,data=strain.wo,kernel ="radial",cost=1 ,gamma=0.05)
predict.wo <- predict(svm8, newdata = stest.wo) # ���� ��� ��������,
conmatrix2<-table(predict.wo,stest.wo$quality)
confusionMatrix(conmatrix, positive = NULL, prevalence = NULL)
