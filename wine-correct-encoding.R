#metafora tou dataset apo to Excel stin R
options(digits=10) #aparaitito gia na min ginoun anepithimites strogilopoiseis
options(scipen=100) #σταματαω το scientific notation για νούμερα με λιγοτερα από 100 ψηφια

#eisagogi tou winequality-red.txt μέσω του Import Dataset
RedWine$alcohol=as.numeric(RedWine$alcohol)#Παρατηρησα πως η μεταβλητή Alcohol εισήχθει λάθος ως Factor
attach(RedWine)
library(pastecs) #για χρηση της εντολής stat.desc
library(plotrix)
library(psych)
library(car)
library(MASS)
library(e1071)#SVM
library(caret)
sum1<-summary(RedWine) #1η βασική επισκόπηση του dataset 
write.csv(sum1,'Dataset Summary.csv')
stat1<-stat.desc(RedWine,p=0.95) #Στατιστικά μέτρα για τις ποσοτικές μεταβλητές
write.csv(stat1,'statdesc.csv')

#Γραφήματα


#Ιστογράμματα


hist(fixed.acidity,col="lightgreen",freq=FALSE,main="Σταθερή οξύτητα", xlab="Τιμές",ylab="Πυκνότητα" )
curve(dnorm(x, mean=mean(fixed.acidity), sd=sd(fixed.acidity)), add=TRUE)

hist(volatile.acidity,col="lightgreen",freq=FALSE,main="Πτητική οξύτητα", xlab="Τιμές",ylab="Πυκνότητα" )
curve(dnorm(x, mean=mean(volatile.acidity), sd=sd(volatile.acidity)), add=TRUE)

hist(citric.acid,col="lightgreen",freq=FALSE,main="Κιτρικό Οξύ", xlab="Τιμές",ylab="Πυκνότητα" )
curve(dnorm(x, mean=mean(citric.acid), sd=sd(citric.acid)), add=TRUE)

hist(residual.sugar,col="lightgreen",freq=FALSE,main="Κατάλοιπα Σακχάρων", xlab="Τιμές",ylab="Πυκνότητα" )
curve(dnorm(x, mean=mean(residual.sugar), sd=sd(residual.sugar)), add=TRUE)

hist(chlorides,col="lightgreen",freq=FALSE,main="Χλωρίδια", xlab="Τιμές",ylab="Πυκνότητα" )
curve(dnorm(x, mean=mean(chlorides), sd=sd(chlorides)), add=TRUE)

hist(free.sulfur.dioxide ,col="lightgreen",freq=FALSE,main="Ελεύθερο Θειικό Διοξείδιο", xlab="Τιμές",ylab="Πυκνότητα" )
curve(dnorm(x, mean=mean(free.sulfur.dioxide ), sd=sd(free.sulfur.dioxide )), add=TRUE)

hist(total.sulfur.dioxide ,col="lightgreen",freq=FALSE,main="Συνολικό Θειικό Διοξείδιο", xlab="Τιμές",ylab="Πυκνότητα" )
curve(dnorm(x, mean=mean(total.sulfur.dioxide ), sd=sd(total.sulfur.dioxide )), add=TRUE)

hist(density ,col="lightgreen",freq=FALSE,main="Πυκνότητα Κρασιού", xlab="Τιμές",ylab="Πυκνότητα" )
curve(dnorm(x, mean=mean(density ), sd=sd(density )), add=TRUE)

hist(pH ,col="lightgreen",freq=FALSE,main="pΗ", xlab="Τιμές",ylab="Πυκνότητα" )
curve(dnorm(x, mean=mean(pH ), sd=sd(pH )), add=TRUE)

hist(sulphates ,col="lightgreen",freq=FALSE,main="Θειώδη", xlab="Τιμές",ylab="Πυκνότητα" )
curve(dnorm(x, mean=mean(sulphates ), sd=sd(sulphates )), add=TRUE)

hist(alcohol ,col="lightgreen",freq=FALSE,main="Αλκοόλη", xlab="Τιμές",ylab="πυκνότητα" )
curve(dnorm(x, mean=mean(alcohol ), sd=sd(alcohol )), add=TRUE)

hist(quality ,col="lightgreen",breaks=10,main="Ποιότητα", xlab="Τιμές",ylab="Συχνότητα")
# pinakas syxnotitwn
qfreq <- c(sum(quality==3),sum(quality==4),sum(quality==5),sum(quality==6),sum(quality==7),sum(quality==8))
qlabel <- c("ποιότητα 3","ποιότητα 4","ποιότητα 5","ποιότητα 6","ποιότητα 7","ποιότητα 8")
qper <- qfreq*(100/1599)
qfreqtable <- cbind(qlabel,qfreq,qper)
write.csv(qfreqtable,'freqtable.csv')

#Thikogramata
#Σε περιπτωση εμφάνισης του λάθους Error in plot.new() : figure margins too large
#η λύση ειναι το μεγάλωμα του plot viewer
colors = c(1:50) #καθορισμός χρωματικής παλλέτας
par(mfrow=c(2,2)) 
boxplot(fixed.acidity~quality,col=(c("gold","darkgreen")),ylab="Σταθερή Οξύτητα",xlab="Ποιότητα",pch=19 )
boxplot(volatile.acidity~quality,col=(c("gold","darkgreen")),ylab="Πτητική Οξύτητα",xlab="Ποιότητα" )
boxplot(citric.acid~quality,col=(c("gold","darkgreen")),ylab="Κιτρικό Οξύ",xlab="Ποιότητα" )
boxplot(residual.sugar~quality,col=(c("gold","darkgreen")),ylab="Κατάλοιπα Σακχάρων",xlab="Ποιότητα" )
boxplot(chlorides~quality,col=(c("gold","darkgreen")),ylab="Χλωρίδια",xlab="Ποιότητα" )
boxplot(free.sulfur.dioxide~quality,col=(c("gold","darkgreen")),ylab="Ελεύθερο Θειικό Διοξείδιο",xlab="Ποιότητα" )
boxplot(total.sulfur.dioxide~quality, col=(c("gold","darkgreen")),ylab="Συνολικό Θειικό Διοξείδιο",xlab="Ποιότητα" )
boxplot(density~quality,  col=(c("gold","darkgreen")),ylab="Πυκνότητα",xlab="Ποιότητα" )
boxplot(pH~quality,col=(c("gold","darkgreen")),ylab="pH",xlab="Ποιότητα" )
boxplot(sulphates~quality,col=(c("gold","darkgreen")),ylab="Θειώδη",xlab="Ποιότητα" )
boxplot(alcohol~quality,col=(c("gold","darkgreen")),ylab="Αλκοόλη",xlab="Ποιότητα" )


#corellation 
p.cor<-cor(RedWine[,-12]) #Συσχέτιση Pearson
write.csv(p.cor,'correlationmatrix.csv')#Εξαγωγή σε Excel

#Aφαίρεση Outliers. Διώχνω στοιχεία μεγαλύτερων από Q3 + 1.5IQR
limout <- rep(0,11)

for (i in 1:11){
  
  t1 <- quantile(RedWine[,i], 0.75) #ευρέση Q3
  
  t2 <- IQR(RedWine[,i], 0.75) #εύρεση IQR (εύρους μεταξύ των τεταρτημόριων)
  
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

#Διαχωρισμός των δύο dataset σε Train και Test 
#Dataset με τα outliers (wo=with outliers)
data = RedWine
dim(data) #Εύρεση διαστάσεων πίνακα

indexes1 = sample(1:nrow(data), size=0.3*nrow(data)) #Αλλάζοντας το 0.3, αλλάζω και το τελικό μεγεθος των dataset

test.wo = data[indexes1,] #Διαχωρισμός του dataset
dim(test.wo)  
train.wo = data[-indexes1,]
dim(train.wo) 
test.wo.1<-test.wo[,-12]

#Dataset χωρίς Outliers
data = RedWineLib
dim(data) #Εύρεση διαστάσεων πίνακα

indexes2 = sample(1:nrow(data), size=0.3*nrow(data)) #Αλλάζοντας το 0.3, αλλάζω και το τελικό μεγεθος των dataset

test.no = data[indexes2,] #Διαχωρισμός του dataset
dim(test.no)  
train.no = data[-indexes2,]
dim(train.no) 
test.no.1<-test.no[,-12]
#Εφαρμογή multiple regression

#α) Στο set με τα outliers
lm.wout <- step(lm(quality ~ 1, RedWine), scope=list(lower=~1,upper = ~alcohol*(fixed.acidity+volatile.acidity+citric.acid+residual.sugar++chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates), direction="forward"))
# Καταλήγω στο quality ~ volatile.acidity + alcohol + sulphates + chlorides + 
#total.sulfur.dioxide + residual.sugar + free.sulfur.dioxide + 
#alcohol:sulphates + alcohol:free.sulfur.dioxide + alcohol:total.sulfur.dioxide
#To οποίο και θα ονομάσω
lm.wo.f<-lm(quality ~ volatile.acidity + alcohol + sulphates + chlorides + total.sulfur.dioxide + residual.sugar + free.sulfur.dioxide + alcohol:sulphates + alcohol:free.sulfur.dioxide + alcohol:total.sulfur.dioxide,train.wo)
summary(lm.wo.f)
lmFitWineQuality.wo <- predict(lm.wo.f, newdata = test.wo1) # τεστ του μοντέλου,
correct_predictions.wo <- sum(round(lmFitWineQuality.wo) == test.wo) #σωστές προβλέψεις
accuracy.wo <- correct_predictions.wo/(length(test.wo[[1]])) #βαθμός επιτυχίας
accuracy.wo

#Montelo me pr<0.05
lm.wo.f6<-lm(quality ~ volatile.acidity  + sulphates + chlorides + alcohol:sulphates + alcohol:total.sulfur.dioxide,train.wo)
summary(lm.wo.f6)
lmFitWineQuality.wo <- predict(lm.wo.f6, newdata = test.wo1) # τεστ του μοντέλου,
correct_predictions.wo <- sum(round(lmFitWineQuality.wo) == test.wo) #σωστές προβλέψεις
accuracy.wo <- correct_predictions.wo/(length(test.wo[[1]])) #βαθμός επιτυχίας
accuracy.wo


#β) Στο set χωρίς τα outliers                
lm.nout <- step(lm(quality ~ 1, RedWineLib), scope=list(lower=~1,upper = ~alcohol*(fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates), direction="forward"))
#καταλήγω στο quality ~ sulphates + volatile.acidity + alcohol + chlorides + 
#total.sulfur.dioxide + pH + residual.sugar + free.sulfur.dioxide + 
#fixed.acidity
#το οποίο και θα ονομάσω
lm.no.f<-lm(quality ~ sulphates + volatile.acidity + alcohol + chlorides + total.sulfur.dioxide + pH + residual.sugar + free.sulfur.dioxide + fixed.acidity, train.no)
summary(lm.no.f)
lmFitWineQuality <- predict(lm.no.f, newdata = test.no1) # τεστ του μοντέλου,
correct_predictions.no <- sum(round(lmFitWineQuality) == test.no) #σωστές προβλέψεις
accuracy.no <- correct_predictions.no/(length(test.no[[1]])) #βαθμός επιτυχίας
accuracy.no

lm.no.f2<-lm(quality ~ sulphates + volatile.acidity + alcohol +  total.sulfur.dioxide + pH + free.sulfur.dioxide, train.no)
summary(lm.no.f2)
lmFitWineQuality <- predict(lm.no.f2, newdata = test.no1) # τεστ του μοντέλου,
correct_predictions.no <- sum(round(lmFitWineQuality) == test.no) #σωστές προβλέψεις
accuracy.no <- correct_predictions.no/(length(test.no[[1]])) #βαθμός επιτυχίας
accuracy.no
#SVM

#Dhmiourgia katigoroiopoihshs gia quality<=5, quality=>6

newq1 <- ifelse (train.wo$quality<=5,"Μέτρια","Καλή")
newq2 <- ifelse (train.no$quality<=5,"Μέτρια","Καλή")
newq3 <- ifelse (test.wo$quality<=5,"Μέτρια","Καλή")
newq4 <- ifelse (test.no$quality<=5,"Μέτρια","Καλή")
strain.wo<-data.frame(train.wo[-12],newq1)
strain.no<-data.frame(train.no[-12],newq2)
stest.wo<-data.frame(test.wo[-12],newq3)
stest.no<-data.frame(test.no[-12],newq4)
names(stest.wo)[12]<-"quality"
names(stest.no)[12]<-"quality"
names(strain.wo)[12]<-"quality"
names(strain.no)[12]<-"quality"

#Tuning-επιλογή cost και gamma
svm1<-svm(quality~.,data=strain.no,kernel ="linear",cross=10)
svm2<-svm(quality~.,data=strain.no,kernel ="polynomial",cross=10)
svm3<-svm(quality~.,data=strain.no,kernel ="radial",cross=10)
summary(svm1)
summary(svm2)
summary(svm3)
#παρατηρώ πως έχω το μικρότερο λάθος με πυρήνα Radial
tune.out<-tune(svm,quality~.,data=strain.no,kernel ="radial",ranges=list(cost=c(0.1 ,1 ,10 ,100 ,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
#Καλύτερα αποτελέσματα για  cost=1 gamma=0.5
svm4<- svm(quality~.,data=strain.no,kernel ="radial",cost=1 ,gamma=0.5)
predict.no <- predict(svm4, newdata = stest.no) # τεστ του μοντέλου,
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
#παρατηρώ πως έχω το μικρότερο λάθος με πυρήνα Radial
tune.out2<-tune(svm,quality~.,data=strain.wo,kernel ="radial",ranges=list(cost=c(0.1 ,1 ,10 ,100 ,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out2)
#Καλύτερα αποτελέσματα για  cost=1 gamma=0.5
svm8<- svm(quality~.,data=strain.wo,kernel ="radial",cost=1 ,gamma=0.05)
predict.wo <- predict(svm8, newdata = stest.wo) # τεστ του μοντέλου,
conmatrix2<-table(predict.wo,stest.wo$quality)
confusionMatrix(conmatrix, positive = NULL, prevalence = NULL)
