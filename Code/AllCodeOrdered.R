

# this is all the code from the book, ordered by the sequence of chapters
# in the book

# PrologueReg.tex: 

data(day1)
head(day1)
nrow(day1)
day1 <- day1[,c(8,10:13,16)]  
head(day1)
knnout <- qeKNN(day1,'tot',k=5)  # fit the k-NN model
today <- data.frame(workingday=1, temp=12.8, atemp=11.8, hum=0.23,
 windspeed=5)
predict(knnout,today)  # predict new case
anotherday <- today
anotherday$windspeed <- 18
predict(knnout,anotherday) 
predict(knnout,rbind(today,anotherday))
data(mlb)
head(mlb)
w <- mlb[,c(4:6)]  # extract height, weight and age
z <- qeKNN(w,'Weight')  # fit k-NN model
predict(z,data.frame(Height=72,Age=24))
knnout1 <- qeKNN(mlb[,3:6],'Weight',25)  # extract Position, Height, Age
predict(knnout1,data.frame(Height=72,Age=24,Position='Catcher'))
sqrt((70/39.37-72/39.37)^2 + (12*24-12*30)^2)
ht <- mlb$Height
ht <- (ht - mean(ht)) / sd(ht)
mlb$Height <- ht
x <- data.frame(u=3:5,v=c(12,5,13))
x
mmscale(x)
kno <- qeKNN(day1,'tot',1)
datapoint3X <- day1[3,-6]  # remote Y value
predict(kno,datapoint3X)
day1[3,6]
knnout <- qeKNN(day1,'tot',5)
knnout$testAcc
meanTot <- mean(day1$tot)
meanTot
mean(abs(day1$tot - meanTot))
qeKNN(day1,'tot',10)$testAcc
qeKNN(day1,'tot',25)$testAcc
qeKNN(day1,'tot',10)$testAcc
qeKNN(day1,'tot',10)$testAcc
set.seed(9999)
qeKNN(day1,'tot',10)$testAcc
qeKNN(day1,'tot',10)$testAcc
set.seed(9999)  # try it again
qeKNN(day1,'tot',10)$testAcc
qeKNN(day1,'tot',10)$testAcc
plot(day1$tot,type='l')  # plotting type is 'l', lines between points
day1[,c(8,10:13,16)]
data(day1)
day2 <- day1[,c(1,8,10:13,16)]  
kno <- qeKNN(day2,'tot',k=5)  # re-run k-NN on the new data
kno$testAcc
head(day1)
table(day1$weathersit)
w <- c(102,140,0,129,0)
w == 0
w[w == 0] <- NA  # set the TRUE elements of w to NAs
w
library(regtools)
args(kNN)
x <- mlb[,c(4,6)]  # height, age
y <- mlb[,5]  # weight
newx <- c(72,24)  # ht, age of new player to be predicted re weight
kmax <- 25   # k
knnout <- kNN(x,y,newx,kmax) 
knnout$regests  # predicted value
kno <- qeKNN(mlb[,4:6],'Weight',k=25,holdout=NULL)
predict(kno,data.frame(Height=72,Age=24))


# PrologueClass.tex:

vert <- read.table('column_3C.dat',header=FALSE,stringsAsFactors=TRUE)
head(vert)
table(vert$V7)
class(vert$V7)
levels(vert$V7)
telco <- read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv',header=TRUE)
head(telco)
names(telco)
class(telco$Churn)
telco <- read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv',header=TRUE,
 stringsAsFactors=TRUE)
class(telco$Churn)
tc <- telco[,-1]
sum(is.na(tc))
ccIdxs <- which(complete.cases(tc))
tc <- tc[ccIdxs,]
tc <- na.exclude(tc)
nrow(tc)
set.seed(9999)
knnout <- qeKNN(tc,'Churn',75)
newCase <- tc[8,]
newCase$gender <- 'Male'
newCase$SeniorCitizen <- 1
newCase <- newCase[,-20]
newCase <- subset(newCase,select=-Churn) 
predict(knnout,newCase)
knnout$testAcc
length(levels(telco$customerID))
vert <- read.table('column_3C.dat',header=FALSE,stringsAsFactors=TRUE)
head(vert)
nrow(vert)
set.seed(999)
## kout <- qeKNN(vert,'V7',5)
kout <- qeKNN(vert,'V7',50)
z <- vert[1,-7]  # exclude Y
z$V2 <- 25
predict(kout,z)
kout$testAcc
mean(telco$Churn == 'Yes')
lco$Churn == 'Yes'
table(vert$V7) / nrow(vert)
kout$baseAcc
data(day1)
day1 <- day1[,c(8,10:13,16)]  # extract the desired features
set.seed(9999)
knnout <- qeKNN(day1,'tot',k=5)  # fit the k-NN model
knnout$testAcc
knnout$baseAcc
kout$confusion
ma <- read.csv('KaggleV2-May-2016.csv',header=TRUE,
 stringsAsFactors=TRUE) 
# load('MLBook_ma_data')
names(ma)
nrow(ma)
length(unique(ma$PatientId))
ma <- ma[,-c(1,2,4,5,7)]
names(ma)
table(ma$No.show)
kout <- qeKNN(ma,'No.show',25)
names(kout)
names(kout$holdoutPreds)
predY <- kout$holdoutPreds$predClasses
table(predY)
preds <- predict(kout,ma[,-9])  # exclude "Y", column 9
table(preds$probs[,2])
sum(preds$probs[,2] > 0.25)
w <- qeROC(tc,knnout,'Churn','Yes')
w$auc
head(w$thresholds,20)
head(w$specificities,20)
head(w$sensitivities,20)
qeROC(vert,kout,'V7','DH')
qeROC(vert,kout,'V7','NO')
qeROC(vert,kout,'V7','SL')


# BiasVarOver.tex:

data(pef)
head(pef)
dim(pef)
z <- qeKNN(pef,'wageinc',k=10)
z$testAcc
z$baseAcc


# DimReduce.tex:   

yr <- read.csv('YearPredictionMSD.txt',header=FALSE)
library(data.table)      
yr <- fread('YearPredictionMSD.txt')
yr <- as.data.frame(yr)
dim(yr)
system.time(kNN(yr[,-1],yr[,1],yr[1,-1],25))
pcout <- prcomp(yr[,-1]) 
rt <- pcout$rotation
dim(rt)
rt[1:10,1:10]
names(yr)
pcout$rotation <- rt[,1:20]
pcX <- predict(pcout,yr[,-1])  
dim(pcX)
w <- yr[8,-1]
w[1] <- 32.6 
newW <- predict(pcout,w)
newW
pcout$sdev^2
u <- c(12,5,13)
cumsum(u)  # 12+5 = 17, 12+5+13=30
cumsum(u) / length(u)  # convert to proportions
pcsds <- pcout$sdev^2
cumsum(pcsds) / sum(pcsds)
z$testAcc
w <- yr[8,-1]
w[1] <- 32.6
predict(z,w)
qeFOCI(pef,'wageinc')
yr1 <- yr[sample(1:nrow(yr),50000),]
system.time(z <- qeFOCI(yr1,'V1',numCores=2,parPlat='locThreads'))
z


# CART.tex:

w <- airq[1,-1]
w[2] <- 8.8
w
predict(dtout,w)
plot(dtout)
dtout$termNodeMembers[['5']]
data(yell10k)
head(yell10k)
length(unique(yell10k$PULocationID))
length(unique(yell10k$DOLocationID))
143*205
dtout <- qeDT(yell10k,'tripTime')
dtout$testAccAA
dtout$baseAcc
dtout
dtout$termNodeCounts
library(data.table)
cvr <- fread('covtype.data.gz')
cvr[1,]
cvr$V55 <- as.factor(cvr$V55)
table(cvr$V55)
cvr50k <- cvr[sample(1:nrow(cvr),50000),]
dto <- qeDT(cvr50k,'V55')
dto$testAcc
dto$baseAcc
dto$confusion
dto$nNodes
dto$nTermNodes
sort(airq$Wind)
args(qeDT)

# BagBoost.tex:

rfout <- qeRF(vert,'V7',holdout=NULL)
z <- vert[1,-7]
z$V2 <- 18
predict(rfout,z)
predict(kout,z)
dim(afrsoil)
names(afrsoil)
set.seed(9999)
rfo <- qeRF(afrsoil[,c(1:3578,3597)],'pH',holdout=500)
rfo$testAcc
rfo$baseAcc
range(afrsoil$pH)
predict(rfo,afrsoil[88,1:3594])
ds <- read.csv('dataset.csv',stringsAsFactors=TRUE) 
names(ds) 
ds <- ds[,-1] 
head(ds) 
dim(ds)
set.seed(9999)
gbout <- qeGBoost(ds,'MOS',nTree=750,holdout=NULL)  
ds3 <- ds[3,-8]
ds3[,3] <- 1500
ds3[,4] <- 62
predict(gbout,ds3)
gbm.perf(gbout$gbmOuts)
predict(gbout,ds3,newNTree=382)
mean(abs(preds - ds[,8]))
set.seed(9999)
gbout <- qeGBoost(vert,'V7')
predict(gbout,vert[12,-7])


# FineTuning.tex:

set.seed(9999)
ftout <- qeFT(data=pef,yName='wageinc',qeftn='qeKNN',
   pars=list(k=5:25),nTst=1000,nXval=5)
ftout
ftout <- qeFT(data=pef,yName='occ',qeftn='qeKNN',pars=list(k=1:25),
 nTst=1000,nXval=5)
ftout
head(phoneme)
dim(phoneme)
z <- qeFT(phoneme,'lbl','qeDT',list(alpha=c(0.01,0.05,0.10,0.25,0.50,1),
 50,1000,5,showProgress=T)
z


# LinGenLin.tex:

data(mlb) 
hw <- mlb[,2:3] 
ht71 <- which(hw$Height == 71)
mean(hw$Weight[ht71])
meanWts <- tapply(hw$Weight,hw$Height,mean) 
meanWts
meanWts['70']
plot(names(meanWts),meanWts) 
lmout <- lm(Weight ~ .,data=hw)
lmout
predict(lmout,data.frame(Height=71))
qelout <- qeLin(hw,'Weight',holdout=NULL)
qelout$coef
predict(qelout,data.frame(Height=71))
qelout <- qeLin(mlb[,4:6],'Weight',holdout=NULL)
predict(qelout,data.frame(Height=71,Age = 28))
Abb[1,]$house_rules 
Abb[1,]$monthly_price 
table(Abb$square_feet)
head(Abb)
linout <- qeLin(Abb[,-c(4,5)],'monthly_price',holdout=NULL)
linout$coef
class(linout)
linout <- qeLin(Abb[,-c(4,5)],'monthly_price')
data(mlb)
mlb[1,]
set.seed(9999)
glout <- qeLogit(tc,'Churn',holdout=NULL,yesYVal='Yes')
names(tc)
newx <- tc[333,-20]  # exclude Y
newx
newx$gender <- 'Female'
predict(glout,newx)
data(falldetection)
fd <- falldeteciton 
head(fd) 
fd$ACTIVITY <- as.factor(fd$ACTIVITY)  # was integer, need factor for qe*
set.seed(9999)
fd$ACTIVITY <- as.factor(fd$ACTIVITY)
fdout <- qeLogit(fd,'ACTIVITY')
fdout$testAcc
fdout$baseAcc
table(fd$ACTIVITY)
data(day1)
e1 <- day1[,c(4,10,12,13,16)]
e2 <- day1[,c(4,10,12,13,16,6,7)]  # add holiday, weekday columns
names(e1)
names(e2)
set.seed(9999)
e1out <- qeLin(e1,'tot')
e2out <- qeLin(e2,'tot')
newx1 <- e1[3,-5]  # exclude tot
newx2 <- e2[3,-5]  # exclude tot
predict(e1out,newx1)
predict(e2out,newx2)
stdErrPred(e1out,newx1)
stdErrPred(e2out,newx2)
data(day1)
day1tottemp <- day1[,c(10,16)]  # just tot, temp
head(day1tottemp)
day1tottemp$tempSqr <- day1tottemp$temp^2 
head(day1tottemp)
day1tottemp <- day1[,c(10,16)]
qepout <- qePolyLin(day1tottemp,'tot',deg=2,holdout=NULL) 
names(qepout)
qepout$bh
predict(qepout,data.frame(temp=12))
set.seed(9999)
qePolyLin(day1tottemp,'tot',deg=2,holdout=100)$testAcc
set.seed(9999)  # to get the same holdout set
qeLin(day1tottemp,'tot',holdout=100)$testAcc
data(pef)   
set.seed(9999)
qeLogit(pef,'occ')$testAcc 
set.seed(9999) 
qePolyLog(pef,'occ',2)$testAcc 
table(mlb$Height)
qeCompare(vert,'V7',c('qeLogit','qePolyLog','qeKNN','qeRF','qeGBoost'),100)


# LASSO.tex:

yellout <- qeLASSO(yell10k,'tripTime')
yellout$testAcc
yellout$baseAcc
Abb$square_feet <- NULL 
Abb$weekly_price <- NULL 
Abb <- na.exclude(Abb)
z <- qeLASSO(Abb,'monthly_price',holdout=NULL)
plot(z)
x18 <- Abb[18,-4] 
x18[4] <- 360 
x18[8] <- 92 
predict(z,x18)
z$coefs
z$whenEntered
afrsoil1 <- afrsoil[,c(1:3578,3597)]
z <- qeLASSO(afrsoil1,'pH',holdout=NULL)
z$nzero
z$lambda


# SVM.tex:

data(forest500)
qeFOCI(forest500,'V55')$sel
f500 <- forest500[,c(1,6,55)]
head(f500)
table(f500$V55)
f500$V55 <- regtools::toSubFactor(f500$V55,list('1'))
head(f500)
plot(f500[,1:2],pch=ifelse(f500[,3] == '1',0,3))
w <- qeLogit(f500,'V55',holdout=NULL,yesYVal='1')
cf <- coef(w$glmOuts[[1]])
abline(a=-cf[1]/cf[3], b=-cf[2]/cf[3])
head(iris)
j2 <- iris[,c(2,4,5)]  # sep. width, pet. width species
head(j2)
j2$Species <- toSubFactor(j2$Species,'setosa')
j2[c(7,77),]
plot(j2[,1:2],pch=3*as.numeric(j2[,3]))
plot(iris[,c(1,3)],pch=as.numeric(iris$Species))
set.seed(9999)
z <- matrix(rnorm(500),ncol=2) 
plus1 <- z[z[,1]^2 + z[,2]^2 > 4,]  # outer ring, class +1
minus1 <- z[z[,1]^2 + z[,2]^2 < 2,]  # inner disk, class -1
plus1 <- cbind(plus1,+1)  # add in Y column
minus1 <- cbind(minus1,-1)  # add in Y column
head(plus1)  # take a look
head(minus1) 
pm1 <- rbind(plus1,minus1)  # combine into one dataset 
plot(pm1[,1:2],pch=pm1[,3]+2)  # gives us pluses and circles
pm2 <- pm1[,1:2]^2  # replace each X value by its square
pm2 <- cbind(pm2,pm1[,3])  # tack on Y to the new datasets
plot(pm2[,1:2],pch=pm2[,3]+2)
z <- qeSVM(f500,'V55',holdout=NULL)
newx <- f500[8,1:2]
newx
newx[2] <- 2888
predict(z,newx)
gpout <- polyreg::getPoly(forest500,2)  # input with n = 500, p = 54


# NeuralNets.tex:

z <- vert[1,-7]  # exclude "Y", which we are predicting
nnout <- qeNeural(vert,'V7',holdout=NULL)
z$V2 <- 18
predict(nnout,z)
pars <-  list(hidden=c('5,5','25,25','100,100','100,0.2,100,0.2',
 learnRate=c(0.0001,0.0005,0.001,0.005))
ftout <- qeFT(fd,'ACTIVITY','qeNeural',pars=pars,nTst=250,nXval=25)
ftout$outdf
qeNeural(fd,'ACTIVITY')$baseAcc  # any qe* function could be called


# FineTuning.tex:

set.seed(9999)
ftout <- qeFT(data=pef,yName='wageinc',qeftn='qeKNN',
   pars=list(k=5:25),nTst=1000,nXval=5)
ftout
ftout <- qeFT(data=pef,yName='occ',qeftn='qeKNN',pars=list(k=1:25),
 nTst=1000,nXval=5)
ftout
head(phoneme)
dim(phoneme)
z <- qeFT(phoneme,'lbl','qeDT',list(alpha=c(0.01,0.05,0.10,0.25,0.50,1),
 50,1000,5,showProgress=T)
z


# TimeSeries.tex:

x <- c(5,12,13,8,88,6) 
w <- TStoX(x,2)
wd <- as.data.frame(w)
wd
eus <- EuStockMarkets  # built-in R dataset
tsout <- qeTS(5,eus,'qeKNN',opts=list(k=10))  # use k-NN with k = 10
data(weatherTS)
head(weatherTS)
ptot <- weatherTS$PRECTOT
z <- qeTS(2,ptot,'qeRF',holdout=NULL)
length(ptot)
predict(z,ptot[4016:4017])
qeTS(1,ptot,"qeKNN")$testAcc
replicMeans(1000,'qeTS(1,ptot,"qeKNN")$testAcc')
mean(abs(ptot - mean(ptot)))
replicMeans(1000,'qeTS(1,ptot,"qeKNN")$testAcc')  
replicMeans(1000,'qeTS(2,ptot,"qeKNN")$testAcc')  
replicMeans(1000,'qeTS(3,ptot,"qeKNN")$testAcc')  
replicMeans(1000,'qeTS(4,ptot,"qeKNN")$testAcc')  
replicMeans(1000,'qeTS(5,ptot,"qeKNN")$testAcc')  
replicMeasn(1000,'qeTS(6,ptot,"qeKNN")$testAcc')  
replicMean(1000,'qeTS(7,ptot,"qeKNN")$testAcc')  
replicMeasn(1000,'qeTS(8,ptot,"qeKNN")$testAcc')  
replicMeans(1000,'qeTS(9,ptot,"qeKNN")$testAcc')  
replicMeans(1000,'qeTS(10,ptot,"qeKNN")$testAcc') 
replicMeans(1000,'qeTS(3,ptot,"qeLin")$testAcc')
replicMeans(1000,'qeTS(3,ptot,"qePolyLin")$testAcc')
replicMeans(1000,'qeTS(3,ptot,"qeRF")$testAcc')
data(quizzes)
str(quizzes)
quizzes[8,1]
quizzes[8,2]
z <- qeText(quizzes,qeName='qeRF')
predict(z,quizzes[8,1])
library(textdata)
ag <- dataset_ag_news()
agdf <- as.data.frame(ag)  # qe-series functions require data frames
agdf[,1] <- as.factor(agdf[,1])  # qe requires a factor Y
dim(ag)
agdf[28,]  # for example
smallSet <- sample(1:nrow(agdf),10000)
agdfSmall <- agdf[smallSet,]
w <- qeText(agdfSmall[,c(1,3)],'class',qeName='qeSVM')
w$testAcc
w$baseAcc
