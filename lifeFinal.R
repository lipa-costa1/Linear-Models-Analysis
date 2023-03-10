###########   professora, basta correr este ficheiro r para obter o  ###########
###########  modelo j? com o diagn?stico inicial aplicado, aplicando ###########
###########    a escolha por Best Subsets e retirando os outliers    ###########
###########            i.e. basta fazer CTRL+A e correr              ###########


###########  Imputa??o: aqui est? o c?digo para realizar a imputa??o ############
#
# for (X in c(2:20)) {
#   life[,X] = ifelse(is.na(life[,X]),
#                     ave(life[,X], FUN = function(x) median(x, na.rm = TRUE)),
#                     life[,X])
# }



library(ggplot2)
library(GGally)
library(leaps)
library(carData)
library(car)
library(MASS)
library(calibrate)
library(purrr)

life <- read.csv("https://web.tecnico.ulisboa.pt/~ist13493/AML2020_2021/Project/LifeExpectancyData.csv")
life <- as.data.frame(life)
life$Country<-as.factor(life$Country)
life<-life[,-c(2,3)]
life[,9]=ifelse(life[,9]>40,40,life[,9])

any(is.na(life))

life<-na.omit(life)
rownames(life)<-c(1:length(life[,1]))


out=boxplot(life)$out         #diagon?stico inicial, retira os outliers iniciais 

life <- life[-which(life$Life.expectancy %in% out),]
rownames(life)<-c(1:length(life[,1]))


n<-dim(life)[1]
set.seed(3493)                                      # for reproducible example
test.ind<-sample(n,0.2*n)                           # random sample of 20% of data
data.train<-life[-test.ind,]                        # complementar das linhas test.ind. 
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))

data.train<-rbind(data.train,data.test[54,]) #Ireland
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-54,]
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[54,]) #Equatorial Guinea
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-54,]
rownames(data.test)<-c(1:length(data.test[,1]))

#modelo completo
m.full <- lm(data.train$Life.expectancy~.,data=data.train)



#BEST SUBSET

regfit.full=regsubsets(Life.expectancy~.,data=data.train[,c(2:20)] ,nvmax=19,really.big=TRUE)
reg.summary=summary (regfit.full)



#escolha por adj R^2

lr <- leaps(data.train[,c(3:20)],data.train[,2],method="adjr2")
plot(lr$size,lr$adjr2,xlab="subset size", ylab="R^2 adjusted statistic")


plot(lr$size,lr$adjr2,xlab="subset size", ylab="R^2 adjusted statistic")

which.max(reg.summary$adjr2)

best.fitAdR2=reg.summary$which[which.min(reg.summary$cp),]
which(best.fitAdR2)+1
best.fitAdR2<-c(1,3,4,5,7,8,9,10,11,12,13,14,15,17,19,20)
m.bestsubsetAdR2<-lm(data.train$Life.expectancy~.,data=data.train[,best.fitAdR2])
m.bestsubset <- m.bestsubsetAdR2


#escolha por CP

lcp <- leaps(data.train[,c(3:20)],data.train[,2],method="Cp")
plot(lcp$size,lcp$Cp, xlab="subset size", ylab="Cp statistic")
abline(a=0,b=1, col="blue")


which.min(reg.summary$cp)

best.fitcp=reg.summary$which[which.min(reg.summary$cp),]
which(best.fitcp)+1
best.fitcp<-c(1,3,4,5,7,9,10,11,12,13,14,15,17,19,20)
m.bestsubsetcp<-lm(data.train$Life.expectancy~.,data=data.train[,best.fitcp])


# escolha por BIC

best.fitbic=reg.summary$which[which.min(reg.summary$bic),]
which(best.fitbic)+1
best.fitbic<-c(1,3,4,6,9,10,12,14,19,20) #j? inclui a vari?vel country

best.fitbic<-c(1,3,4,6,9,12,14,19,20) #j? sem a infant.deaths, que era desnecess?ria por ter problemas claros de multicolinearidade

m.bestsubsetbic<-lm(data.train$Life.expectancy~.,data=data.train[,best.fitbic])

m.bestsubset<-lm(data.train$Life.expectancy~.,data=data.train[,best.fitbic])



######################### come?ar a retirar outliers ###########################



k=length(m.bestsubset$coefficients)-1
cv=2*sqrt(k/n)
DFFITS=which(abs(dffits(m.bestsubset))>cv)
coisa=outlierTest(m.bestsubset,n.max=Inf)
OUTLIERS=as.numeric(as.vector(names(coisa$bonf.p)))
life<-rbind(data.train[-c(DFFITS,OUTLIERS,1021,1022,446,873,976),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


k=length(m.bestsubset$coefficients)-1
cv=2*sqrt(k/n)
DFFITS=which(abs(dffits(m.bestsubset))>cv)
coisa=outlierTest(m.bestsubset,n.max=Inf)
OUTLIERS=as.numeric(as.vector(names(coisa$bonf.p)))
life<-rbind(data.train[-c(DFFITS,OUTLIERS,305,497,878,925,923),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(175,40,105),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(175,40,105),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


k=length(m.bestsubset$coefficients)-1
cv=2*sqrt(k/n)
DFFITS=which(abs(dffits(m.bestsubset))>cv)
coisa=outlierTest(m.bestsubset,n.max=Inf)
OUTLIERS=as.numeric(as.vector(names(coisa$bonf.p)))
life<-rbind(data.train[-c(DFFITS,OUTLIERS,281,300,706,853),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(149,22,84,95),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(149,22,84,95),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])

k=length(m.bestsubset$coefficients)-1
cv=2*sqrt(k/n)
DFFITS=which(abs(dffits(m.bestsubset))>cv)
life<-rbind(data.train[-c(DFFITS,53,79,643,667,711),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(36,62,123),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(36,62,123),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


k=length(m.bestsubset$coefficients)-1
cv=2*sqrt(k/n)
DFFITS=which(abs(dffits(m.bestsubset))>cv)
coisa=outlierTest(m.bestsubset,n.max=Inf)
OUTLIERS=as.numeric(as.vector(names(coisa$bonf.p)))
life<-rbind(data.train[-c(DFFITS,OUTLIERS,93,306,395,676,426),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(80,35,149),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(80,35,149),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


k=length(m.bestsubset$coefficients)-1
cv=2*sqrt(k/n)
DFFITS=which(abs(dffits(m.bestsubset))>cv)
coisa=outlierTest(m.bestsubset,n.max=Inf)
OUTLIERS=as.numeric(as.vector(names(coisa$bonf.p)))
life<-rbind(data.train[-c(DFFITS,OUTLIERS,152,350,496,676,682),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(139,101),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(139,101),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])



life<-rbind(data.train[-c(334,337,527,551,644),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(139,101,43,12,22,68),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(139,101,43,12,22,68),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(284,306,180,442,619),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(33,140,63,151,36),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(33,140,63,151,36),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])




########## j? passa as suposi??es, continuar para obter PRESS finito ###########




life<-rbind(data.train[-c(236,331),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(138),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(138),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(47,333),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(137),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(137),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(209,420),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(38,7),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(38,7),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(268,334),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(155,37),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(155,37),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(111,373),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(414,511),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(79),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(79),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(413,521),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(61),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(61),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(135,509),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(107),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(107),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(468,552,615),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(4,173,465),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(134,408),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(127),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(127),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(592,626),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(283),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(289),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(111,145),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(545),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(464,512),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(127),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(127),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(320,528,619),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(85,139),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(85,139),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(509,317),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[c(32),])
rownames(data.train)<-c(1:length(data.train[,1]))
data.test<-data.test[-c(32),]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(615),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,]
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(331),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])


life<-rbind(data.train[-c(589),],data.test)
rownames(life)<-c(1:length(life[,1]))
n<-dim(life)[1]
set.seed(3493)             
test.ind<-sample(n,0.2*n)
data.train<-life[-test.ind,]
rownames(data.train)<-c(1:length(data.train[,1]))
data.test <- life[test.ind,] 
rownames(data.test)<-c(1:length(data.test[,1]))
m.bestsubset <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fitbic])





