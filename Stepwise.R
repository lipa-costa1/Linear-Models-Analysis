library(ggplot2)
library(GGally)
library(car)
library(lmtest)
library(nortest)
library(calibrate)

#dados
life <- read.csv("https://web.tecnico.ulisboa.pt/~ist13493/AML2020_2021/Project/LifeExpectancyData.csv")
life <- as.data.frame(life)
life<-life[,-c(2,3)] # retirar variáveis "year" e "status"

#variável categórica
life$Country<-as.factor(life$Country)

#BMI < 40
life[,9]=ifelse(life[,9]>40,40,life[,9])

#Retirar NA's
life<-na.omit(life)
rownames(life)<-c(1:length(life[,1]))
summary(life)

#diagn�sitco inicial
out=boxplot(life)$out
out
life <- life[-which(life$Life.expectancy %in% out),]

#Reordenar
rownames(life)<-c(1:length(life[,1]))

#dados sem coluna dos pa�ses
life_sem_paises = life[,-1]

pairs(life)
ggpairs(life[,-1])+ scale_color_manual(values=c("#FA58AC"))

n<-dim(life)[1]

set.seed(3493)             # for reproducible example
test.ind<-sample(n,0.2*n)  # random sample of 20% of data

#Training data
data.train<-life[-test.ind,]
summary(data.train)
dim(data.train)

#Test data
data.test <- life[test.ind,]
summary(data.test)
dim(data.test)

#Reordenar e retirar
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[54,]) #Ireland
data.test<-data.test[-54,]
rownames(data.test)<-c(1:length(data.test[,1]))
data.train<-rbind(data.train,data.test[54,]) #Equatorial Guine
data.test<-data.test[-54,]




m1 <- lm(data.train$Life.expectancy~.,data=data.train) # com todas as variaveis X
summary(m1)

m1_sempaises <- lm(data.train[,-1]$Life.expectancy~.,data=data.train[,-1]) # com todas as variaveis X


anova(m1_sempaises)
anova_alt(m1)

extractAIC(m1) #AIC
extractAIC(m1, k = log(n)) #BIC
sum((m1$residuals/( 1-hatvalues(m1)))^2) #PRESSp

#Previsão
y.pred.m1 <- predict(m1,data.test) 
summary(sqrt((y.pred.m1 - data.test$Life.expectancy)^2))

d<-data.frame(yp.m1=y.pred.m1, y=data.test$Life.expectancy)
ggplot(d, aes(yp.m1, y, color =yp.m1 )) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  theme_minimal()



######## step FOI FEITO SEM PAISES(ADDED AFTER)
m.base <-lm(data.train[,-1]$Life.expectancy~Adult.Mortality,data=data.train[,-1]) #É a variável inicial mais significativa
summary(m.base)

m.full<- lm(data.train[,-1]$Life.expectancy~.,data=data.train[,-1])  
summary(m.full)

step.forward.NA <- step(m.base,scope =list(upper=m.full,lower=~1), direction = "forward", trace=FALSE) #escolhe com base no AIC

step.backward.NA <- step(m.full, direction = "backward", trace=FALSE )

step.both.NA <- step(m.base, scope = list(upper=m.full, lower=~1 ), direction = "both", trace=FALSE)

#Backward elimination using p-values to delete predictors one-at-a-time
options(max.print=999999)
m.full<- lm(data.train[,-1]$Life.expectancy~.,data=data.train[,-1])
summary(m.full)

m.new = update(m.full, .~. - Measles  )
summary(m.new)

m.new = update(m.new, .~. - thinness..1.19.years  )
summary(m.new)

m.new = update(m.new, .~. - Population )
summary(m.new)

m.new = update(m.new, .~. - Alcohol  )
summary(m.new)

m.new = update(m.new, .~. - Hepatitis.B    )
summary(m.new)

m.new = update(m.new, .~. - Diphtheria    )
summary(m.new)

m.new = update(m.new, .~. - GDP    )
summary(m.new)

m.new = update(m.new, .~. - thinness.5.9.years    )
summary(m.new)

step.backward.p.NA<-m.new

library(MASS)
summary(m.full)
dropterm(m.full, test = "F" )

m.new<-update(m.full, .~. - Measles )
dropterm(m.new, test = "F" )

m.new<-update(m.new, .~. - thinness..1.19.years )
dropterm(m.new, test = "F" )

m.new<-update(m.new, .~. - Population )
dropterm(m.new, test = "F" )

m.new<-update(m.new, .~. - Alcohol  )
dropterm(m.new, test = "F" )

m.new<-update(m.new, .~. - Hepatitis.B  )
dropterm(m.new, test = "F" )

m.new<-update(m.new, .~. - Diphtheria   )
dropterm(m.new, test = "F" )

m.new<-update(m.new, .~. - GDP   )
dropterm(m.new, test = "F" )

m.new<-update(m.new, .~. - thinness.5.9.years   )
dropterm(m.new, test = "F" )

step.backward.f.NA <- m.new
## o step.backward.f.NA e o step.backward.p.NA sao o mesmo e a ordem é a mesma


library(MASS)
m.null<- lm(data.train[,-1]$Life.expectancy~ 1,data=data.train[,-1]) 
summary(m.null)

addterm(m.null, scope=m.full, test="F" )
m.new<-update(m.null, .~. + Schooling)
addterm(m.new, scope=m.full, test="F" )

m.new<-update(m.new, .~. + HIV.AIDS )
addterm(m.new, scope=m.full, test="F" )

m.new<-update(m.new, .~. + Adult.Mortality    )
addterm(m.new, scope=m.full, test="F" )

m.new<-update(m.new, .~. + Income.composition.of.resources)
addterm(m.new, scope=m.full, test="F" )

m.new<-update(m.new, .~. + percentage.expenditure)
addterm(m.new, scope=m.full, test="F" )

m.new<-update(m.new, .~. + BMI)
addterm(m.new, scope=m.full, test="F" )

m.new<-update(m.new, .~. + Polio)
addterm(m.new, scope=m.full, test="F" )

m.new<-update(m.new, .~. + Total.expenditure )
addterm(m.new, scope=m.full, test="F" )

m.new<-update(m.new, .~. + thinness..1.19.years )
addterm(m.new, scope=m.full, test="F" )

m.new<-update(m.new, .~. + Diphtheria   )
addterm(m.new, scope=m.full, test="F" )

step.forward.f.NA <- m.new

step.both.aic.NA <- step(m.base, direction = "both", scope= formula(m1_sempaises))

summary(step.backward.NA) #11 VAR BEST
anova_alt(step.backward.NA) # a
summary(step.backward.p.NA)#11 VAR pior #igual ao step backwards f
anova_alt(step.backward.p.NA) # b
summary(step.both.NA)#11 VAR pior
anova_alt(step.both.NA) # c
summary(step.both.aic.NA)#11 VAR pior
anova_alt(step.both.aic.NA) # d
summary(step.forward.NA)# 12 VAR pior
anova_alt(step.forward.NA) # e
summary(step.forward.f.NA)# 12 VAR pioe
anova_alt(step.forward.f.NA)# f 


best.fit<-c("Country", "Adult.Mortality", "percentage.expenditure", "BMI", "under.five.deaths", "Polio", "Total.expenditure", "HIV.AIDS", "thinness.5.9.years", "Income.composition.of.resources", "Schooling") 
#Retirámos o infant deaths
m.stepwise <- lm(data.train$Life.expectancy ~ .,data=data.train[,best.fit])

summary(m.stepwise)
anova_alt(m.stepwise)

y.pred.m.stepwise <- predict(m.stepwise,data.test) 
summary(sqrt((y.pred.m.stepwise - data.test$Life.expectancy)^2))

d<-data.frame(yp.m.stepwise=y.pred.m.stepwise, y=data.test$Life.expectancy)
ggplot(d, aes(yp.m.stepwise, y, color =yp.m.stepwise )) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  theme_minimal()