library(IMTest)
library(nortest)
library(QuantPsyc)
library(jtools)
library(olsrr)

##########################  TESTES PARA AS SUPOSIÇÕES ##########################


pearson.test(resid(m.bestsubset)) #Qui Quadrado


shapiro.test(residuals(m.bestsubset)) #shapiro teste para normalidade


durbinWatsonTest(m.bestsubset) #teste Durbin Watson


ncvTest(m.bestsubset) #Avaliar homoscedasticity com Breusch-Pagan test


##############################  ANALISAR O MODELO ##############################

plot(m.bestsubset$residuals,xlab1="a",col=rgb(0.5,0.8,1),pch=16) #plots do modelo

hist(resid(m.bestsubset),col=rgb(0.5,0.8,1),pch=16,main="",xlab="Resíduos",ylab="Frequência") #histograma


anova_alt(m.bestsubset) #MSE e +

cena=summary(m.bestsubset)
c(cena$adj.r.squared,cena$r.squared) #R^2 ajustado e R^2

extractAIC(m.bestsubset)#AIC

extractAIC(m.bestsubset, k = log(n)) #BIC

sum((m.bestsubset$residuals/( 1-hatvalues(m.bestsubset)))^2)#PRESS (pode dar inf)?

ols_mallows_cp(m.bestsubset,m.full) #Cp

# previsão
previsao.y <- predict(m.bestsubset,data.test)
summary(sqrt((previsao.y - data.test$Life.expectancy)^2))

d<-data.frame(previsao.y, y=data.test$Life.expectancy) 
ggplot(d, aes(previsao.y, y, color =previsao.y )) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  theme_minimal()




vif(lm(data.train$Life.expectancy ~ .,data=data.train[,c(3,4,6,9,10,12,14,19,20)])) #vif das variáveis numéricas

