library(olsrr)
library(dplyr)
library(ROCR)
library(MASS)
library(sandwich)
library(lmtest)
library(pscl)
library(ggplot2)
library(grid)
library(gridExtra)

olympics <- read.csv("olympics2016.csv")

olympics$GDPpercapita <- olympics$gdp2016 * 10^6/ olympics$pop2016

olympics$host2000 <- rep(0,108)
olympics$host2000[olympics$country == "Australia"] <- 1

olympics$host2004<- rep(0,108)
olympics$host2004[olympics$country == "Greece"] <- 1

olympics$host2008<- rep(0,108)
olympics$host2008[olympics$country == "China"] <- 1

olympics$host2012 <- rep(0,108)
olympics$host2012[olympics$country == "United Kingdom"] <- 1

olympics$prevhost2012 <- rep(0,108)
olympics$prevhost2012[olympics$country == "China"] <- 1
olympics$prevhost2012[olympics$country == "Greece"] <- 1

olympics$futhost2012 <- rep(0,108)
olympics$futhost2012[olympics$country == "Brazil"] <- 1

olympics$muslim[olympics$muslim==2]<- 1
#######################################################################################################################
str(olympics)
summary(olympics)
#corrleation between predictors
round(cor(olympics[,c(2,3,11,13,15,17,19)],use="na.or.complete"),2)
corrplot(cor(olympics[,c(2,3,11,13,15,17,19)],use="na.or.complete"),method="number" ,type="upper")

# histogram of the responce variable
ggplot(data=olympics,aes(x= tot12))+geom_histogram(aes(y=..density..),fill="blue",color="black") + geom_density(aes(y=..density..)) + xlab("total no of medals 2012") + ggtitle("Histogram of Number of medals 2012") + 
  theme(plot.title=element_text(hjust=0.5),plot.margin=unit(c(1,5,1.5,1.2),"cm"))

pairs.panels(olympics[,c(2,3,11,13,15,17,19)])
logolympics <- log(olympics[,c(2,3,11,13,15,17,19)])

#data before log transformation
ggpairs(olympics[,c(2,3,11,13,15,17,19)], lower="blank",upper=list(continuous = wrap("points",alpha=0.5 , color="blue")))

#data after log transformation
ggpairs(logolympics, lower="blank",upper=list(continuous = wrap("points",alpha=0.5 , color="blue")))

# |GDP and tot12 and label countries with medals >30

g2<-ggplot(olympics,aes(x=tot12,y=pop2016)) + geom_point(col="blue") +xlab("Total medal 2012") + ylab("Population")+ geom_text(aes(label=ifelse(tot12>35,as.character(country),'')),hjust=1,vjust=1)+ggtitle("Population vs Total medal 2012")+theme(plot.title=element_text(hjust=0.5))

g1<-ggplot(olympics,aes(x=tot12,y=gdp2016)) + geom_point(col="blue") +xlab("Total medal 2012") + ylab("GDP")+ geom_text(aes(label=ifelse(tot12>35,as.character(country),'')),hjust=1,vjust=1)+ggtitle("GDP vs Total medal 2012")+theme(plot.title=element_text(hjust=0.5),plot.margin=unit(c(1,5,1.5,1.2),"cm"))+geom_smooth(method="lm",se=F)

grid.arrange(g1,g2,nrow=1)


g3<-ggplot(train,aes(x=comm,y= tot,color=comm)) + geom_boxplot() +xlab("Current/former Communist") + ylab("Total medal 2012")+ ggtitle("Total medal 2012 vs Communist State")+theme(plot.title=element_text(hjust=0.5))
g4<- ggplot(train,aes(x=soviet,y= tot,color=soviet)) + geom_boxplot() +xlab("Former Soviet union") + ylab("Total medal 2012")+ ggtitle("Total medal 2012 vs Soviet union")+theme(plot.title=element_text(hjust=0.5))

grid.arrange(g3,g4,nrow=1)



g5<-ggplot(train,aes(x=oneparty,y= tot,color=oneparty)) + geom_boxplot() +xlab("One party state") + ylab("Total medal 2012")+ ggtitle("Total medal 2012 vs One party state")+theme(plot.title=element_text(hjust=0.5))
g6<- ggplot(train,aes(x=muslim,y= tot,color=muslim)) + geom_boxplot() +xlab("Mulsim Majority") + ylab("Total medal 2012")+ ggtitle("Total medal 2012 vs Muslim majority")+theme(plot.title=element_text(hjust=0.5))

grid.arrange(g5,g6,nrow=1)



############################################################################################################################################




train <- data.frame(country=olympics$country , GDPpercapita = olympics$GDPpercapita , pop = olympics$pop2016 , 
                    host = as.factor(olympics$host2012) , futhost = as.factor(olympics$futhost2012) , prevhost= as.factor(olympics$prevhost2012) ,
                    soviet = as.factor(olympics$soviet ), comm=as.factor(olympics$comm) , muslim = as.factor(olympics$muslim ), oneparty= as.factor(olympics$oneparty)
                    , tot=olympics$tot12 , totprev1=olympics$tot08 , totprev2=olympics$tot04,totprev3=olympics$tot00 , gold=olympics$gold12,
                    goldprev1=olympics$gold08 , goldprev2=olympics$gold04 , goldprev3= olympics$gold00 , totmedals= olympics$totmedals12,
                    totmedalsprev1= olympics$totmedals08 , totmedalsprev2 = olympics$totmedals04 , totmedalsprev3 = olympics$totmedals00 ,
                    GDP=olympics$gdp2016,athlete12 =olympics$athletes_sent2016)


test <- data.frame(country=olympics$country , GDPpercapita = olympics$GDPpercapita , pop = olympics$pop2016 , 
                    host = as.factor(olympics$host2016) , futhost = as.factor(olympics$futhost2016) , prevhost= as.factor(olympics$prevhost2016),
                    soviet = as.factor(olympics$soviet) , comm= as.factor(olympics$comm) , muslim = as.factor(olympics$muslim) , oneparty= as.factor(olympics$oneparty)
                    , tot=olympics$tot16 , totprev1=olympics$tot12 , totprev2=olympics$tot08,totprev3=olympics$tot04 ,
                    goldprev1=olympics$gold12 , goldprev2=olympics$gold08 , goldprev3= olympics$gold04 , totmedals= olympics$totmedals16,
                    totmedalsprev1= olympics$totmedals12 , totmedalsprev2 = olympics$totmedals08 , totmedalsprev3 = olympics$totmedals04 ,
                    GDP=olympics$gdp2016,athlete16 = olympics$athletes_sent2016)

train<-train[-54,]
test<- test[-54,]
##train<- train[complete.cases(train),]
##test<- test[,-c(54,64,1,82,89)]
train <- train %>% mutate( Medshare =tot/totmedals , Medshareprev1 = totprev1/totmedalsprev1 , Medshareprev2 =totprev2/totmedalsprev2 , Medshareprev3 = totprev3/totmedalsprev3)
test <- test %>% mutate( Medshare = tot/totmedals , Medshareprev1 = totprev1/totmedalsprev1 , Medshareprev2 = totprev2/totmedalsprev2 , Medshareprev3 = totprev3/totmedalsprev3)
train <- train %>% mutate(popperMedal = pop/totmedals , GDPperMedal= GDP/totmedals)
test <- test %>% mutate(popperMedal = pop/totmedals , GDPperMedal= GDP/totmedals)
train$avgtot <- (train$totprev1 +train$totmedalsprev2+train$totprev3 )/3
test$avgtot <- (test$totprev1+test$totprev2+test$totprev3)/3

#linear Model distribution

model1<- lm(tot ~ log(pop)+log(GDP) + host+soviet+comm+prevhost+totprev1+futhost+oneparty+muslim+totprev2+totprev3+athlete12,data=train)
#variance inflation factor to check for multicollinearity and execlude variables with high vif
vif(model1)
model1<- lm(tot ~ log(pop)+log(GDP) + host+soviet+comm+prevhost+totprev1+futhost+oneparty+muslim,data=train)
step(model1)
ols_step_backward(model1)
ols_step_forward(model1)
best <- ols_best_subset(model1)
plot(best)
#compare all this models and find the last one is the best one
model_final1<- lm(tot  ~ log(pop)+prevhost+host+oneparty+totprev1+muslim,data=train)
model_final2<- lm(tot  ~ host+oneparty+totprev1+muslim,data=train)
model_final3<- lm(tot ~ host+oneparty+totprev1,data=train)
model_final4<- lm(tot  ~ host+totprev1,data=train)
model_final5<- lm(tot  ~ totprev1,data=train)
# in sample MAE ,MFE and RMSE
predictions2012 <- data.frame("No of Medals"=train$tot,"Predicted No of Medals"=fitted(model_final5))
predictions2012 <- predictions2012 %>% mutate(diff=round(No.of.Medals-Predicted.No.of.Medals) , MFE=diff)
#The mean absolute error
MAE_train<- sum(abs(predictions2012$diff),na.rm=T)/nrow(predictions2012)
MFE_train <- sum(predictions2012$diff,na.rm=T)/nrow(predictions2012)
RMSE_train<- sqrt(sum((predictions2012$diff)^2,na.rm=T)/nrow(predictions2012))  #3.849
Performance <- cbind("MAE"=MAE_train,"MFE"=MFE_train,"RMSE"=RMSE_train)

#MAE betweeen 2.6574-2.37
#predicted medal for each country
pred <- predict(model_final5,test)
#out of sample MAE and MFE
predictions2016 <- data.frame("No of Medals"=test$tot,"Predicted No of Medals"=pred)
predictions2016 <- predictions2016 %>% mutate(diff=round(No.of.Medals-Predicted.No.of.Medals) , MFE=diff)
#The mean absolute error
MAE <- sum(abs(predictions2016$diff),na.rm=T)/nrow(predictions2016)
#Ideal value = 0;
#MFE > 0, model tends to under-forecast
#MFE < 0, model tends to over-forecast

MFE <- sum(predictions2016$MFE,na.rm=T)/nrow(predictions2016)
#54,64,1,82,89
#RMSE
RMSE <- sqrt(sum((predictions2016$diff)^2,na.rm=T)/nrow(predictions2016)) # 4.628815
##Model Diagnostics 
out_Performance <- cbind("MAE"=MAE,"MFE"=MFE,"RMSE"=RMSE)

par(mfrow=c(2,2))
plot(model_final5)

plot(train$tot,train$totprev1)
outlier <- identify(cbind(train$tot,train$totprev1))
train$country[outlier]
# China          Germany        Russia         United Kingdom United States 
#top ranked countries are outlier to the model

par(mfrow=c(2,2))

# 1- contstant variability of residuals
plot(model_final5$residuals ~ model_final5$fitted.values, xlab="fitted No of medals" , ylab = " Residuals" , pch=20,main="Residuals vs Fitted values")
abline(h=0,lty=2)

#2 
plot(model_final5$residuals , pch=20 ,ylab= "Residuals",main="Residuals vs index")
abline(h=0,lty=2)
#3-

qqnorm(model_final5$residuals)
qqline(model_final5$residuals, col= "blue")

#4-
plot(model_final5$residuals ~ train$totprev1,xlab="model from previous Olympics Games",ylab="Residuals",main="Residuals vs Predictor")






#As there are alot of more outliers ,the normality condition is dubious.
hist(model_final5$residuals,xlab= "Residuals", main= "Distribution of Residuals",col= "blue",prob=TRUE)
lines(density(model_final5$residuals),lwd=3)



# 3- contstant variability of residuals
# the fitted values vs residual is a random scatter for every bathroom discrete value.

plot(abs(model_final5$residuals) ~ model_final5$fitted.values,ylab= "Absolute Residuals",xlab= "Predicted House Price", pch=20 )




par(mfrow=c(3,2))

plot(model_final5$residuals ~ train$totprev2,xlab="model from previous Olympics Gmaes",main="Residuals vs Medal from previous Olympic")
# 4-independent residuals
#Residuals are independent.
plot(model_final5$residuals , pch=20 ,ylab= "Residuals",,main="Residuals vs index")
abline(h=0,lty=2)










#################################################################################################################
######################################################################################################################################

##Basic poisson model

poiss.m<- glm(tot~ log(GDPpercapita)+log(pop)+host+prevhost+futhost + soviet+comm + muslim + oneparty + totprev1,offset=log(totmedals) , data=train,family= poisson)
summary(poiss.m)
##using drop1 and F test several times we have the final model
poiss.m.f<- glm(tot~ log(GDPpercapita)+log(pop)+comm+ muslim +totprev1+host, data=train,family= poisson,offset = log(totmedals))
summary(poiss.m.f)

confint(poiss.m.f)
##overdispersion , adjusted poisson model
#coeftest(poiss.m,vcov=sandwich)
#adjpoiss.m<- glm(tot~ log(GDP)+log(pop)+host+prevhost+soviet + muslim + totprev1 , data=train,family= poisson,offset = log(totmedals))
#summary(adjpoiss.m)
#Goodness of fit based on residual deviance and degrees of freedom
#poisson model doesn't fit the data (p< 0.05)
1-pchisq(deviance(poiss.m.f),df.residual(poiss.m.f))

#################################################################################################################################

##Quasipoisson

Qpoiss.m<-  glm(tot~ log(GDPpercapita)+log(pop)+host+prevhost+futhost + soviet+comm + muslim + oneparty + totprev1,offset=log(totmedals) , data=train,family= "quasipoisson")
summary(Qpoiss.m)

##using drop1 and F test several times we have the final model
Qpoiss.m.f<- glm(tot~ log(GDPpercapita)+log(pop)+comm + muslim + totprev1+host, data=train,family= quasipoisson,offset = log(totmedals))
summary(Qpoiss.m.f)




#coeftest(Qpoiss.m,vcov=sandwich)
#adjQpoiss.m<- glm(tot~ log(GDP)+log(pop)+host+prevhost+comm+soviet + muslim + totprev1 , data=train,family= quasipoisson,offset = log(totmedals))
#summary(adjQpoiss.m)

##########################################################################################################################

##negative binomial 

nb.m <- glm.nb(tot~ log(GDPpercapita)+log(pop)+host+prevhost+futhost + soviet+comm + muslim + oneparty + totprev1 , data=train)
summary(nb.m)
##using drop1 LRT test several times we have the final model
nb.m.f<- glm.nb(tot~ log(GDPpercapita)+log(pop)+comm+muslim+ totprev1, data=train)
summary(nb.m.f)

# Goodness of fit (p>0.05 indicates that the model fit the data)
1-pchisq(deviance(nb.m.f),df.residual(nb.m.f))

#########################################################################################################################

#zeroinflated  poisson model
zin.poiss.m <- zeroinfl(tot~ log(GDPpercapita)+log(pop)+host+comm+ muslim + totprev1+offset(log(totmedals))|totprev1+log(GDPpercapita)+muslim, data=train )
summary(zin.poiss.m)

zin.poiss.m.f<- zeroinfl(tot~ log(GDPpercapita)+log(pop)+host+comm+ muslim + totprev1+offset(log(totmedals))| totprev1+log(GDPpercapita)+muslim, data=train )
summary(zin.poiss.m.f)

#zero inflated negative binomial
# as the estimated theta parameter is significant that means the zero inflated model is not appropraite (overdispersion)

zin.nb.m <- zeroinfl(tot~ log(GDPpercapita)+log(pop)+comm+host+ muslim + totprev1, data=train,dist="negbin")
summary(zin.nb.m)
#choose significant terms for the zero prediction part
zin.m.f<- zeroinfl(tot~ log(GDPpercapita)+log(pop)+comm + muslim + totprev1 , data=train,dist="negbin" )
summary(zin.m.f)

lrtest(zin.nb.m,zin.m.f)
#waldtest(zin.nb.m,zin.m.f)

# hurdl poisson
#hurdle.poiss.m <- hurdle(tot~ log(GDPpercapita)+log(pop)+host+soviet+ muslim + totprev1+offset(log(totmedals)), data=train )
#summary(hurdle.poiss.m)

#hurdle.nb.m <- hurdle(tot~ log(GDPpercapita)+log(pop)+host+comm+ muslim + totprev1, data=train ,dist="negbin")
#summary(hurdle.nb.m)


#hurdle.m.f<- hurdle(tot~ log(GDPpercapita)+log(pop)+comm + muslim + totprev1+host | log(GDPpercapita)+totprev1+muslim, data=train,dist="negbin" )
#summary(hurdle.m.f)

#waldtest(hurdle.nb.m,hurdle.m.f)
#lrtest(hurdle.nb.m,hurdle.m.f)

#########################################################################################################################
##comparison
full.model<- list("Pois"=poiss.m.f,"Qpois"=Qpoiss.m.f ,"Neg"=nb.m.f,"lm"=model_final5,"Zin"=zin.m.f)
#coeff of the models
sapply(full.model[-5],function(x) coef(x)[1:10])

## st error
cbind("Pois" = sqrt(diag(vcov(poiss.m.f))),sapply(full.model[-1],function(x) sqrt(diag(vcov(x)))[1:8]))

## log-liklihood
rbind(logLik = sapply(full.model, function(x) round(logLik(x), digits = 0)),Df = sapply(full.model, function(x) attr(logLik(x), "df")))

## AIC
rbind(AIC= sapply(full.model, function(x) AIC(x)))


##zero counts#

round(c("Train" = sum(train$tot < 1,na.rm=T) , "Pois" = sum(dpois(0,fitted(poiss.m.f)),na.rm=T) , "NB" = sum(dnbinom(0,mu= fitted(nb.m.f),size=nb.m.f$theta),na.rm=T) , "ZINB" = sum(predict(zin.m.f, type="prob")[,1],na.rm=T) ,"lm" = sum(predict(model_final5)<1, na.rm=T)))

round(c("Test" = sum(test$tot < 1,na.rm=T) , "Pois" = sum(dpois(0,fitted(poiss.m.f,test)),na.rm=T) , "NB" = sum(dnbinom(0,mu= fitted(nb.m.f,test),size=nb.m.f$theta), na.rm=T) , "ZINB" = sum(predict(zin.m.f, test,type="prob")[,1], na.rm=T),"lm"=  sum(predict(model_final5,test)<1,na.rm=T)))


#in-sample model error
fitted_model<- data.frame(cbind("Total Medals" = train$tot, poiss.diff = train$tot - fitted(poiss.m.f) , Qpoiss.diff = train$tot - fitted(Qpoiss.m.f)  , nb.diff = train$tot - fitted(nb.m.f) , zin.diff = train$tot - fitted(zin.m.f), lm.diff=train$tot - fitted(model_final5)))

MAE_train <- cbind(poiss.MAE = sum(abs(fitted_model$poiss.diff),na.rm=T)/nrow(fitted_model) , Qpoiss.MAE = sum(abs(fitted_model$Qpoiss.diff),na.rm=T)/nrow(fitted_model)  , nb.MAE = sum(abs(fitted_model$nb.diff),na.rm=T)/nrow(fitted_model) , zin.MAE = sum(abs(fitted_model$zin.diff),na.rm=T)/nrow(fitted_model),lm.MAE = sum(abs(fitted_model$lm.diff),na.rm=T)/nrow(fitted_model))
MFE_train <- cbind(poiss.MFE = sum(fitted_model$poiss.diff,na.rm=T)/nrow(fitted_model) , Qpoiss.MFE = sum(fitted_model$Qpoiss.diff,na.rm=T)/nrow(fitted_model)  , nb.MFE = sum(fitted_model$nb.diff,na.rm=T)/nrow(fitted_model) , zin.MFE = sum(fitted_model$zin.diff,na.rm=T)/nrow(fitted_model),lm.MFE = sum(fitted_model$lm.diff,na.rm=T)/nrow(fitted_model) )
RMSE_train <- cbind(poiss.RMSE = sqrt(sum((fitted_model$poiss.diff)^2,na.rm=T)/nrow(fitted_model)) , Qpoiss.RMSE = sqrt(sum((fitted_model$Qpoiss.diff)^2,na.rm=T)/nrow(fitted_model))  , nb.RMSE = sqrt(sum((fitted_model$nb.diff)^2,na.rm=T)/nrow(fitted_model)) , zin.RMSE = sqrt(sum((fitted_model$zin.diff)^2,na.rm=T)/nrow(fitted_model)) , lm.RMSE = sqrt(sum((fitted_model$lm.diff)^2,na.rm=T)/nrow(fitted_model)))

##MAE and MFE for the models out of sample
Predictions <- data.frame(cbind("Total Medals" = test$tot, "poiss.pred" =predict(poiss.m.f,test,type="response") ,"Qpoiss.pred" =predict(Qpoiss.m.f,test,type="response") ,"nb.pred" =predict(nb.m.f,test,type="response") , "zin.pred" = predict(zin.m.f,test,type="response") , "lm.pred" = predict(model_final5,test)))
Predictions <- Predictions %>% mutate (poiss.diff = Total.Medals - poiss.pred , Qpoiss.diff = Total.Medals - Qpoiss.pred  , nb.diff = Total.Medals - nb.pred , zin.diff = Total.Medals - zin.pred  , lm.diff = Total.Medals - lm.pred)

#MFE > 0, model tends to under-forecast
#MFE < 0, model tends to over-forecast
MAE <- cbind(poiss.MAE = sum(abs(Predictions$poiss.diff),na.rm=T)/nrow(Predictions) , Qpoiss.MAE = sum(abs(Predictions$Qpoiss.diff),na.rm=T)/nrow(Predictions)  , nb.MAE = sum(abs(Predictions$nb.diff),na.rm=T)/nrow(Predictions) , zin.MAE = sum(abs(Predictions$zin.diff),na.rm=T)/nrow(Predictions), lm.MAE = sum(abs(Predictions$lm.diff),na.rm=T)/nrow(Predictions))
MFE <- cbind(poiss.MFE = sum(Predictions$poiss.diff,na.rm=T)/nrow(Predictions) , Qpoiss.MFE = sum(Predictions$Qpoiss.diff,na.rm=T)/nrow(Predictions)  , nb.MFE = sum(Predictions$nb.diff,na.rm=T)/nrow(Predictions) , zin.MFE = sum(Predictions$zin.diff,na.rm=T)/nrow(Predictions) , lm.MFE = sum(Predictions$lm.diff,na.rm=T)/nrow(Predictions))
RMSE<- cbind(poiss.RMSE = sqrt(sum((Predictions$poiss.diff)^2,na.rm=T)/nrow(Predictions)) , Qpoiss.RMSE = sqrt(sum((Predictions$Qpoiss.diff)^2,na.rm=T)/nrow(Predictions))  , nb.RMSE = sqrt(sum((Predictions$nb.diff)^2,na.rm=T)/nrow(Predictions)) , zin.RMSE = sqrt(sum((Predictions$zin.diff)^2,na.rm=T)/nrow(Predictions)) , lm.RMSE = sqrt(sum((Predictions$lm.diff)^2,na.rm=T)/nrow(Predictions)))

prediction2012 <- data.frame("country" =train$country , "Actual medal" = train$tot , "lm Predicted medal" = round(fitted(model_final5),0),"lm.diff"= abs(round(train$tot - fitted(model_final5),0)) , "QPoiss Predicted medal" = round(fitted(Qpoiss.m.f),0) , "Qpoiss.diff" = abs(round(train$tot - fitted(Qpoiss.m.f),0)) ,"Zin predicted medal" = round(fitted(zin.m.f),0) , "Zin.diff" = abs(round(train$tot -fitted(zin.m.f),0) ))


prediction2016 <- data.frame("country" =test$country , "Actual medal" = test$tot , "lm Predicted medal" = round(predict(model_final5,test),0),"lm.diff"= abs(round(test$tot - predict(model_final5,test),0)) , "QPoiss Predicted medal" = round(predict(Qpoiss.m.f,test,typ="response"),0) , "Qpoiss.diff" = abs(round(test$tot - predict(Qpoiss.m.f,test,typ="response"),0)) , "Zin predicted medal" = round(predict(zin.m.f,test),0) , "Zin.diff" = abs(round(test$tot-predict(zin.m.f,test) ,0)))


prediction2012 <- prediction2012[order(prediction2012$Actual.medal,decreasing = T),]

prediction2016 <- prediction2016[order(prediction2016$Actual.medal,decreasing = T),]
head(prediction2012[order(prediction2012$lm.diff,decreasing = T),],10)







#Diagnostics of the model

d2<- data.frame(res=resid(nb.m.f,type="pearson"))

p1<- ggplot(d2,aes(sample=res)) + geom_point(stat="qq",col="green") + xlab("Theoratical Quantiles") + ylab("Quantiles")

p2<- ggplot(d2,aes(x=predict(nb.m.f,type="link"),y=res))+geom_point(col="green")+geom_hline(yintercept = 0)+xlab("linear Predictor") + ylab("Pearson Residuals")

p3<- ggplot(d2,aes(x=log(train$GDPpercapita),y=d2)) + geom_point(col="green") +xlab("log(GDPpercapita)") + ylab("Pearson Residuals")

p4<- ggplot(d2,aes(x=log(train$pop),y=d2)) + geom_point(col="green") +xlab("log(pop)") + ylab("Pearson Residuals")

p7 <- ggplot(d2,aes(x=train$totprev1,y=d2)) + geom_point(col="green") +xlab("Total previous medal") + ylab("Pearson Residuals")
  
grid.arrange(p1,p2,p7,p4,nrow=2)



## model does not fit the data well(large positive or negative residual)

p.resid <- resid(nb.m.f,type= "pearson")
pred.medal <- round(fitted(zin.m.f)[abs(p.resid) > 1],2)
cbind(train[abs(p.resid) > 1,c(1,11)],pred.medal)








cols<- c("Poiss"= "black" , "Qpoiss" = "green" , "Neg" = "red")
# the negative binomial curve is far from the Quassi curve. and that is indicator of the variance may not be proportional to the mean 
poiss.fig <- data.frame(train,link=predict(poiss.m.f, type="link"), fit=predict(poiss.m.f,type="response"), pearson=residuals(poiss.m.f,type="pearson"),resid=residuals(poiss.m.f,type="response"), residSqr=residuals(poiss.m.f,type="response")^2)

c2<-ggplot(data=poiss.fig, aes(x=fit, y=residSqr)) + geom_point() + geom_abline(intercept = 0, slope = 1) + geom_abline(intercept = 0, slope = summary(Qpoiss.m.f)$dispersion, color="green") + theme_bw()+ggtitle("Square of residuals vs fitted values")+theme(plot.title=element_text(hjust=0.5))


nb.fig <- data.frame(train, link=predict(nb.m.f, type="link"), fit=predict(nb.m.f, type="response"), pearson=residuals(nb.m.f,type="pearson"), resid=residuals(nb.m.f,type="response"), residSqr=residuals(nb.m.f,type="response")^2 )

c1<- ggplot(data=nb.fig, aes(x=fit, y=residSqr)) + geom_point() + geom_abline(intercept = 0, slope = 1) + geom_abline(intercept = 0, slope = summary(Qpoiss.m.f)$dispersion, color="green") + stat_function(fun=function(fit){fit + fit^2/summary(nb.m.f)$theta}, color="red") + theme_bw() +theme(plot.title=element_text(hjust=0.5))+ggtitle("Square of residuals vs fitted values")+scale_color_manual("Models", values=c(black="black",blue="blue",red="red",green="green"),
                                                                                                                                                                                                                                                                                                                                                                                                          labels=c("poiss","loess line","Neg","Qpoiss"))+ stat_smooth(method="loess", se = FALSE) 

grid.arrange(c1,c2,nrow=1)













2000Australia 58 49 46 35 29
2004Greece 13 16 4 2 6
2008china 59 63 100 88 70
2012United Kingdom 28 30 47 65 67 
2016Brazil 12 10 15 17 19
