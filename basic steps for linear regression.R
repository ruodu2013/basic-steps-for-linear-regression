
setwd=("C:/Users/Ruo/Documents/R")
fueleff<-read.csv("C:/Users/Ruo/Documents/R/FuelEfficiency.csv")
head(fueleff)
cor(fueleff)
plot(GPM~WT,data=fueleff)
plot(GPM~DIS,data=fueleff)
model.full<-lm(GPM~.,data=fueleff)
summary(model.full)

library(leaps)
x=fueleff[,2:7]
y=fueleff[,1]
leaps=regsubsets(GPM~.,data=fueleff,nbest=2,nvmax=8)# 2 best model for each number of predictors
summary(leaps)


plot(leaps, scale="adjr2")

out=summary(leaps)
tab=cbind(out$which,out$rsq,out$adjr2,out$cp)
tab

library(MASS)
full.model<-lm(GPM~.,data=fueleff)
reduced.model<-step(full.model,direction="backward")

min.model <- lm(GPM~1,data=fueleff)
forward.model<-step(min.model,direction="forward",scope=(~MPG + WT + DIS + NC + HP + ACC + ET))

model2<-lm(GPM~MPG + NC + HP + ET,data=fueleff)
summary(model2)

 
plot(model2,which=c(1:4))
par(mfrow = c(2,2)) 

