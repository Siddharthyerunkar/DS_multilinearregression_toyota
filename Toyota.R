
library(car)
tota<-Toyota[,c(3,4,7,9,13,14,15,16,17)]
tota1<-na.omit(tota)
pairs(tota1)
cor(tota1)

model.tota<-lm(Price~.,data = tota1)
summary(model.tota)

car::vif(model.tota)
alias(model.tota)

library(MASS)
stepAIC(model.tota)

plot(model.tota)
car::residualPlots(model.tota)
car::avPlots(model.tota)
car::qqPlot(model.tota)
influenceIndexPlot(model.tota)

####Iteration 1 
#Remove 79th observation
tota1["Age2"]<-tota1$Age_08_04*tota1$Age_08_04

tota2<-tota1[-79,]
model1<-lm(Price~., data = tota2)
summary(model1)
plot(model1)
car::residualPlots(model1)
car::avPlots(model1)
car::qqPlot(model1)
influenceIndexPlot(model1)

####Iteration 2 
#Remove 79,107,109th observation
tota1["HP2"]<-tota1$HP*tota1$HP

tota3<-tota1[-c(79,107,109),]

model2<-lm(Price~., data = tota3)
summary(model2)
plot(model2)
car::residualPlots(model2)
car::avPlots(model2)
car::qqPlot(model2)
influenceIndexPlot(model2)

####Iteration 3 
#Remove 79,107,109th observation
tota1["tax2"]<-tota1$Quarterly_Tax*tota1$Quarterly_Tax

tota4<-tota1[-c(79,107,109),]

model3<-lm(Price~., data = tota4)
summary(model3)
plot(model3)
car::residualPlots(model3)
car::avPlots(model3)
car::qqPlot(model3)
influenceIndexPlot(model3)

