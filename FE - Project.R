setwd("D:/Projects")
FE2010<-read.csv("FE2010.csv")

head(FE2010)

dim(FE2010)
summary(FE2010)
str(FE2010)

sum(is.na(FE2010))

library(ggplot2)

# Using the scatter plot to visualise the relationship
plot(FE2010$EngDispl,FE2010$FE)

# to draw the best fit line
scatter.smooth(x=FE2010$EngDispl,y=FE2010$FE, Main="EngDisp ~ FE",col="Red")



# for skewness function: Density plot: To see the distribution of the predictor variable. 
#Ideally, a close to normal distribution (a bell shaped curve),
#without being skewed to the left or right is preferred.
library(e1071)  
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(FE2010$EngDispl), main="Density plot : EngDispl", ylab= "Frequency",
             sub=paste("Skewness:", round(e1071::skewness(FE2010$EngDispl), 2)))

polygon(density(FE2010$EngDispl),col = "red")

plot(density(FE2010$FE), main= "Density plot: FE", ylab="frequency", sub=paste("skewness:", round(e1071::skewness(FE2010$FE), 2)))
polygon(density(FE2010$FE), col = "red")

t.test(FE2010$EngDispl)


# using the Box plot to check for outliers
par(mfrow=c(1,2))# divid graph into 2 columns
boxplot(FE2010$EngDispl, main="EngDispl") #boxplot for Eng disp
boxplot(FE2010$FE, main="FE") #Boxplot for FE


# Treating Outlier
## Labeling Outliers 
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

library(dplyr)
FE2010%>%
  mutate(outlier = ifelse(is_outlier(FE2010$EngDispl), FE2010$EngDispl, as.numeric(NA))) %>%
  ggplot(.,aes(1,FE2010$EngDispl)) + geom_boxplot(fill = "steelblue",outlier.colour = "red",
                                             outlier.shape = 2)+
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)


# Creating valus for UB and LB
UB<-quantile(FE2010$EngDispl,0.75,na.rm = T) + 1.5*IQR(FE2010$EngDispl,na.rm = T)
LB<-quantile(FE2010$EngDispl,0.25,na.rm = T) - 1.5*IQR(FE2010$EngDispl,na.rm = T)


#count outliers
length(FE2010$EngDispl[FE2010$EngDispl<LB | FE2010$EngDispl>UB])

#Assigning values

FE2010$EngDispl[FE2010$EngDispl>UB] <- UB
FE2010$EngDispl[FE2010$EngDispl<LB] <- LB

boxplot(FE2010$EngDispl)

#to check the correlation between EngDispl and EF
cor(FE2010$EngDispl,FE2010$FE)
cr<-cor(FE2010)
# Cor plot to visualize the relation
library(corrplot)
par(mfrow=c(1,2))
corrplot(cr,type = "lower")
corrplot(cr,method = "number")

install.packages("GGally")
library(GGally)
library(dplyr)
ggscatmat(FE2010,columns = 1:ncol(FE2010))



#Build a liner regression model
FE_model = lm(FE~EngDispl, data = FE2010)
FE_model

summary(FE_model)

plot(FE_model)


FE_mod_log<-lm(log(FE)~EngDispl,data = FE2010)
summary(FE_mod_log)
plot(FE_mod_log)

#Normality test
hist(FE_mod_log$residuals)

# Build Liner regression with all the varibles
FE_all_mod<-lm(FE~., data = FE2010)
FE_all_mod
summary(FE_all_mod)

# To check multi-collinarity Test
library(car)
corl<- cor(FE2010)
corl
vif(FE_all_mod)
sqrt(vif(FE_all_mod))
vif(FE_all_mod)>5


#Train and Test data
library(caTools)

set.seed(25)

sample<-sample.split(FE2010,SplitRatio = 0.70)
train1<-subset(FE2010,sample=TRUE)
test<-subset(FE2010,sample=FALSE)

#Fit the model on training data and predict on test data

train_mod<-lm(FE~EngDispl,data = train1)

pred_FE2010<-predict(train_mod,test)
head(pred_FE2010)
summary(train_mod)

final<-cbind(test$FE,pred_FE2010)

#Calculate prediction accuracy and error rates

actual_pred<-data.frame(cbind(actuals=test$FE, Predictions=pred_FE2010)) # make actuals_predicteds dataframe.

head(actual_pred)

# MAPE Calculation
mape <- mean(abs((actual_pred$Predictions - actual_pred$actuals))/actual_pred$actuals) 
mape
Model_rate = 100-mape
Model_rate

RMSE(train1$FE,pred_FE2010)


#==========================
pred<-predict(FE_model)
F2010<-FE2010
F2010$Predicted<-NA
F2010$Predicted<-pred
F2010$error<-FE_model$residuals

FE2011<-read.csv("FE2011.csv")

pred2011<-predict(FE_model,FE2011)
head(pred2011)

result<-cbind(FE2011$FE,pred2011)

actual_pred2011<-data.frame(cbind(actual=FE2011$FE,predictions=pred2011))
  
head(actual_pred2011)  
  
  
  
  


#Compare models: Anova : Null Hypothisis say all populaton means are same.
library(DAAG)
m1<- lm(FE~EngDispl, data = FE2010)
m2<- lm(FE~EngDispl + NumCyl, data = FE2010)
anovatest<-Anova(m1,m2)

#post hoc test or TURKEY test
#Turkey test for pair wise comparision and same group size
TukeyHSD(anovatest)


#Auto corelation test. if P-value is >0.05 then there is no problem of auto correlation.
#then we can go to HETEROSCEDASCITY test  ncvTest(FE_all_mod) , spreadLevelplot(FE_all_mod)
durbinWatsonTest(FE_all_mod)


