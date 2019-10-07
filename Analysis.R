###3. Building Models###

##3.1 Logistic Regression##

#load packages#
install.packages("stargazer")
library('stargazer')
library('MASS')
library('car')
library('caret')

#Create training and test sets#
Train <- createDataPartition(m4$Affiliation, p=0.80, list=FALSE) 
m4_train <- m4[Train, ]
m4_test <- m4[-Train, ]

#first run full model#
logit1=glm(Affiliation ~ coal+wind+solar+bio+hydro+petrol+nuclear+wood+ng+other,family=binomial(link='logit'),data=m4_train);logit1
summary(logit1) #only coal and petrol statistically significant
logit1$fitted.values
vif1=vif(logit1);vif1 #all vif lower than 10, so no real issue with multicollinearity

#variable selection 1#
drop1(logit1,test = 'LRT') #keep coal bio ng
anova(logit1,test='LRT') #keep coal bio ng
drop1(logit1,test = 'Rao') #keep bio petrol
anova(logit1,test='Rao') #keep bio coal

#update model 1#
logit2=glm(Affiliation ~ coal+bio+ng+petrol,family=binomial(link='logit'),data = m4_train)
summary(logit2) #coal bio petrol statistically significant

#variable selection 2#
drop1(logit2,test = 'LRT') #keep coal bio 
anova(logit2,test='LRT') #keep coal bio 
drop1(logit2,test = 'Rao') #bio petrol
anova(logit2,test='Rao') #coal bio petrol

#update model 2, drop ng#
logit3=glm(Affiliation ~ coal+bio+petrol,family=binomial(link='logit'),data = m4_train)
summary(logit3) 

#variable selection 3#
drop1(logit3,test = 'LRT') #keep variables coal and bio
anova(logit3,test='LRT') #keep variables coal and bio
drop1(logit3,test = 'Rao') #bio petrol
anova(logit3,test='Rao') #keep all

#since no variables can be decisively dropped, logit3 model is final (results similar all 3 times)#
summary(logit3) #bio always significant at 5% level
                #petrol is usually significant at either 5% or 10% level (less stable)
                #coal is sometimes significant at the 10% level (less stable)

#prediction (average on all 3 training sets)#
p1 <- predict(logit3,newdata=m4_train,type='response')
p1 <- ifelse(p1 > 0.5,1,0)
p1=as.factor(p1)
str(p1)
levels(m4_train$Affiliation) <- c("0", "1")
misClasificError <- mean(p1 != m4_train$Affiliation);misClasificError
confusion <- table(p1,m4_train$Affiliation);confusion #in all 3 cases, false pos. > false neg.
print(paste('Accuracy',1-misClasificError)) #90% average accuracy (1: 90%, 2: 88%, 3: 92%)

#prediction (average on all 3 test sets)#
p2 <- predict(logit3,newdata=m4_test,type='response')
p2 <- ifelse(p2 > 0.5,1,0)
p2=as.factor(p2)
levels(m4_test$Affiliation) <- c("0", "1")
misClasificError <- mean(p2 != m4_test$Affiliation);misClasificError
confusion <- table(p2,m4_test$Affiliation);confusion #in all 3 cases, false pos. > false neg.
print(paste('Accuracy',1-misClasificError)) #84% average accuracy (1: 88%, 2: 88%, 3: 77%))

#k-folds cross validation#
ctrl <- trainControl(method = "repeatedcv", number = 5, savePredictions = TRUE)
mod_fit <- train(Affiliation ~ coal+bio+petrol,  
                 data=m4, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)
summary(mod_fit) #output similar to previous 
p3 = predict(mod_fit, newdata=m4_test,type = 'prob');p3
p3 <- ifelse(p3$`Blue State` > 0.5,1,0)
p3=as.factor(p3);p3
levels(m4_test$Affiliation) <- c("0", "1")
misClasificError <- mean(p3 != m4_test$Affiliation);misClasificError
confusion <- table(p3,m4_test$Affiliation);confusion 
print(paste('Accuracy',1-misClasificError)) #88% accuracy

#Analyze quality of model#
install.packages('generalhoslem') #Hosmer-Lemeshow Stat
library('generalhoslem')
generalhoslem::logitgof(m4_train$Affiliation,fitted(logit3)) #p-value of 0.81 shows no evidence that predicted different from observed

install.packages('fmsb') #Neagelkerke
library('fmsb')
NagelkerkeR2(logit3) #R2 value of 0.72 indicates a model with decent prediction

OptimisedConc=function(model) #Concordance measure
{
  Data = cbind(model$y, model$fitted.values) 
  ones = Data[Data[,1] == 1,]
  zeros = Data[Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  return(list("Percent Concordance"=PercentConcordance,"Percent Discordance"=PercentDiscordance,"Percent Tied"=PercentTied,"Pairs"=Pairs))
}
OptimisedConc(logit3) #model shows prediction of 95%, which is very good

#check for outliers#
N=length(m4_train$STATE) #global influence plot
STATE=1:N
hat.bw=hatvalues(logit3)
rstudent.bw=rstudent(logit3)
par(mfrow=c(2,2))
plot(hat.bw,rstudent.bw)
dffits.bw=dffits(logit3)
plot(STATE,dffits.bw,type = 'l')
cov.bw=covratio(logit3)
plot(STATE,cov.bw,type = 'l')
cook.bw=cooks.distance(logit3)
plot(STATE,cook.bw,type = 'l')
par(mfrow=c(1,1))
plot(STATE,cook.bw)
identify(STATE,cook.bw)
cook.bw

#observations 12 (HI), 14 (ID),and 33 (NM) look to be potential influential obs.
#Hawaii produces a lot of petrol for a blue state
#NM is a blue state that doesnt produce much biomass (37th) 
#ID produces the least amount of coal (21st) and most biomass (47th) per customer out of any red state

#create boxplots to compare outliers of ID and NM#
#coal for outliers compared to nations average#
plot3=ggplot(data = m6[33,],aes(x=STATE,y=coal))+geom_bar(stat='identity',fill='blue',width = 0.5);plot3
aggregate(m6$coal,list(m6$Affiliation),mean)
plot4=plot3+geom_bar(data=m6[14,],aes(x=STATE,y=coal),stat = 'identity',fill='red',width = 0.5)+geom_hline(data=m6,yintercept =  c(1.71),list(m6),color='blue')+xlab('States')+ylab('Coal per Customer (√MWH)')+ggtitle('Comparison to Affiliaton Average: Coal (2016)')+theme(plot.title = element_text(hjust = 0.5));plot4
plot5=plot4+geom_hline(data=m6,yintercept =  c(3.981),list(m6),color='red');plot5

#biomass in idaho compared to nations average#
plot6=ggplot(data=m6[14,],aes(x=STATE,y=bio))+geom_bar(stat='identity',fill='red',width = 0.5);plot6
#bio for co and nm#
plot7=plot6+geom_bar(data=m6[33,],aes(x=STATE,y=bio),stat='identity',fill='blue',width = 0.5);plot7
#bio for outliers compared to nations average#
aggregate(m6$bio,list(m7$Affiliation),mean)
plot8=plot7+geom_hline(data=m6,yintercept = 0.19,color='red')+xlab('States')+ylab('Bio per Customer (√MWH)')+ggtitle('Comparison to Affiliation Averages: Bio (2016)')+theme(plot.title = element_text(hjust = 0.5));plot7
plot9=plot8+geom_hline(data=m6,yintercept = 0.4286,color='blue');plot9

#new dataset with percentile ranks#
m7=m4[,c(1,3,5,8)]
percentile_df=mutate(m7,percent_rank_coal=ntile(m7$coal,100))
percentile_df=mutate(percentile_df,percent_rank_bio=ntile(percentile_df$bio,100))

#create tables for latex#
stargazer(logit3,title="Results", align=F)
model=factor(c('Reduced Model (bio+coal'))
misclassification_rate=c(misClasificError)
combine=data.frame(model,misclassification_rate)
library('xtable')
xtable(combine)


##3.2 random forest##
library('randomForest')
library('caret')

#build model#
par(mfrow=c(1,1))
rf1=randomForest(m4[,4:13],m4$Affiliation,mtry=3,data=m4,importance = TRUE,proximity = TRUE,ntree = 200) #first use default values for ntree and mtry
print(rf1) #oob error 19.61%, so 80% accuracy

#error rate of random forest
plot(rf1) #error stabilizes after 100 trees

#tune mtry
par(mfrow=c(1,1))
tuneRF(m4[,-c(1)],m4[,c(1)],stepFactor = 0.5,plot = TRUE,ntreeTry = 100,trace = TRUE,improve = 0.4) #anything above 3 mtry is the same meaning that all variables
varImpPlot(rf1) #bio and coal look to be only important variables

###4.Summarize###

##build table combining all models##
Logistic_Error=c(0.16)
RandomForest_Error=c(0.20)
KNN_Error=c(0.19)
all_misclass=data.frame(Model,Logistic_Error,RandomForest_Error,KNN_Error)
library('xtable')
xtable(all_misclass)
