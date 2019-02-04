#random forest
library('randomForest')
library('caret')

#full model
par(mfrow=c(1,1))
rf1=randomForest(df1[,2:11],df1$Affiliation,mtry=3,data=df1,importance = TRUE,proximity = TRUE,ntree = 200) #first use default values for ntree and mtry
print(rf1) #oob error 17.65%

#error rate of random forest
plot(rf1) #best under 100

#tune mtry
par(mfrow=c(1,1))
tuneRF(df1[,-c(1)],df1[,c(1)],stepFactor = 0.5,plot = TRUE,ntreeTry = 200,trace = TRUE,improve = 0.4) #anything above 3 mtry is the same meaning that all variables
varImpPlot(rf1) #bio and coal look to be only important variables
