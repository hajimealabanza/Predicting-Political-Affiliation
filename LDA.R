###discriminant analysis
library('MASS')
library('xtable')

#####all energy sources#####
output1=lda(Affiliation ~ coal+wind+solar+bio+hydro+petrol+nuclear+wood+ng+other,data=df1);output1
output2=predict(output1,newdata = df1[,c(2:11)])$class;output2
table(output2,df1$Affiliation)
output3=lda(Affiliation ~ coal+wind+solar+bio+hydro+petrol+nuclear+wood+ng+other,data=df1,CV=TRUE);output3
lda1=table(output3$class,df1$Affiliation);lda1 #73% accuracy
xtable(lda1)

#reduced model (coal bio)
output7=lda(df1$Affiliation~coal+bio,data=df1);output7
output8=predict(output7,newdata = df1[,c(3,6)])$class;output8
table(output8,df1$Affiliation)
output9=lda(df1$Affiliation~coal+bio,data=df1,CV=TRUE);output9
lda2=table(output9$class,df1$Affiliation);lda2 #80% accuracy, lowest accuracy out of all models probably because division isnt linear
xtable(lda2)

#which states are being misclassified (model 2)?
pred_redvsblue=output9$class
as.data.frame(pred_redvsblue)
actual_redvsblue=as.data.frame(m7[,c(1,14)])
actualvspred=cbind(pred_redvsblue,actual_redvsblue)
