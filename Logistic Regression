#full model
install.packages("stargazer")
library(stargazer)
library('MASS')
df1=m7[,c(14,25,26,27,28,29,30,31,32,33,34)]
df1$Affiliation=as.factor(df1$Affiliation)
colnames(df1)[c(2:11)]=c('solar','coal','ng','wind','bio','hydro','wood','petrol','nuclear','other')
df1$Affiliation=as.factor(df1$Affiliation)
logit1=glm(Affiliation ~ coal+wind+solar+bio+hydro+petrol+nuclear+wood+ng+other,family=binomial(link='logit'),data=df1);logit1
summary(logit1)
logit1$fitted.values
#variable selection
stepboth1=stepAIC(logit1,k=2,direction='both',scope = list(upper=~.,lower=~1)) 
drop1(logit1,test = 'LRT') #keep coal bio 
anova(logit1,test='LRT') #keep coal bio 
#prediction 1
p1 <- predict(logit1,newdata=df1,type='response');p1
p1 <- ifelse(p1 > 0.5,1,0);p1
p1=as.factor(p1)
str(p1)
levels(df1$Affiliation) <- c("0", "1")
misClasificError1 <- mean(p1 != df1$Affiliation);misClasificError1
print(paste('Accuracy',1-misClasificError1)) #86%

#reduced model
logit4=glm(Affiliation ~ coal+bio,family=binomial(link='logit'),data = df1)
summary(logit4)
logit4$fitted.values #explore which states are being misclassifed
#variable selection
stepboth3=stepAIC(logit2,k=2,direction='both',scope = list(upper=~.,lower=~1))  
drop1(logit4,test = 'LRT') #keep variables coal and bio
anova(logit4,test='LRT') #keep variables coal and bio
#prediction 2
p3 <- predict(logit4,newdata=df1,type='response');p3
p3 <- ifelse(p3 > 0.5,1,0);p3
p3=as.factor(p3)
str(p3)
levels(df1$Affiliation) <- c("0", "1")
misClasificError3 <- mean(p3 != df1$Affiliation);misClasificError3
print(paste('Accuracy',1-misClasificError3)) #86%

#check for outliers
N=length(m7$STATE)
state=1:N
hat.bw=hatvalues(logit4)
rstudent.bw=rstudent(logit4)
par(mfrow=c(2,2))
plot(hat.bw,rstudent.bw)
dffits.bw=dffits(logit4)
plot(state,dffits.bw,type = 'l')
cov.bw=covratio(logit4)
plot(state,cov.bw,type = 'l')
cook.bw=cooks.distance(logit4)
plot(state,cook.bw,type = 'l') #seem to be a few outlying states according to cook.bw

#identify observations-->observations 6 (CO), 14 (ID),and 33 (NM)look to be influential obs.
#This makes sense as CO and NM are blue states that dont produce much biomass (41st,37th) and
#ID produces the least amount of coal and most biomass per customer out of any red state (21st,47th). 
par(mfrow=c(1,1))
plot(state,cook.bw)
identify(state,cook.bw)
cook.bw

#create boxplots to compare outliers with group average

#coal for outliers compared to nations average
plot3=ggplot(data = m7[6,],aes(x=STATE,y=sqrt_coal))+geom_bar(stat='identity',fill='blue',width = 0.5);plot3
plot4=plot3+geom_bar(data=m7[33,],aes(x=STATE,y=sqrt_coal),stat = 'identity',fill='blue',width = 0.5);plot4
plot5=plot4+geom_bar(data=m7[14,],aes(x=STATE,y=sqrt_coal),stat = 'identity',fill='red',width = 0.5)+geom_hline(data=m7,yintercept = mean(m7[,16]))+xlab('States')+ylab('Coal per Customer (√MWH)')+ggtitle('Outlying States Comparison to National Average: Coal (2016)')+theme(plot.title = element_text(hjust = 0.5));plot5
aggregate(m7$sqrt_coal,list(m7$Affiliation),mean)
plot5=plot4+geom_bar(data=m7[14,],aes(x=STATE,y=sqrt_coal),stat = 'identity',fill='red',width = 0.5)+geom_hline(data=m7,yintercept =  c(1.628),list(m7),color='blue')+xlab('States')+ylab('Coal per Customer (√MWH)')+ggtitle('Comparison to Affiliaton Average: Coal (2016)')+theme(plot.title = element_text(hjust = 0.5));plot5
plot6=plot5+geom_hline(data=m7,yintercept =  c(3.981),list(m7),color='red');plot6

#biomass in idaho compared to nations average
plot9=ggplot(data=m7[14,],aes(x=STATE,y=sqrt_bio))+geom_bar(stat='identity',fill='red',width = 0.5);plot9
#bio for co and nm
plot10=plot9+geom_bar(data=m7[6,],aes(x=STATE,y=sqrt_bio),stat='identity',fill='blue',width = 0.5);plot10
plot11=plot10+geom_bar(data=m7[33,],aes(x=STATE,y=sqrt_bio),stat='identity',fill='blue',width = 0.5);plot11
#bio for outliers compared to nations average
plot12=plot11+geom_hline(data=m7,yintercept = mean(m7[,19]))+xlab('States')+ylab('Bio per Customer (√MWH)')+ggtitle('Outlying States Comparison to Affiliation Averages: Bio (2016)')+theme(plot.title = element_text(hjust = 0.5));plot12
aggregate(m7$sqrt_bio,list(m7$Affiliation),mean)
plot12=plot11+geom_hline(data=m7,yintercept = 0.201,color='red')+xlab('States')+ylab('Bio per Customer (√MWH)')+ggtitle('Comparison to Affiliation Averages: Bio (2016)')+theme(plot.title = element_text(hjust = 0.5));plot12
plot13=plot12+geom_hline(data=m7,yintercept = 0.4299,color='blue');plot13

#new dataset with percentile ranks
m10=m7[,c(1,14,25,28)]
percentile_df=mutate(m10,percent_rank_coal=ntile(m10$sqrt_coal,100))
percentile_df=mutate(percentile_df,percent_rank_bio=ntile(percentile_df$sqrt_bio,100))

#create tables for latex
stargazer(logit4,title="Results", align=F)
model=factor(c('Reduced Model (bio+coal'))
misclassification_rate=c(misClasificError3)
combine=data.frame(model,misclassification_rate)
library('xtable')
xtable(combine)
