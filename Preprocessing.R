###1.Preprocessing Data###

##import data##
library('readxl')
library('tidyr')
customer=read_excel('/Users/halabanz/Desktop/Applied Multivariate/customers.xlsx')
gen=read_excel('/Users/halabanz/Desktop/Applied Multivariate/generation.xlsx')
redvsblue=read_excel('/Users/halabanz/Desktop/Applied Multivariate/redvsblue.xlsx')
re=read_excel('/Users/halabanz/Desktop/Applied Multivariate/re.xlsx')

##clean customer data (2016)##
str(customer) #look at structure of dataset
customer2=customer[customer$Year==2017 & customer$`Industry Sector Category`=='Total Electric Industry',-c(3:8)] #select row for: 2016 entries and Total Electric Industry
sort(customer$State) #sort by alphabetical order, asc

##clean generation data (2016)##
str(gen)
gen2=gen[gen$YEAR==2017 & gen$`TYPE OF PRODUCER`=='Total Electric Power Industry',-c(3)] 
gen3=spread(gen2,`ENERGY SOURCE`,`GENERATION (Megawatthours)`) #reshape from long to wide with energy source as one column and gen in other
colnames(gen3)[5]='Hydroelectric'
sort(gen3$STATE)
summary(gen3)
gen4=gen3[,-c(4,10,12,14)] #remove columns not used in analysis

##merge data##
m1=merge(gen4,customer2,by.x = 'STATE',by.y = 'State') #merge generation and customer data
m2=m1[,-c(13)] #get rid of duplicate year
colnames(m2)[13]='Total_Customers' #rename column
m3=merge(m2,redvsblue,by.x='STATE',by.y = 'STATE') #merge m2 and data for political affilation
summary(m3)

##impute missing values to 0##
m3[is.na(m3)] <- 0 #after doing some research, data seems to be missing because generation was at 0 or close to it
str(m3)
summary(m3)

##transform variables (calculate total generation per customer)#
m4=m3 #making sure that data (m3) is available in its original form

#first create function for total gen/cust
gen_per_cust=function(col) {
  col/m4$Total_Customers
}

#create new columns for generation per customer (all sources)
for(i in 3:12) {
  m4[[paste(substr(colnames(m4)[[i]],1,7),'per_customer',sep="_")]] = gen_per_cust(m4[[i]])
}

m4=m4[,-c(3:13)] #drop old variables
colnames(m4)[c(4:13)]=c('coal','hydro','ng','nuclear','other','bio','petrol','solar','wind','wood') #change names

m4$Affiliation=as.factor(m4$Affiliation) #change affiliation from 2 to 4 to 0 to 1
levels(m4$Affiliation)=c('Red State','Blue State')
contrasts(m4$Affiliation)

str(m4) #check structure of final dataframe
m4[,4:13] <- lapply(m4[,4:13], as.numeric) #change from character to numeric
