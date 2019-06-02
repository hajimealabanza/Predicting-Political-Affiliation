#import data
library('readxl')
library('tidyr')
customer=read_excel('/Users/halabanz/Desktop/Applied Multivariate/customers.xlsx')
gen=read_excel('/Users/halabanz/Desktop/Applied Multivariate/generation.xlsx')
redvsblue=read_excel('/Users/halabanz/Desktop/Applied Multivariate/redvsblue.xlsx')

#clean customer data
str(customer)
customer2=customer[customer$Year==2016 & customer$`Industry Sector Category`=='Total Electric Industry',-c(3:8)]
sort(customer$State)

#clean generation data
str(gen)
gen2=gen[gen$YEAR==2016 & gen$`TYPE OF PRODUCER`=='Total Electric Power Industry',-c(3)]
gen3=spread(gen2,`ENERGY SOURCE`,`GENERATION (Megawatthours)`)
colnames(gen3)[5]='Hydroelectric'
sort(gen3$STATE)
summary(gen3)
gen4=gen3[,-c(4,10,12,14)]

#merge data
m1=merge(gen4,customer2,by.x = 'STATE',by.y = 'State')
m2=m1[,-c(13)]
colnames(m2)[13]='Total_Customers'
m4=merge(m2,redvsblue,by.x='STATE',by.y = 'STATE')
summary(m4)

#impute missing values to 0
m4$Other[m4$Other<0]=0
m4[is.na(m4)] <- 0
str(m4)
summary(m4)

#transform variables

m5=m4

#calulate total generation per customer

m5$solar_gen_customer=(m5$`Solar Thermal and Photovoltaic`/m5$Total_Customers)
m5$coal_gen_customer=(m5$Coal/m5$Total_Customers)
m5$ng_gen_customer=(m5$`Natural Gas`/m5$Total_Customers)
m5$wind_gen_customer=(m5$Wind/m5$Total_Customers)
m5$bio_gen_customer=(m5$`Other Biomass`/m5$Total_Customers)
m5$hydro_gen_customer=(m5$Hydroelectric/m5$Total_Customers)
m5$wood_gen_customer=(m5$`Wood and Wood Derived Fuels`/m5$Total_Customers)
m5$petrol_gen_customer=(m5$Petroleum/m5$Total_Customers)
m5$nuclear_gen_customer=(m5$Nuclear/m5$Total_Customers)
m5$other_gen_customer=(m5$Other/m5$Total_Customers)

#sqrt variables

m6=m5

m6$sqrt_solar=sqrt(m6$solar_gen_customer)
m6$sqrt_coal=sqrt(m6$coal_gen_customer)
m6$sqrt_ng=sqrt(m6$ng_gen_customer)
m6$sqrt_wind=sqrt(m6$wind_gen_customer)
m6$sqrt_bio=sqrt(m6$bio_gen_customer)
m6$sqrt_hydro=sqrt(m6$hydro_gen_customer)
m6$sqrt_wood=sqrt(m6$wood_gen_customer)
m6$sqrt_petrol=sqrt(m6$petrol_gen_customer)
m6$sqrt_nuclear=sqrt(m6$nuclear_gen_customer)
m6$sqrt_other=sqrt(m6$other_gen_customer)

scat1<-m6[,c(24:33)]
pairs(scat1,main='Paired scatterplots of dataset')
cor3=cor(scat1)

m7=m6
str(m7)
summary(m7)
m7$Affiliation=as.factor(m7$Affiliation)
levels(m7$Affiliation)=c('Red State','Blue State')
contrasts(m7$Affiliation)
