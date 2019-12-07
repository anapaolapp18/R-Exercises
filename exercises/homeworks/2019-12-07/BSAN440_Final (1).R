#load the data
library(tidyverse)
suicide.dat<- read_csv("master.csv")
suicide.dat

#preliminary EDA
view(suicide.dat)
dim(suicide.dat)
names(suicide.dat)
summary(suicide.dat)

#cleaning

#renaming to shorter variable
suicide.dat<- rename(suicide.dat, suicide_rate="suicides/100k pop")
suicide.dat<- rename(suicide.dat, HDI="HDI for year")

#inputting missing values
suicide_backup <- suicide
suicide = suicide_backup
mean_HDI <- mean(suicide.dat$HDI, na.rm = T)
suicide.dat[is.na(suicide.dat$HDI),"HDI"] = mean_HDI


#EDA
#country, year, sex, age, generation, HDI for year, gdp_for_year ($) and gdp_per_capita ($)

#GDP Per Capita:

ggplot(suicide.dat, aes(log(gdp_per_capita), log(suicide_rate)))+
  geom_point()

cor(suicide.dat$gdp_per_capita,suicide.dat$suicide_rate,use="complete.obs")

tab.per_capita <- table(suicide.dat$gdp_per_capita, suicide.dat$suicide_rate, dnn = c("Per capita", "SUICIDE_RATE"))
per_capita_cst=chisq.test(tab.per_capita)


#GDP for year

ggplot(suicide.dat, aes(log(gdp_for_year), log(suicide_rate)))+
  geom_point()

cor(suicide.dat$gdp_for_year,suicide.dat$suicide_rate,use="complete.obs")

tab.for_year <- table(suicide.dat$gdp_for_year, suicide.dat$suicide_rate, dnn = c("For Year", "SUICIDE_RATE"))
for_year_cst=chisq.test(tab.for_year)


# Country

ggplot(suicide.dat, aes(x=country, y=suicide_rate))+
  stat_summary(fun.y = "mean", geom="bar", fill="blue", na.rm = T)+
  ylab("Suicide_rate")

tab.country <- table(suicide.dat$country, suicide.dat$suicide_rate, dnn = c("COUNTRY", "SUICIDE_RATE"))
country_cst=chisq.test(tab.country)
view(tab.country)

## year

ggplot(suicide.dat, aes(year, suicide_rate))+
  geom_point()

cor(suicide.dat$year,suicide.dat$suicide_rate,use="complete.obs")

tab.year <- table(suicide.dat$year, suicide.dat$suicide_rate, dnn = c("year", "SUICIDE_RATE"))
year_cst=chisq.test(tab.year)

## sex

ggplot(suicide.dat, aes(x=sex, y=suicide_rate))+
  stat_summary(fun.y = "mean", geom="bar", fill="blue", na.rm = T)+
  ylab("Suicide_rate")

tab.sex <- table(suicide.dat$sex, suicide.dat$suicide_rate, dnn = c("SEX", "SUICIDE_RATE"))
sex_cst=chisq.test(tab.sex)
view(tab.sex)

## Age

ggplot(suicide.dat, aes(x=age, y=suicide_rate))+
  stat_summary(fun.y = "mean", geom="bar", fill="blue", na.rm = T)+
  ylab("Suicide_rate")

tab.age <- table(suicide.dat$age, suicide.dat$suicide_rate, dnn = c("AGE", "SUICIDE_RATE"))
age_cst=chisq.test(tab.age)
view(tab.age)

## Generation 

ggplot(suicide.dat, aes(x=generation, y=suicide_rate))+
  stat_summary(fun.y = "mean", geom="bar", fill="blue", na.rm = T)+
  ylab("Suicide_rate")

tab.generation <- table(suicide.dat$generation, suicide.dat$suicide_rate, dnn = c("Generation", "SUICIDE_RATE"))
generation_cst=chisq.test(tab.generation)
view(tab.generation)

## HDI for year

ggplot(suicide.dat, aes(HDI, log(suicide_rate)))+
  geom_point()

cor(suicide.dat$HDI,suicide.dat$suicide_rate,use="complete.obs")

tab.HDI <- table(suicide.dat$HDI, suicide.dat$suicide_rate, dnn = c("HDI", "SUICIDE_RATE"))
HDI_cst=chisq.test(tab.HDI)

## Correlation between generation and age

####Training and Testing 
##Model 1 
index <- sample(nrow(suicide.dat),nrow(suicide.dat)*0.90)
suicide.train <- suicide.dat[index,] 
suicide.test <- suicide.dat[-index,] 

lm.fit1= lm(log(suicide_rate)~generation+ age + sex ,data=suicide.train)
summary(lm.fit1)
AIC(lm.fit1)
plot(lm.fit1)
plot(lm.fit1$fitted.values, lm.fit1$residuals)


lm.fit2= lm(suicide_rate~sex ,data=suicide.train)
summary(lm.fit2)
plot(lm.fit2)
AIC(lm.fit2)

pred.test<- predict(lm.fit1, newdata = suicide.test)
MSPE1<- mean((suicide.test$suicide_rate-pred.test)^2)

sum.fit1=summary(lm.fit1)
RS1=sum.fit1$r.squared
ARS1=sum.fit1$adj.r.squared
MSE1=(sum.fit1$sigma)^2
AIC(lm.fit1)
BIC(lm.fit1)
plot(lm.fit1)


#Part 2
nullmodel<- lm(suicide_rate~1 , data=suicide.train)
fullmodel<- lm(suicide_rate~. , data=suicide.train)
lm.fit2<- step(nullmodel, scope = list(lower=nullmodel, upper=fullmodel), direction = "both") 
summary(lm.fit2)

pred.model2<- predict(lm.fit2, newdata = suicide.test)
mspe2<- mean((suicide.test$suicide_rate-pred.model2)^2) 

sum.fit2=summary(lm.fit2)
RS=sum.fit2$r.squared
ARS=sum.fit2$adj.r.squared
MSE=(sum.fit2$sigma)^2
AIC(lm.fit2)
BIC(lm.fit2)
plot(lm.fit2)







