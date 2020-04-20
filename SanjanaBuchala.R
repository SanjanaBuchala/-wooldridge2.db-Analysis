setwd("D:/SPRING 19/BA with R")
library(data.table)
library(ggplot2)
library(tidyverse)
library(RSQLite)
library(DBI)
library(RColorBrewer)
library(broom)
library(lmtest)
library(sandwich)
install.packages('plyr')
install.packages('tseries')
library(plyr)
library(tseries)
install.packages('plm')
install.packages('reshape')
library(plm)
library(reshape)

build.nsim <- function(nsim,nvec){
  simdx <- c()
  for(i in 1:length(nvec)) 
    simdx <- c(simdx,rep(1:nsim,each=nvec[i])+(i-1)*nsim)
  dt <- data.table(sim=simdx)
  bigN <- nrow(dt)
  dt$n <- rep(rep(nvec,nvec),each=nsim)
  dt$one <- 1
  dt$simc <- dt[,cumsum(one),by=sim]$V1
  dt$one <- NULL
  return(dt)
}
wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}


## QUESTION 1:
hprice <- wpull('hprice1')
summary(hprice)

summary(step(lm(price~(bdrms+lotsize+sqrft+colonial)^2+I(bdrms^2)+I(lotsize^2)+I(sqrft^2),data = hprice)))

summary(step(lm(price~(bdrms+lotsize+sqrft+colonial)^2+I(bdrms^2)+I(lotsize^2)+I(sqrft^2),data = hprice), k = log(nrow(hprice))))


# INFERENCE:
# The best fit model after the step function, with the best AIC and BIC is:
# AIC: 678.46, BIC: 697.58



##QUESTION 2:
gpa2 <- wpull('gpa2')
summary(gpa2)

summary(step(lm(colgpa~(sat+hsize+hsperc+athlete+hsrank+tothrs+female+white)^2+I(sat^2)+I(hsize^2),data = gpa2)))

summary(step(lm(colgpa~(sat+hsize+hsperc+athlete+hsrank+tothrs+female+white)^2+I(sat^2)+I(hsize^2),data = gpa2), k = log(nrow(gpa2))))


# INFERENCE:
# The best fit model after the step function, with the best AIC and BIC is:
# AIC: -5157.94, BIC: -5047.93



##QUESTION 3:
mlb <- wpull('mlb1')
summary(mlb)

summary(step(lm(salary~(years+nl+games+atbats+runs+hits+doubles+triples+hruns+rbis+bavg+bb+so+sbases+fldperc+scndbase+shrtstop+thrdbase+outfield+catcher+allstar+slugavg+hispan+black)^2, data = mlb)))

summary(step(lm(salary~(years+nl+games+atbats+runs+hits+doubles+triples+hruns+rbis+bavg+bb+so+sbases+fldperc+scndbase+shrtstop+thrdbase+outfield+catcher+allstar+slugavg+hispan+black)^2, data = mlb), k = log(nrow(mlb))))


# INFERENCE:
# The best fit model after the step function, with the best AIC and BIC is:
# AIC = -692.05, BIC = -7.24


## QUESTION 4:

#Q4.1 The model seen to have an R-squared value of 85.7% with a sample size of 128. It is seen from the model that the dummy variable has a p-value much lesser than 0.05, therefore meaning that y90 is statistically significant at the 0.01% level. Now, the co-efficient on pctstu is 0.005, which means that a 1 point increase in pctstu results in a 0.5% increase in the rent. It is also safe to say that the variable pctstu is statistically significant at the 0.1% level

#Q4.2 The standard errors in the equation are not valid because of serial correlation. If we consider the presence of the ai in the error term, it means that the errors across the time periods of the two cities are positively correlated, which invalidates the usual OLS standard error statistics


re <- wpull('rental')
summary(re)
re$y90 <- as.numeric(re$year==90)
pctstu = 100*re$enroll/re$pop

re1 <- pdata.frame(re,index = c("city","year"))
model1 <- plm(log(rent)~y90+log(pop)+log(avginc)+pctstu,data=re1,model="pooling")
summary(model1)


#Q4.3 After first differencing the pooled OLS equation we find that the co-efficient on pctstu is twice as much as that obtained using the pooled OLS method. This means that every 1% point increase in student population increases the rent by 1.1%. We have obtained a less precise estimate after differencing. The relative size of student population does affect rental prices

model2 <- plm(diff(log(rent))~diff(y90)+diff(log(pop))+diff(log(avginc))+diff(pctstu),data=re1,model="pooling")
summary(model2)


#Q4.4 After the fixed effects model, it is clear that we get identical estimates and standard errors to those in part

model3 <- plm(log(rent)~y90+log(pop)+log(avginc)+pctstu,data=re1,model="within")
summary(model3)





## QUESTION 5:
#5.1:
#i think that b1 should be negative and b2 should be positive but unemployment may or may not be the cause which led to murders and crimes

#Q5.2:
#No, there doesn't seem to be any evidence of deterrent effects in the above model because of the positive sign on exec, even though it is insignificant

#Q5.3:
#After fixed effect model is applied, we can infer that there is persence of a deterrent effect. If there are 10 more execution It is associated with 1.03 decre.

#Q5.4
#The heteroskedastic robust standard errors are as given below
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -5.27800    6.51135 -0.8106  0.41957  
# d93         -2.06742    1.27433 -1.6224  0.10794  
# exec         0.12773    0.16366  0.7804  0.43701  
# unem         2.52889    1.46759  1.7232  0.08801 .
#
#Q5.5 
#Texas has the largest number for the execution variable in 1993, which is 34 in this case. It is approximately 3 times bigger than the next highest value as well

#Q5.6 
#Now the estimated deterrent effect is smaller. Perhaps more importantly, the standard error on exec has increased by a substantial amount. This happens because when we drop Texas, we basically tend to lose much of the variation in the key explanatory variables, mainly exec.

#Q5.7 
#The size of the deterrent effect is only about half as big as when 1987 is not used. Plus, the t statistic, about -0.34, which is very small. Adding another year causes the standard error on the exec co-efficient to slightly increase .34, is very small The earlier finding of a deterrent effect is not robust to the time period used. Oddly, adding another year of data causes the standard error on the exec coefficient to markedly increase.

mur <- wpull('murder')
summary(mur)
mur1 <- pdata.frame(mur, index = c("state","year"))
pmurder <- pdata.frame(mur,index=c('state','year'))
d93 <- as.numeric(mur$year==93)


model4 <- plm(mrdrte~d93+exec+unem, data = mur1[which(mur1$year==90|mur1$year==93),], model = "pooling")
summary(model4)

model5 <- plm(mrdrte~d93+exec+unem, data = mur1[which(mur1$year==90|mur1$year==93),], model = "within")
summary(model5)

model6 <- plm(diff(mrdrte)~diff(d93)+diff(exec)+diff(unem), data = mur1[which(mur1$year==90|mur1$year==93),], model = "pooling")
summary(model6)

# Heteroskedastic Robust Standard Errors
coeftest(model5, vcov=vcovHC) 

max(mur1$exec)
model6 <- plm(mrdrte~d93+exec+unem, data = mur1[which((mur1$year==90|mur1$year==93) & mur1$state!="TX"),], model = "within")
summary(model6)

# Heteroskedastic Robust Standard Errors
coeftest(model6, vcov=vcovHC)

model7 <- plm(mrdrte~as.factor(year)+exec+unem, data = mur1, model = "within")
summary(model7)





## QUESTION 6:
#Q6.1:
#The model using pooled OLS has an R-square value 40.541% and sample size of 4596. It can be said from the model that a 0.1 change in concen will increase the airfare by 3.601%

#Q6.2:
#The 95% confidence interval for b1 is 0.3012 to 0.4191. We cannot infer about the reliability. The Robust 95% Confidence interval is calculated as:
# 0.3601 +/- 1.96*0.0585 , is the range from 0.24544 to 0.4748

#Q6.3:
#The equation: 
#log(fare) = 0.36*concen - 0.902*log(dist) + 0.1030*[log(dist)^2] > Let us say log(dist) = y 
#-Plugging in and Differentiating on both sides with respect to 'y' we get that, 
#y = 0.902/(2*0.103) = 4.3786 
#log(dist) = 4.3786 
#dist = e^(4.3786) = 79.726. 
#We can see from the summary that the minimum dist value is 95. So, we can say that the inflection point lies outside the range

#Q6.4:
#The fixed effects model produced an estimate of concen as 0.1688.
#The fixed effects equation for concen is: log(fare) = 0.0228y99 + 0.0364y99 + 0.0978*y00 + 0.1688*concen

#Q6.5:
#Two other characteristics captured by ai may be passen  (average number of passengers) and fare (average one-way airfare). They are not correlated, however there may be other factors such as population, income etc. on the routes that a particular airline operates

#6.6:
#Yes I would agree with the fact that a higher concentration increases the airfare. I feel the best estimate would be using the fixed effects model since it allows for correlation between factors mentioned in part (v)

air <- wpull('airfare')
summary(air)
air1 <- pdata.frame(air, index = c("id","year"))
concen = air$bmktshr
y98 <- as.numeric(air$year==98)
y99 <- as.numeric(air$year==99)
y00 <- as.numeric(air$year==00)


model8 <- plm(log(fare)~y98+y99+y00+concen+log(dist)+I(log(dist)^2),model = "pooling", data = air1)
summary(model8)
confint(model8)

# Robust 95% CI using standard errors
coeftest(model18,vcov = vcovHC)

model9 <- plm(log(fare)~y98+y99+y00+concen+log(dist)+I(log(dist)^2),model = "within", data = air1)
summary(model9)

cor(air1$concen,air1$fare)
cor(air1$concen,air1$passen)




## QUESTION 7:
#Q7.1:
#From the logit model it is seen that there is a marginal effect of 0.1443 on white when compared to the linear co-efficient on white which is 0.2006. This means that the linear model has a higher chance of approval when compared to the logit model i.e, 20% versus 14.43%

#Q7.2:
#After adding all the variables to the logit model, we find that the average marginal effect on whites is about 0.08282. This means that there is still evidence of some discrimination against non-whites or still some positive bias towards whites, which in this case is about 8.28%
# This is comparatively smaller when compared to the estimates in part (i) and is also statistically significant

loanapp <- wpull('loanapp')
summary(loanapp)

model10 <- glm(approve~white, family = "binomial", data = loanapp)
summary(model10)
margins(model10)

model11 <- lm(approve~white,data = loanapp)
summary(model11)

model12 <- glm(approve ~  white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr, family = binomial, data = loanapp)
summary(model12)
margins(model12)





## QUESTION 8:
#Q8.1:
# Approximately 8822/9822 = 89.21% of the sample are employed at the time of interview and also about 974/9822 = 9.9% of the sample has abused alcohol

#Q8.2:
# The linear model of employ on abuse has an R-square value of about 0.07% and a sample size of 9822. The equation can be written as
# employ = 0.901 - 0.0283*abuse
# which means that a person who abuses alcohol has a 2.8% lower chances of being employed or entering the workforce
# The heteroskedastic robust standard errors are as follows:
#  Estimate     Std. Error   t value      Pr(>|t|)
#(Intercept)  0.9009946  0.0031755 283.7299  < 2e-16 ***
#  abuse       -0.0283046  0.0111529  -2.5379  0.01117 *
#  We can conclude that the abuse is statistically significant in both cases at the 5% level

#Q8.3:
# From the logit model that abuse has an average marginal effect of -0.02589 and is seen to be significant at the 1% level. The logit model shows that there is approximately 2.6% lesser chance of getting employed when compared to the linear model

#Q8.4: 
# The logit model predicts employ = 0.901 at abuse = 0 and employ = 0.8727 at abuse = 1.
# The probit model predicts employ = 0.901 at abuse = 0 and employ = 0.8727 at abuse = 1
# Since there is only a single independent variable to work with the two models have the same estimates of employ when abuse = 1 and 0.

#Q8.5:
# From the linear model with all variables, the coefficient for abuse is 0.0203, which is statistically significant at the 5% level. This means that a person who abuses alcohol has a 2.03% lesser chance of being employed or entering the workforce

#Q8.6:
#From the logit model it is clear that the average marginal effect of abuse is about -0.01938, which basically means that a person who abuses alcohol has approximately a 1.94% lower chance of being employed. We also find that abuse from the logit model is also statistically significant at the 5% level and is also very close in value to that obtained using the linear model

#Q8.7:
#We cannot exactly say that adding the variables as controls is necessary because alcohol abuse is in direct correlation with a person's health. Health metrics are directly proportional to alcohol abuse

#Q8.8:
#We can see that mothalc and fathalc are not correlated with abuse, which means that abuse can be thought of as endogenous in the equation. From the linear model including mothalc and fathalc, we can clearly see that both the variables are not statistically significant. Hence it is safe to say that these variables need not be included in the model as instrumental variables
# It is not sensible to include them.

al <- wpull('alcohol')
summary(al)
sum(al$employ)
max(al$index)

sum(al$abuse)

model13 <- lm(employ~abuse,data = al)
summary(model13)
coeftest(model13, vcov = vcovHC)

model14 <- glm(employ~abuse, family = binomial, data = al)
summary(model14)
margins(model14)

model15 <- glm(employ~abuse, family = binomial(link = "probit"), data = al)
summary(model15)
margins(model15)

# predicting employ using logit
predict(model14,data.frame(abuse = c(0,1)), type = "response")
# predicting employ using probit
predict(model15,data.frame(abuse = c(0,1)), type = "response")

model16 <- lm(data = al, employ ~ abuse + age + I(age)^2 + educ + I(educ)^2 + married + famsize + white + northeast + midwest + south + centcity + outercity + qrt1 + qrt2 + qrt3)
summary(model16)

model17 <- glm(data = al, employ ~ abuse + age + I(age)^2 + educ + I(educ)^2 + married + famsize + white + northeast + midwest + south + centcity + outercity + qrt1 + qrt2 + qrt3, family = binomial)
summary(model17)
margins(model17)

cor(al$abuse,al$fathalc)
cor(al$abuse,al$mothalc)
model18 <- lm(employ ~ abuse + mothalc + fathalc, data = al)
summary(model18)







## QUESTION 9:
#Q9.1:
# The poisson regression model has an AIC of 4176.5 and a sample size of 1129. We can clearly see from that model that the co-efficient on y82 is -0.19261 and the average marginal effect is -0.5283
# This means that from the years 1974 to 1982 there has been approximately 19.26% decrease in the number of kids born to a woman

#Q9.2:
# The estimated percentage difference in fertility between black women and non-black women, holding other factors fixed is about 43.383%

#Q9.3:
# The correlation between kids and the poisson-predicted kids is about 0.34769. We can compute the R-square for the poisson regression model as sqrt of correlation-value for possion model which is basically about 0.12089
# The R-square from the linear model is about 0.1295. We can conclude that the R-square value of the linear model is higher than the poisson model.

fer <- wpull('fertil1')
summary(fer)
y74 <- as.numeric(fer$year == 74)
y76 <- as.numeric(fer$year == 76)
y78 <- as.numeric(fer$year == 78)
y80 <- as.numeric(fer$year == 80)
y82 <- as.numeric(fer$year == 82)
y84 <- as.numeric(fer$year == 84)

model19 <- glm(kids~educ+age+I(age^2)+black+east+northcen+west+farm+othrural+town+smcity+y74+y76+y78+y80+y82+y84, family=poisson, data=fer)
summary(model19)
margins(model19)

# Estimated percentage difference between black women and non-black women
(exp(0.3603475) - 1)*100

model20 <-  lm(kids~educ+age+I(age^2)+black+east+northcen+west+farm+othrural+town+smcity+y74+y76+y78+y80+y82+y84, data=fer)
summary(model20)

fitted_values <- fitted(model19)
fer$pred_poisson <- predict(model19,type="response")
fer$pred_linear <- predict(model20)

corv <- cor.test(fer$kids,fer$pred_poisson)$estimate
I(corv^2)












