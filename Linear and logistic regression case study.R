# Implementation of regreesion
library(car)
data("Davis")
dim(Davis)
str(Davis)
brief(Davis)


########Simple linear Regression
lm(Davis$weight~Davis$repwt)
model=lm(Davis$weight~Davis$repwt)
summary(model)


model=lm(weight~repwt,data = Davis)
summary(model)

Output = summary(model)

Output$coefficients
Output$coefficients[2]
Output$coefficients[6]
Output$coefficients[8]
Output$adj.r.squared
Output$fstatistic
Output$fstatistic[1]

----------------------------------------------------------------------------------------------------


# Stock Price prediction case study on regression modelling 

data <- read.csv("E:/R prac/Datasets Predictive/ABC.csv")
View(data)

head(data)

data = na.omit(data)
class(data$Date)
data$Date = as.Date(data$Date , format = '%d-%m-%Y')
class(data$Date)

plot(data$Date,data$ABC, xlab = 'Date',ylab = '',main = 'ABC Returns', type = 'l')



# plot the cumulative data

data$cum_ret =data$Price/data$Price[1]
plot(data$Date,data$cum_ret, xlab = 'Date',ylab = '',
     main = 'ABC cumulative Returns', type = 'l', lwd =2)
----------------------------------------------------------------------------------

# Create test and train data

library(lubridate)

train = data[data$Date <= as.Date('2017-12-01') & year(data$Date)> 2006,]
test = data[data$Date > as.Date('2017-12-31'),]



# examine the relationship between nifty and ABC returns

slr =lm(ABC~Nifty,data = train)

summary(slr)

p = predict(slr,test)

plot(test$Date,test$ABC,xlab = 'Nifty',ylab = 'ABC',main = 'Predicted Returns vs Actual Returns',
     ylim = c(-0.2,0.2),pch=20,col='red',type = 'l')
lines(test$Date,p,lwd=4,col='green',type = 'l')
legend('topright',c('Actual Return','Predicted Return'),fill = c('red','green'))

cor(test$ABC,p)
########Cumulative Returns
# head(data)
# 
# slr =lm(cum_ret~Nifty,data = train)
# 
# summary(slr)
# 
# p = predict(slr,test)

# --------------------------------------------------------------------------------


# MULTIPLE LINEAR REGRESSION MODEL
head(train)

mlr=lm(ABC ~ Sensex+Sentiment+DividendAnnounced+Nifty,data = train)
summary(mlr)
----------------------------------
# ""multicolinearity""
library(car)
vif(mlr)

cor(train[,c('ABC','Nifty','Sensex','Sentiment')])

mlr=lm(ABC ~ Sensex+Sentiment+DividendAnnounced,data = train)
summary(mlr)

test_predicted =predict(mlr, newdata = test[,c("Sensex","Sentiment","DividendAnnounced")])
library(miscTools)


plot(test$Date,test$ABC,xlab = 'Date',ylab = 'ABC',main = 'Predicted Returns vs Actual Returns',
     ylim = c(-0.2,0.2),pch=20,col='red',type = 'l')
lines(test$Date,test_predicted,lwd=4,col='green',type = 'l')
legend('topright',c('Actual Return','Predicted Return'),fill = c('red','green'))

cor(test$ABC,test_predicted)


--------------------------------------------------------------------------------


# Logistic regression with Msc fail case 

library(readr)
MSc_fail <- read_csv("E:/R prac/Datasets Predictive/MSc_fail.csv")
View(MSc_fail)
head(MSc_fail)

linear = lm(Fail~Age+Female+WorkExperience+English+PGDegree+Agrade+BelowBGrade+
              Year2004+Year2005+Year2006+Year2007, data = MSc_fail)

summary(linear)

# Implementation of logit probimodel

logit = glm(formula(linear),data = MSc_fail,family = binomial('logit'))
summary(logit)

probit = glm(formula(linear),data = MSc_fail,family = binomial('probit'))
summary(probit)

install.packages('margins')
library('margins')
margins(probit)
margins(logit)


plot(logit$fitted.values,type = 'l',xlab = '',ylab = '',main = 
       'fitted vs actualfor Logit',ylim=c(0,1),col= 'green')
lines(MSc_fail$Fail,type = 'p',col='red',pch=20)
legend('topright',c('fitted value','Actual value'),fill = c('green','red'))

