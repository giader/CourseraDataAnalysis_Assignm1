## http://ww2.coastal.edu/kingw/statistics/R-tutorials/multregr.html

###options(show.signif.stars = F)     ##  is it useful ? 
model1 <- lm(interestrate ~ 
               FICOavg 
             + debt_income
             + loanlength
             + opencreditlines
             + bal_income
             + homeownership
             + loanpurpose
             + state
             + emplength
             + inquiries
             + monthlyincome
             , data = loansdata)
summary(model1)


model2 <- update(model1, .~.- bal_income - state - loanpurpose)
model3 <- update(model2, .~.- homeownership)
model4 <- update(model3, .~.- opencreditlines)
model5 <- update(model4, .~.- debt_income)

summary(model1)
summary(model2)
summary(model3)  ## Final
summary(model4)
summary(model5)
### LoanLengthFac60 months

anova(model1, model2)
anova(model2, model3)
anova(model3, model4)
anova(model4, model5)

#final model
finalmodel <- model3
summary(finalmodel)
step(finalmodel, direction="backward")

## test start
confint(model5)
par(mfrow=c(2,2))
plot(model5)
par(mfrow=c(1,1))
## test end 

confint(finalmodel, level=0.95)  ## level = 0.95  as default
par(mfrow=c(2,2))
plot(finalmodel)
par(mfrow=c(1,1))
#http://ww2.coastal.edu/kingw/statistics/R-tutorials/multregr.html
#http://ww2.coastal.edu/kingw/statistics/R-tutorials/simplelinear.html

#TO DO: correlation and prediction
library(MASS)

## Now we'll look at a correlation matrix of most of the numerical variables. 
## This will illustrate how in R you can use one command to do many things at once.
## The first part of the command rounds the results to 3 decimal places. 
## Nested inside of that command is one that calculates the correlation matrix. 
## And nested inside of that is a restriction on the loansData dataframe that specifies 
## only certain variables to be included. 
summary(loadsdata)  ## Exist NA values
str(loansdata)

sapply(loansdata[1,],class)
sum(is.na(loansdata$empLength))

##loansdata.temp <- c(loansdata, nrow(na.omit(loansdata)))
## loansdata.temp <- loansdata[nrow((na.omit(loansdata), ]  

### loansdata.temp <- data.frame(loansdata,na.rm=TRUE)

round(cor(loansdata[,c(2,3,6,9,11,13,15,18)]),4)



#cleanup
rm(list = ls(pattern = "model."))