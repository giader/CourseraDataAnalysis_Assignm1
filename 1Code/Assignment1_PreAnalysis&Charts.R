str(loansdata)
## requested       : int  20000 19200 35000 10000 12000 6000 10000 33500 14675 7000 ...
## funded          : num  20000 19200 35000 9975 12000 ...
## interestrate    : num  0.089 0.1212 0.2198 0.0999 0.1171 ...
## loanlength      : Factor w/ 2 levels "36 months","60 months": 1 1 2 1 1 1 1 2 1 1 ...
## loanpurpose     : Factor w/ 14 levels "car","credit_card",..: 3 3 3 3 2 10 3 2 2 2 ...
## debt_income     : num  0.149 0.284 0.238 0.143 0.188 ...
## state           : Factor w/ 46 levels "AK","AL","AR",..: 37 39 5 16 28 7 19 18 5 5 ...
## homeownership   : Factor w/ 4 levels "MORTGAGE","OTHER",..: 1 1 1 1 4 3 4 1 4 4 ...
## monthlyincome   : num  6542 4583 11500 3833 3195 ...
## FICOrange       : Factor w/ 38 levels "640-644","645-649",..: 20 16 11 12 12 7 17 14 10 16 ...
## opencreditlines : int  14 12 14 10 11 17 10 12 9 8 ...
## revolvingcredit : int  14272 11140 21977 9346 14469 10391 15957 27874 7246 7612 ...
## inquiries       : int  2 1 1 0 0 2 0 0 1 0 ...
## employmentlength: Factor w/ 12 levels "< 1 year","1 year",..: 1 4 4 7 11 5 3 3 10 5 ...
## emplength       : num  0 2 2 5 9 3 11 11 8 3 ...
## bal_income      : num  2.18 2.43 1.91 2.44 4.53 ...
## bal_yearincome  : num  0.182 0.203 0.159 0.203 0.377 ...
## fundedPerc      : num  1 1 1 0.998 1 ...
## FICOavg         : num  737 717 692 697 697 672 722 707 687 717 ...

## Investigation about relationship between FICO score and Interest Rate 
par(mfrow=c(1,1))
plot(loansdata$FICOrange, loansdata$interestrate, 
     main = "Fig1-FICO score versus Interest Rate",
     labels=as.character(seq(640,830,by=50)),
     xlab = "FICO Score", ylab = "Int. rate", yaxt = "n")
axis(2, at=pretty(loansdata$interestrate),
     lab=paste0(pretty(loansdata$interestrate)*100, "%"),
     las = TRUE)
dev.copy(png,'fig1-InterestRate-vs-FICO-range.png')
dev.off()

## Investigation about relationship between FICO score avg and Interest Rate 
plot(loansdata$FICOavg, loansdata$interestrate,
     pch=19, cex=0.7, 
     main = "Fig1a-FICO score average vs Interest Rate",
     xlab = "FICO Score", ylab = "Int. rate (range: 0 - 1)",
     col=loansdata$loanlength)
legend("topright",legend=c("36 months","60 months"),col=c("black","red"),
      pch=c(19,19),cex=c(1,1))
dev.copy(png,'fig1a-InterestRate-vs-FICO-avg.png')
dev.off()

## Investigation about relationship between FICO score and Interest Rate 

# when:
# Loan.Length :  36 months  
loansdata.temp <- loansdata[loansdata$loanlength == "36 months", ]
plot(loansdata.temp$FICOrange, loansdata.temp$interestrate, 
     main = paste0("Fig2-FICOscore vs Int.Rate (Loan ", loansdata.temp$loanlength[1], ")"),  
     xlab = "FICO Score", ylab = "Int. rate", yaxt = "n")
axis(2, at=pretty(loansdata.temp$interestrate)
     , lab=paste0(pretty(loansdata.temp$interestrate)*100, "%")
     , las = TRUE)
dev.copy(png,'fig2-InterestRate-vs-FICO-range(when LoanLength=36m).png')
dev.off()
# when:
# Loan.Length :  60 months   
loansdata.temp <- loansdata[loansdata$loanlength == "60 months", ]
plot(loansdata.temp$FICOrange, loansdata.temp$interestrate, 
     main = paste0("Fig3-FICOscore vs Int.Rate (Loan ", loansdata.temp$loanlength[1], ")"),  
     xlab = "FICO Score", ylab = "Int. rate", yaxt = "n", col="red")
axis(2, at=pretty(loansdata.temp$interestrate)
     , lab=paste0(pretty(loansdata.temp$interestrate)*100, "%")
     , las = TRUE)
dev.copy(png,'fig3-InterestRate-vs-FICO-range(when LoanLength=60m).png')
dev.off()
rm(loansdata.temp)

# Fitting linear model 
par(mfrow=c(1,2))
loansdata.temp <- loansdata[loansdata$loanlength == "36 months", ]
plot(loansdata.temp$FICOavg, loansdata.temp$interestrate, 
     main = paste0("Loan: ",loansdata.temp$loanlength[1]),
     xlab = "FICO Score", ylab = "Int. rate", yaxt = "n")
axis(2, at=pretty(loansdata.temp$interestrate)
     , lab=paste0(pretty(loansdata.temp$interestrate)*100, "%")
     , las = TRUE)
abline(lm(loansdata.temp$interestrate ~ loansdata.temp$FICOavg), col = "red", lwd =2)

loansdata.temp <- loansdata[loansdata$loanlength == "60 months", ]
plot(loansdata.temp$FICOavg, loansdata.temp$interestrate, 
     main = paste0("Loan: ",loansdata.temp$loanlength[1]),
     xlab = "FICO Score", ylab = "Int. rate", yaxt = "n", col="red")
axis(2, at=pretty(loansdata.temp$interestrate)
     , lab=paste0(pretty(loansdata.temp$interestrate)*100, "%")
     , las = TRUE)
abline(lm(loansdata.temp$interestrate ~ loansdata.temp$FICOavg), col = "blue", lwd =2)
mtext(side = 1, text = "Fig 4", line = 4)
dev.copy(png,'fig4-InterestRate-vs-FICO-range-fitting-linear.png')
dev.off()
par(mfrow=c(1,1))
rm(loansdata.temp)

## Histogram FICO Range (avg)
hist(loansdata$FICOavg, 
     main = "Fig 5 - FICO Range",  
     xlab = "FICO Range(avg)")
axis(1,1:40, levels(loansdata$FICOavg))
dev.copy(png,'fig5-Hist-FICO-range(avg).png')
dev.off()

## Investigation about Amount Requested versus Interest Rate 

plot(loansdata$requested, loansdata$interestrate, 
     main = "Fig6-Amount Requested vs Int. Rate",  
     xlab = "amount in $", ylab = "Int. rate", yaxt = "n")
axis(2, at=pretty(loansdata$interestrate),
     lab=paste0(pretty(loansdata$interestrate)*100, "%"),
     las = TRUE)
dev.copy(png,'fig6-AmountFunded-vs-InterestRate.png')
dev.off()

## FICO score against the ratio between Amount Funded and Requested
# less relevant 
plot(loansdata$FICOrange, loansdata$fundedPerc, 
     main = "Granted vs FICO Score",
     xlab = "FICO Score", ylab = "Funded/requested", yaxt = "n")
axis(2, at=pretty(loansdata$fundedPerc)
     , lab=paste0(pretty(loansdata$fundedPerc)*100, "%")
     , las = TRUE)

## Ratio between Amount Funded and Requested (Funded %) versus Debt to Income ratio 
# less relevant 
plot(loansdata$debt_income, loansdata$fundedPerc, 
     main = "Funded % vs  Debt/Income ratio", 
     xlab = "Debt/Income", ylab = "Funded/requested",
     yaxt = "n", xaxt = "n")
axis(2, at=pretty(loansdata$fundedPerc),
     lab=paste0(pretty(loansdata$fundedPerc)*100, "%"),
     las = TRUE)
axis(1, at=pretty(loansdata$debt_income),
     lab=paste0(pretty(loansdata$debt_income)*100, "%"),
     las = TRUE)

## Ratio between Amount Funded and Requested (Funded %) versus nr of Inquiries in the last 6 m
# less relevant 
plot(loansdata$inquiries, loansdata$fundedPerc, 
     main = "Funded/Requested vs Inquiries", 
     xlab = "nr Inquiries", ylab = "Funded %"
     , yaxt = "n")
axis(2, at=pretty(loansdata$fundedPerc)
     , lab=paste0(pretty(loansdata$fundedPerc)*100, "%")
     , las = TRUE)

## Ratio between Interest rate versus nr of Inquiries in the last 6 m
plot(loansdata$inquiries, loansdata$interestrate, 
     main = paste0("Fig7-Int.Rate (mean:", 
                   round(mean(loansdata$interestrate, na.rm=T)*100, digits = 2),  
                   "%) vs nr Inquiries"), 
     xlab = "nr Inquiries", ylab = "Int.Rate", yaxt = "n")
axis(2, at=pretty(loansdata$interestrate)
     , lab=paste0(pretty(loansdata$interestrate)*100, "%")
     , las = TRUE)
abline(a=mean(loansdata$interestrate), b=0, col="red")
dev.copy(png,'fig7-IterestRate-vs-Inquiries.png')
dev.off()

## Debt to Income ratio versus Loan Length
# less relevant either 36 or 60 months
loansdata.temp <- loansdata[loansdata$loanlength == "60 months", ]
plot(loansdata.temp$debt_income, loansdata.temp$interestrate, 
     main = "Int. Rate vs Debt/Income(60m)", 
     xlab = "debt_income", ylab = "Int.Rate",
     yaxt = "n", xaxt = "n")
axis(2, at=pretty(loansdata.temp$interestrate),
     lab=paste0(pretty(loansdata.temp$interestrate)*100, "%"),
     las = TRUE)
axis(1, at=pretty(loansdata.temp$debt_income),
     lab=paste0(pretty(loansdata.temp$debt_income)*100, "%"),
     las = TRUE)
rm(loansdata.temp)
# less relevant either 36 or 60 months

## Boxplot about Home Ownership vs Int. Rate 
# less relevant
# remove NA
##par(mfrow=c(1,2))
table(loansdata$homeownership, useNA="ifany")          ## Consider <NA> value
table(loansdata$homeownership, exclude = c(NA, NaN))   ## NO <NA> value
loansdata.temp <- loansdata[!is.na(loansdata$homeownership),]  ## remove NA
plot(loansdata.temp$homeownership, loansdata.temp$interestrate, 
     main = "Fig8-Home owner vs Int.Rate", 
     xlab = "Home Ownership", ylab = "Int. Rate",
     yaxt = "n")
axis(2, at=pretty(loansdata.temp$interestrate),
     lab=paste0(pretty(loansdata.temp$interestrate)*100, "%"),
     las = TRUE)
rm(loansdata.temp)
dev.copy(png,'fig8-BoxPlot-IntRate-vs-Ownership.png')
dev.off()
## Interest per State
plot(loansdata$state, loansdata$interestrate, 
     main = "Fig9-US state vd Int.Rate", 
     xlab = "US state", ylab = "Int.Rate",
     yaxt = "n")
axis(2, at=pretty(loansdata$interestrate), 
     lab=paste0(pretty(loansdata$interestrate)*100, "%"), las = TRUE)
par(mfrow=c(1,1))
dev.copy(png,'fig9-BoxPlot-IntRate-vs-State.png')
dev.off()

## Boxplot for interest rate versus purchase reason.
## Added line as mean interest rate.
wip.temp <- with(loansdata, reorder(loanpurpose, interestrate, median))
par(mar=c(8, 4, 4, 2))
plot(wip.temp, loansdata$interestrate,
     main = paste0("Fig10-Int.Rate (mean:", 
                   round(mean(loansdata$interestrate, na.rm=T)*100, digits = 2),  
                   "%) vs LoansPurpose"), 
     xlab = "", ylab = "Int. Rate",
     yaxt = "n", xaxt = "n")
axis(2, at=pretty(loansdata$interestrate),
     lab=paste0(pretty(loansdata$interestrate)*100, "%") , las = 2)
axis(1, las = 2, at = wip.temp,
     labels = wip.temp, cex.axis = 0.7)
mtext(side = 1, text = "Loan Purpose", line = 7)
abline(a=mean(loansdata$interestrate), b=0, col="red")
par(mar=c(5.1, 4.1, 4.1, 2.1))
rm(wip.temp)
dev.copy(png,'fig10-BoxPlot-IntRate-vs-LoansPurpose.png')
dev.off()


######  PAIRS   Chart
## http://ww2.coastal.edu/kingw/statistics/R-tutorials/multregr.html
pairs(formula = ~ FICOavg 
      + interestrate 
      + debt_income 
      + fundedPerc 
      + bal_income 
      + opencreditlines 
      + emplength, data = loansdata,
      main = "Fig 11 - Scatterplot Matrix")
dev.copy(png,'fig11-ScatterMatrix.png')
dev.off()
## If two people have the same FICO score, can other var explain a difference 
## in Interest Rate between them ?? 
names(loansdata)
table(loansdata$FICOrange)
mean(loansdata$FICOavg)

# At the same FICO score (ex 705-709 with max freqency) the relationships are:
loansdata.temp <- loansdata[loansdata$FICOrange == "675-679", ]
table(loansdata.temp$FICOavg)
pairs(formula = ~ FICOavg 
      + interestrate 
      + debt_income 
      + fundedPerc 
      + bal_income 
      + opencreditlines 
      + emplength, data = loansdata.temp)

pairs(formula = ~ FICOavg 
      + interestrate 
      + debt_income 
      + fundedPerc 
      + opencreditlines 
      + emplength, data = loansdata.temp,
      main = "Fig12-Scatterplot Matrix when FICO Range = 675-679")
rm(loansdata.temp)
dev.copy(png,'fig12-ScatterMatrix-when-FICORange675-679.png')
dev.off()

loansdata.temp <- loansdata[loansdata$FICOrange == "705-709", ]
pairs(formula = ~ FICOavg 
      + interestrate 
      + debt_income 
      + fundedPerc 
      + opencreditlines 
      + emplength, data = loansdata.temp,
      main = "Fig13-Scatterplot Matrix when FICO Range = 705-709")
rm(loansdata.temp)
dev.copy(png,'fig13-ScatterMatrix-when-FICORange705-709.png')
dev.off()

## Investigation about Revolving CREDIT Balance versus Interest Rate 
# less relevant
plot(loansdata$revolvingcredit, loansdata$interestrate, 
     main = "Revolving Credit vs Interest Rate",  
     xlab = "amount in $", ylab = "Int. rate", yaxt = "n")
axis(2, at=pretty(loansdata$interestrate),
     lab=paste0(pretty(loansdata$interestrate)*100, "%"),
     las = TRUE)
# less relevant

pairs(formula = ~ FICOavg 
      + interestrate 
      + debt_income 
      + fundedPerc 
      + bal_income 
      + opencreditlines, data = loansdata.temp)

## Debt to Income ratio versus Open CREDIT Lines
# 

## no good without FICO Range  r2 too low close to zero
pairs(formula = ~ interestrate 
      + debt_income 
      + fundedPerc 
      + bal_income 
      + opencreditlines, data = loansdata.temp)

modelX1 <- lm(interestrate ~ debt_income + fundedPerc +
               bal_income + opencreditlines, data = loansdata.temp)
summary(modelX1)
lm(formula = interestrate ~ debt_income + fundedPerc + 
     bal_income + opencreditlines, data = loansdata.temp)
rm(loansdata.temp)
## no good without FICO Range  r2 too low close to zero

#cleanup
rm(list = ls(pattern = "model."))
