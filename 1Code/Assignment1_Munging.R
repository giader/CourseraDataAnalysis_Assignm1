#Data Analysis Assignment no.1
#Due date 17/11
# https://class.coursera.org/dataanalysis-002/human_grading/view/courses/971332/assessments/4/submissions
# https://class.coursera.org/dataanalysis-002/human_grading/index

# WORK 
setwd("C:/Users/Gianni/Dropbox/R_Coursera/DataAnalysis/Assignment1")

loansData <- read.csv("./loansData.csv")
zloansData <- loansData  # save the original
loansdata <- loansData

#head(loansdata)
names(loansdata)
#rename variables
names(loansdata) <- tolower(names(loansdata))
names(loansdata) <- gsub("\\.","",names(loansdata))
names(loansdata) <- gsub("byinvestors","",names(loansdata))
names(loansdata) <- gsub("inthelast6months","",names(loansdata))
names(loansdata) <- gsub("balance","",names(loansdata))
names(loansdata) <- gsub("amount","",names(loansdata))
names(loansdata) <- gsub("ratio","",names(loansdata))
names(loansdata) <- gsub("fico","FICO",names(loansdata))
names(loansdata) <- gsub("debtto","debt_",names(loansdata))
names(loansdata)

## [1] "requested"        "funded"           "interestrate"     "loanlength"       "loanpurpose"     
## [6] "debt_income"      "state"            "homeownership"    "monthlyincome"    "FICOrange"       
## [11] "opencreditlines"  "revolvingcredit"  "inquiries"        "employmentlength"

## dataset structure
head(loansdata)
str(loansdata)

## check NA occorences 
dim(loansdata)
sum(complete.cases(loansdata))
sum(!complete.cases(loansdata))  ##2
loansdata[!complete.cases(loansdata), ]

## check frequencies
table(loansdata$state)
table(loansdata$FICOrange)
table(loansdata$loanlength)
range(loansdata$requested)
range(loansdata$funded)

############# DATA CLEANING and MUNGING
## loansdata$funded == -0.01 to 0.00
loansdata[loansdata$funded == -0.01,]
loansdata$funded <- as.numeric(sub("-0.01", "0", loansdata$funded))


## homwownership == NONE to NA
table(loansdata$homeownership, useNA="ifany")   ## considera gli NA
loansdata$homeownership <- sub("NONE", NA, loansdata$homeownership)

## Variables transformations
loansdata$debt_income <- as.numeric(sub("%","",loansdata$debt_income))/100
loansdata$interestrate <- as.numeric(gsub("%$", "", loansdata$interestrate))/100
loansdata$funded <- as.numeric(loansdata$funded)
#loansdata$loanlengthN <- as.integer(gsub(" months$", "", loansdata$loanlength))  ## New
#loansdata$employmentlength.wip <- gsub("^([0-9]+)(\\+)? years$", "\\1", loansdata$employmentlength)
#loansdata$employmentlength.wip <- gsub("1 year", "1", loansdata$employmentlength)

emplength <- loansdata$employmentlength
emplength <- sub(" year", "", emplength)
emplength <- sub("< 1", "0", emplength)
emplength <- sub("s", "", emplength)
emplength <- sub("n/a", NA, emplength)
loansdata$emplength <- as.numeric(sub("10+", "11", emplength, fixed = TRUE))
##suppressWarnings(loansdata$employmentlength.wip <- as.integer(loansdata$employmentlength))
rm(emplength)
##loansdata <- loansdata[,-15]  
##loansdata$employmentlength.wip <- NULL
str(loansdata$state)
table(loansdata$state)
loansdata$homeownership <- as.factor(loansdata$homeownership)
loansdata$bal_income <- loansdata$revolvingcredit/loansdata$monthlyincome
loansdata$bal_yearincome <- loansdata$revolvingcredit/(loansdata$monthlyincome*12)
loansdata$fundedPerc <- loansdata$funded/loansdata$requested

##loansData$FICO.Lower <- as.numeric(lapply(strsplit(as.character(loansData$FICO.Range),"-"),"[",1))
##loansData$FICO.Upper <- as.numeric(lapply(strsplit(as.character(loansData$FICO.Range),"-"),"[",2))
##loansData$FICO.Media <- (loansData$FICO.Lower + loansData$FICO.Upper) / 2

loansdata$FICOavg<- sapply(strsplit(as.character(loansdata$FICOrange), "-"), 
                          function(v) (as.numeric(v[1])+as.numeric(v[2]))/2)
median(loansdata$FICOavg, na.rm=T)
# FICO range: 640 to 834.
# Median: 702.0 (FICOavg).
# Mean: 707.9 (FICOavg)
# http://en.wikipedia.org/wiki/Credit_score_in_the_United_states:
# "the higher the better". 

loansdata <- loansdata[,-14]  #remove EmploymentLength column
summary(loansdata)
sum(complete.cases(loansdata))
sum(!complete.cases(loansdata))
quantile(loansdata$revolvingcredit/loansdata$monthlyincome, na.rm= TRUE)
quantile(loansdata$debt_income)
quantile(loansdata$debt_income*100)

loansdata[!complete.cases(loansdata), ]
range(loansdata, na.rm=T)
