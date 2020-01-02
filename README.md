# CourseraDataAnalysis_Assignm1
Peer-to-peer lending interest rate analysis
Introduction

Peer-to-peer lending is the practice of lending money to unrelated individuals, or peers, without going through a traditional financial intermediary such as a bank or other traditional financial institution. This lending takes place online on peer-to-peer lending companies' websites using various different lending platforms and credit checking tools” [1].
Lending Club is one of the online financial community that brings together creditworthy borrowers and savvy investors replacing the high cost and complexity of bank lending with a faster way to borrow and invest [2].
Borrowers can apply for a loan online and get an instant rate quote. Lending Club claims that the interest rate of these loans is determined on the basis of characteristics of the person asking for the loan such as their employment history, credit history, and creditworthiness scores.
The aim of this assignment is to parse an analysis to determine if there is a significant association between the interest rate of the loan and the features of borrowers. 
Using exploratory analysis and standard multiple regression techniques we show that there is a significant relationship between interest rate and FICO score, even after adjusting for confounding factors such as the amount funded, the loan length, open credit lines and Inquires on the last 6 months.
My analysis suggests that lower loan rate is associated with higher FICO score .

Methods:

Data Collection 

For our analysis we used a sample of 2,500 peer-to-peer loans issued through the Lending Club. 
The data were downloaded, using the R programming language, from following link:
https://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv

The code book for the variables in the data set is available here
https://spark-public.s3.amazonaws.com/dataanalysis/loansCodebook.pdf
