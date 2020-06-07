#R class final project, analysis stock
install.packages("quantmod")
library("quantmod")

#find company, default: search data from yahoo
getSymbols("GOOG")
TSMC<-getSymbols("2330.TW", auto.assign = F)
#delete NA
TSMC<-na.omit(TSMC)

#list head data
#Open opening share price
#Hight hightest price
#Low lowest price
#Close closing price
#Volime sum of sale
#Adjusted retroactive
#each row mean one dy
head(GOOG)
head(TSMC)
#Plot data with assign range
chartSeries(GOOG["2019-12-01::2020-05-31",])
chartSeries(TSMC["2019-12-01::2020-05-31",])
#count moving average(MA)
#month MA
GOOGma20<-runMean(GOOG[,4],n=20)
TSMCma20<-runMean(TSMC[,4],n=20)
#season MA
GOOGma60<-runMean(GOOG[,4],n=60)
TSMCma60<-runMean(TSMC[,4],n=60)
#plot, maybe white theme is easier to see
#add 20MA&60MA in chart
chartSeries(GOOG["2019-12-01::2020-05-31",], theme = "white")
addTA(GOOGma20,on=1,col="blue")
addTA(GOOGma60,on=1,col="red")

chartSeries(TSMC["2019-12-01::2020-05-31",], theme = "white")
addTA(TSMCma20,on=1,col="blue")
addTA(TSMCma60,on=1,col="red")

barChart(GOOG)
barChart(TSMC)

