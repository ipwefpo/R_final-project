#R class final project, analysis stock
rm(list = ls())
#install.packages("quantmod")
library("quantmod")

############################data select:
#find company, default: search data from yahoo
#input stock name you want to search. Ex:GOOG, 2330.TW...
StockName<-scan(what = "")

yourStock<-getSymbols(StockName, auto.assign = F)
#delete NA
yourStock<-na.omit(yourStock)

############################Split-Apply-Combine
#list head data
#Open opening share price
#Hight hightest price
#Low lowest price
#Close closing price
#Volime sum of sale
#Adjusted retroactive
#each row mean one dy
tail(yourStock)


#count moving average(MA)
#month MA
yourStockma20<-runMean(yourStock[,4],n=20)
#season MA
yourStockma60<-runMean(yourStock[,4],n=60)


############################Split-Apply-Combine & visualize
# count Dividend yield
#count Dividend yield this year and compare to past 3 year

#count Dividend yield this year and compare to past 3 year
DY<-function(thisY, past){
  #count this year dividend yield
  dy<-((thisY[2]) / (thisY[1]))*100
  
  #compare to last three years
  if(dy>max(past)){
    result<-"Higher than past three years"
  }
  else if(dy<min(past)){
    result<-"lower than past three years"
  }
  
  else{
    result<-"between past three years"
  }
  
  #count below the decimal point 0.xxx
  ans = round(dy, 3)
  output1<-data.frame(LastThreeYear=c(past[1],past[2],past[3]),
                      ThisYear=c(ans, "",""),
                      CompareTOlastThreeYear=c(result, "", ""))
  bar<-c("last 3" = past[3],
         "last 2" = past[2],
         "last 1" = past[1],
         "this year" = ans)
  barplot(bar, main = "past three year & this year Dividend yield comparison",
          xlab = "last three year & this year",
          ylab = "rate")
  return(output1)
}
DoDY<-function(){
  #scan data
  #first parameter is buy price(Ex:30$/share)
  #second parameter is expected cash(Ex:1.5$/share)
  
  #scan():input your number in console, one data one enter
  #if input is over, enter again
  
  #input buy price & expected cash
  #use last day close price to count yield rate
  temp<-tail(yourStock[,4],1)
  thisYear<-c(temp[[1]])
  #input expect cash ex:1.5$/share
  print("input expect cash ex:1.5$/share")
  temp<-scan()
  thisYear[2] = temp
  #input past three Dividend yield
  print("input past three Dividend yield")
  pastThreeYear<-scan()
  
  # print past three year & this year Dividend yield and compare
  DY(thisYear, pastThreeYear)
}

DoDY()

############################visualize
#Plot data with assign range
#assign what color you want. Ex: w/b
print("assign what color you want. Ex: w(white)/b(black)")
color<-scan(what = "")
#range of data you want to see. Ex:"2019-12-01::2020-05-31"
print("range of data you want to see. Ex:2019-12-01::2020-05-31")
print("input range:")
range<-scan(what = "")


if( color == "b" ){
  chartSeries(yourStock[range,], name = StockName) 
  
} else if (color == "w") {
  chartSeries(yourStock[range,], theme = "white", 
              name = StockName)
}else{print("wrong color")}
#plot, maybe white theme is easier to see
#add 20MA&60MA in chart

addTA(yourStockma20,on=1,col="blue")
addTA(yourStockma60,on=1,col="red")

#rm(color,range)

yourStock=as.matrix(to.weekly(yourStock))
profit=setNames(numeric(length(rownames(yourStock))), rownames(yourStock))

############################Split-Apply-Combine & visualize
##設定profit向量紀錄每周損益

lastC=yourStock[1,4]　　##先記錄第一週的收盤價
for (m in rownames(yourStock)[-1]) {　　##開始以每週為單位跑迴圈
  
  fee=ceiling(yourStock[m,1]*1000*(0.001425*2*0.5+0.003))　　
  ##設定手續費與稅(假設手續費打5折)
  
  if(yourStock[m,1]<=lastC){profit[m]=(yourStock[m,4]-yourStock[m,1])*1000-fee}　　##開低買進的損益
  lastC=yourStock[m,4]　　##紀錄本周收盤價，做為下週判斷開高開低的依據
}

head(cbind(yourStock,profit),20)
#lwd => line thick cumsum => sum of profit
plot(cumsum(profit),type="l",col="red",lwd=2)
