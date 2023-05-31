#  ASSINGMENT 1

#a
library(tidyverse)
library(rvest)
library(dplyr)
page<-read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
company<-page %>% html_elements(".company-ellipses a") %>% html_text() %>% str_remove_all(' " ')
c_m_p<-page %>% html_elements(".stick+ td span") %>% html_text() %>% str_remove_all(",") %>% as.numeric()
market<-page %>% html_elements("#stock-list-table td:nth-child(5)") %>% html_text()
weekhigh<-page %>% html_elements("#stock-list-table td:nth-child(6)") %>% html_text()
weeklow<- page %>% html_elements("#stock-list-table td:nth-child(7)") %>% html_text()
ROE<-page %>% html_elements("#stock-list-table td:nth-child(8)") %>% html_text()
PE<-page %>% html_elements("td:nth-child(9)") %>% html_text() %>% as.numeric()
PBV<-page %>% html_elements("td:nth-child(10)") %>% html_text() %>% as.numeric()
EV<- page %>% html_elements("td:nth-child(11)") %>% html_text() %>% as.numeric()
sales<-page %>% html_elements("td:nth-child(12)") %>% html_text() %>% as.numeric()
profit<-page %>% html_elements("td:nth-child(13)") %>% html_text() %>% as.numeric()
pricechange<-page %>% html_elements("#stock-list-table td:nth-child(4)") %>% html_text() 
Data_frame1<-data.frame("Company_name(M.cap)"=company,"CMP"=c_m_p,"price_change"=pricechange,"Market_Cap(Cr)"=market,"52 WeekHigh"=weekhigh, "52 WeekLow"=weeklow ,"ROE"=ROE,"P/E"= PE,"P/BV"=PBV,"EV/EBITDA"=EV ,"5YSales Gr(%)"=sales,"5YProfit Gr(%)"=profit)
Data_frame1
#b.
codes <- html %>% html_elements(".company-ellipses a") %>% html_attr("title")
for(i in 1:50){
  codes[i] <- paste("https://www.moneyworks4me.com/indianstocks", urls_codes[i], "/company-info", sep = '')
}
stock_html <- read_html(codes[5])
temp3 <- html_table(stock_html)
tab1<- as.data.frame(temp3[1])
tab2<- as.data.frame(temp3[3])
names(tab1)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
names(tab2)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
tab1<- tab1[-c(1,2,3,4,5),-c(12,13,14)]
tab2<- tab2[-c(1),-c(12,13)]
table1 <- rbind(tab1,tab2)
rownames(table1) <- 1:nrow(table1)
stock_html <- read_html(codes[6])
temp3 <- html_table(stock_html)
tab1<- as.data.frame(temp3[1])
tab2<- as.data.frame(temp3[3])
names(tab1)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
names(tab2)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
tab1<- tab1[-c(1,2,3,4,5),-c(12,13,14)]
tab2<- tab2[-c(1),-c(12,13)]
table2 <- rbind(tab1,tab2)
rownames(table2) <- 1:nrow(table2)
stock_html <- read_html(codes[8])
temp3 <- html_table(stock_html)
tab1<- as.data.frame(temp3[1])
tab2<- as.data.frame(temp3[3])
names(tab1)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
names(tab2)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
tab1<- tab1[-c(1,2,3,4,5),-c(12,13,14)]
tab2<- tab2[-c(1),-c(12,13)]
table3 <- rbind(tab1,tab2)
rownames(table3) <- 1:nrow(table3)
stock_html <- read_html(codes[9])
temp3 <- html_table(stock_html)
tab1<- as.data.frame(temp3[1])
tab2<- as.data.frame(temp3[3])
names(tab1)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
names(tab2)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
tab1<- tab1[-c(1,2,3,4,5),-c(12,13,14)]
tab2<- tab2[-c(1),-c(12,13)]
table4 <- rbind(tab1,tab2)
rownames(table4) <- 1:nrow(table4)
stock_html <- read_html(urls_codes[11])
temp3 <- html_table(stock_html)
tab1<- as.data.frame(temp3[1])
tab2<- as.data.frame(temp3[3])
names(tab1)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
names(tab2)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
tab1<- tab1[-c(1,2,3,4,5),-c(12,13,14)]
tab2<- tab2[-c(1),-c(12,13)]
table5 <- rbind(tab1,tab2)
rownames(table5) <- 1:nrow(table5)
 
#c
tennis <- function(p) {
  a<- 0
  

  while (a < 5) {
    if (runif(1) < p) {
      a <- a + 1
    } else {
      a <- a- 1
    }
    
    if (a == -2) {
      a <- 0  
    } else if (a == 2) {
      a <- 4  
    }
  }
  
  return(a)
}
p <- 0.70  
matches <- vector("numeric", 1000)  

for (i in 1:1000) {
  matches[i] <- tennis(p)
}

ans <- mean(matches)  




#d
MontyHall<-function(){
  
  door <- sample(1:3, 1)
  door
  
  
  choice <- sample(1:3, 1)
  choice
  
  door1 <- sample(setdiff(x<-c(1,2,3), c(choice, door)), 1)
  door1
  
  door2 <- sample(setdiff(x<-c(1,2,3), c(choice, door1)), 1)
  door2
  
  if (door2 == door) {
    return(1)  
  } else {
    return(0)  
  }
}
MontyHall()
Simulation<-c()
for(i in 1:1000)
{
  Simulation<-c(Simulation,MontyHall())
}
Simulation


#Problem (e)
data<-read_html("https://editorial.rottentomatoes.com/guide/best-   .netflix-movies-to-watch-right-now/")
Rank<-file %>% html_elements(".countdown-index") %>% html_text() %>% str_remove_all("#") %>% as.numeric()
title<-file %>% html_elements(".article_movie_title a") %>% html_text()
Tomatoscore<-file %>% html_elements(".tMeterScore") %>% html_text() %>% str_remove_all("%") %>% as.numeric()
Startyear<- file %>% html_elements(".start-year") %>% html_text() %>% substr(2,5) %>% as.numeric()
Data_frame2<-data.frame("Ranking" =Rank,"Name of Movie"=title,"Tomato % score"=Tomatoscore,"Year of movie"=Startyear)
Data_frame2 
