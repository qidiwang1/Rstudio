merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
#merge <- read.csv('/Users/qidiwang1/Desktop/test_merge.csv')
View(processed_data)
library(smoother)

#这里是对每一个国家运算Raw Death Doubling Days，country$death请贴入total death那一列
#merge <- merge[merge$Country!='Benin',]
merge$Country <- factor(merge$Country)

  
country1 <- country
for (a in levels(merge$Country)){
  country <- merge[merge$Country==a,]
  Jdate <- 0
  determine_Jdate <- 0
  doubling_day <- c()
  for (i in c(1:nrow(country))){
    cutoff <- country$confirm[i]/2
    subgroup <- country[country$confirm<=cutoff&country$Jdate<=country$Jdate[i],]
    Jdate <- tail(subgroup$Jdate,1)
    
    min_confirm <- tail(subgroup$confirm,1)
    max_confirm <- country[country$Jdate==(Jdate+1),]$confirm
    determine_Jdate <- Jdate + (cutoff-min_confirm)/(max_confirm-min_confirm)
    if (nrow(subgroup)==0){
      days <- 0 
      doubling_day <- c(doubling_day,days)
    } else if (country$confirm[i]==0) {
      days <- 0 
      doubling_day <- c(doubling_day,days)
    } else {
      days <- country$Jdate[i]-determine_Jdate
      doubling_day <- c(doubling_day,days)
    }
  }
  country$confirm_doubling_days <- doubling_day
  country1 <- rbind(country1,country)
}
library(zoo)
country1 <- country1%>%
  group_by(Country)%>%
  mutate(doubling_days = rollmean(confirm_doubling_days,7,fill=NA,align='right'))
library(writexl)
write_xlsx(country1,'/Users/qidiwang1/Desktop/tttt.xlsx')
#以下为对Raw Death Doubling Days与Raw Confirm Doubling Days进行7天平滑处理，country$death_doubling_days请贴入Raw Death Doubling Days那一列
merge <- data.frame()
for (a in levels(country1$Country)){
  country <- country1[country1$Country==a,]
  smooth_confirm <- c()
  for (i in c(1:6)){
    smooth_confirm <- c(smooth_confirm, as.double(NA))
  }
  for (i in (7:nrow(country))){
    smooth_confirm <- c(smooth_confirm, mean(country$confirm_doubling_days[(i-6):i]))
  }
  country$smooth_confirm_doubling_days <- smooth_confirm
  country$smooth_confirm_doubling_days <- ifelse(is.na(country$smooth_confirm_doubling_days)==TRUE,
                                                 country$confirm_doubling_days,country$smooth_confirm_doubling_days)
  merge <- rbind(merge,country)
}

library(anchors)
merge <- replace.value(merge,'smooth_confirm_doubling_days',0,as.double(NA))