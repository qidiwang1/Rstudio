library(writexl)
data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
data <- data[data$Country!='China_Exhubei'&data$Country!='China_Hubei',]
data$Country <- factor(data$Country)
merge <- data.frame()
for (a in levels(data$Country)){
  country <- data[data$Country==a,]
  newcase_7day <- c()
  for (i in c(1:6)){
    newcase_7day <- c(newcase_7day,as.double(NA))
  }
  for (i in c(7:nrow(country))){
    newcase_7day <- c(newcase_7day,mean(country$New.cases[(i-6):i]))
  }
  country$newcase_7day <- newcase_7day
  merge <- rbind(merge,country)
}

data <- merge
data$Date <- factor(data$Date)

newcase <- data.frame()
for (a in levels(data$Date)){
  certain_date <- data[data$Date==a,]
  US <- certain_date$newcase_7day[certain_date$alpha.3=='USA']
  India <- certain_date$newcase_7day[certain_date$alpha.3=='IND']
  Brazil <- certain_date$newcase_7day[certain_date$alpha.3=='BRA']
  SF <- certain_date$newcase_7day[certain_date$alpha.3=='ZAF']
  Colombia <- certain_date$newcase_7day[certain_date$alpha.3=='COL']
  Mexico <- certain_date$newcase_7day[certain_date$alpha.3=='MEX']
  Asia <- sum(certain_date$newcase_7day[certain_date$region=='Asia'&certain_date$alpha.3!='IND'])
  Africa <- sum(certain_date$newcase_7day[certain_date$region=='Africa'&certain_date$alpha.3!='ZAF'])
  Europe <- sum(certain_date$newcase_7day[certain_date$region=='Europe'])
  Oceania <- sum(certain_date$newcase_7day[certain_date$region=='Oceania'])
  norA <- sum(certain_date$newcase_7day[certain_date$sub.region=='Northern America'&certain_date$alpha.3!='USA'])
  souA <- sum(certain_date$newcase_7day[certain_date$sub.region=='Latin America and the Caribbean'&certain_date$alpha.3!='MEX'&certain_date$alpha.3!='COL'&certain_date$alpha.3!='BRA'])
  newcase <- rbind(newcase,cbind(Date = as.character(a),美国 = US, 印度 = India, 巴西 = Brazil, 南非 = SF, 哥伦比亚 = Colombia,
                                 墨西哥 = Mexico, '亚洲(除印度)'=Asia, '非洲(除南非)'=Africa, 欧洲 = Europe, 大洋洲 = Oceania,
                                 '北美洲(除美国)'=norA, '拉丁美洲(除墨西哥、巴西、哥伦比亚)'=souA))
}

write_xlsx(newcase,'/Users/qidiwang1/Desktop/分地区新增.xlsx')
View(data)

merge <- data.frame()
data$Date <- as.Date(data$Date)
for(a in levels(data$Country)){
  country <- data[data$Country==a,]
  country$newcase_difference = tail(country$newcase_7day,1)-max(country$newcase_7day[country$Date>='2020-07-01'&country$Date<='2020-07-31'])
  country$newcase_ratio = (tail(country$newcase_7day,1)-max(country$newcase_7day[country$Date>='2020-07-01'&country$Date<='2020-07-31']))/max(country$newcase_7day[country$Date>='2020-07-01'&country$Date<='2020-07-31'])
  merge <- rbind(merge,country)
}

merge <- merge[merge$Date==max(merge$Date),c('Country','newcase_difference','newcase_ratio')]
View(merge)
write.csv(merge,'/Users/qidiwang1/Desktop/top change.csv')
