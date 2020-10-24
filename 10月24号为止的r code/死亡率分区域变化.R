library(writexl)
data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
data <- data[data$Country!='China_Exhubei'&data$Country!='China_Hubei',]
ICL <- read.csv('/Users/qidiwang1/Desktop/1012教授演讲/ICL.csv')
data$alpha.3 <- as.character(data$alpha.3)
ICL$alpha.3 <- as.character(ICL$iso3_name)
data$Date <- as.Date(data$Date)
ICL$Date <- as.Date(ICL$Date,'%m/%d/%y')
ICL <- ICL[!is.na(ICL$ICL),]
ICL <- ICL%>%
  group_by(iso3_name)%>%
  mutate(Total_cases=cumsum(ICL))
data <- data%>%
  left_join(ICL,by=c('Date','alpha.3'))
data <- data[data$alpha.3%in%levels(factor(ICL$iso3_name)),]
data$Date <- factor(data$Date)
data$Total_cases[is.na(data$Total_cases)] <- 0
data$fatality_rate = data$death/data$Total_cases
View(data)
newcase <- data.frame()
for (a in levels(data$Date)){
  certain_date <- data[data$Date==a,]
  US <- certain_date$fatality_rate[certain_date$alpha.3=='USA']
  India <- certain_date$fatality_rate[certain_date$alpha.3=='IND']
  Brazil <- certain_date$fatality_rate[certain_date$alpha.3=='BRA']
  SF <- certain_date$fatality_rate[certain_date$alpha.3=='ZAF']
  Colombia <- certain_date$fatality_rate[certain_date$alpha.3=='COL']
  Mexico <- certain_date$fatality_rate[certain_date$alpha.3=='MEX']
  Asia <- sum(certain_date$death[certain_date$region=='Asia'&certain_date$alpha.3!='IND'])/sum(certain_date$Total_cases[certain_date$region=='Asia'&certain_date$alpha.3!='IND'])
  Africa <- sum(certain_date$death[certain_date$region=='Africa'&certain_date$alpha.3!='ZAF'])/sum(certain_date$Total_cases[certain_date$region=='Africa'&certain_date$alpha.3!='ZAF'])
  Europe <- sum(certain_date$death[certain_date$region=='Europe'])/sum(certain_date$Total_cases[certain_date$region=='Europe'])
  Oceania <- sum(certain_date$death[certain_date$region=='Oceania'])/sum(certain_date$Total_cases[certain_date$region=='Oceania'])
  norA <- sum(certain_date$death[certain_date$sub.region=='Northern America'&certain_date$alpha.3!='USA'])/sum(certain_date$Total_cases[certain_date$sub.region=='Northern America'&certain_date$alpha.3!='USA'])
  souA <- sum(certain_date$death[certain_date$sub.region=='Latin America and the Caribbean'&certain_date$alpha.3!='MEX'&certain_date$alpha.3!='COL'&certain_date$alpha.3!='BRA'])/sum(certain_date$Total_cases[certain_date$sub.region=='Latin America and the Caribbean'&certain_date$alpha.3!='MEX'&certain_date$alpha.3!='COL'&certain_date$alpha.3!='BRA'])
  newcase <- rbind(newcase,cbind(Date = as.character(a),美国 = US, 印度 = India, 巴西 = Brazil, 南非 = SF, 哥伦比亚 = Colombia,
                                 墨西哥 = Mexico, '亚洲(除印度)'=Asia, '非洲(除南非)'=Africa, 欧洲 = Europe, 大洋洲 = Oceania,
                                 '北美洲(除美国)'=norA, '拉丁美洲(除墨西哥、巴西、哥伦比亚)'=souA))
}

write_xlsx(newcase,'/Users/qidiwang1/Desktop/分地区新增.xlsx')


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
