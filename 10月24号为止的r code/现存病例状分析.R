merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
#merge$Date <- factor(merge$Date)
merge$Active_cases <- merge$Total_cases-merge$Total_death-merge$Total_recover
data <- data.frame()
for (a in levels(merge$Date)){
  certain_date <- merge[merge$Date == a,]
  Asia <- sum(certain_date$Active_cases[certain_date$Region=='Asia'])
  Africa <- sum(certain_date$Active_cases[certain_date$Region=='Africa'])
  Europe <- sum(certain_date$Active_cases[certain_date$Region=='Europe'])
  North_america <- sum(certain_date$Active_cases[certain_date$Sub_region=='Northern America'])
  Latin <- sum(certain_date$Active_cases[certain_date$Sub_region=='Latin America and the Caribbean'])
  Oceania <- sum(certain_date$Active_cases[certain_date$Region == 'Oceania'])
  data <- rbind(data, data.frame(Date = as.character(a),
              Asia = Asia,
              Europe = Europe,
              Africa = Africa,
              Oceania = Oceania,
              North_America = North_america,
              latin = Latin))
}
write.csv(data,'/Users/qidiwang1/Desktop/data.csv')
europe <- merge[merge$Region=='Europe'&merge$Active_cases>=200000,]
europe$Date <- as.Date(europe$Date)
ggplot()+
  geom_line(aes(x=europe$Date,y=europe$Active_cases,color = factor(europe$Country)))

today <- merge[merge$index_dt==20200810,]
last <- merge[merge$index_dt==20200802,]
sum(today$Active_cases)
6780328-6463226
317102/7
sum(today$Active_cases[today$Sub_region=='Northern America'])/sum(today$Active_cases)
sum(today$Active_cases[today$Region=='Asia'])/sum(today$Active_cases)
sum(today$Active_cases[today$Region=='Europe'])/sum(today$Active_cases)
sum(today$Active_cases[today$Sub_region=='Latin America and the Caribbean'])/sum(today$Active_cases)
sum(today$Active_cases[today$Country=='Bangladesh'])/sum(today$Active_cases[today$Region=='Asia'])
sum(today$Active_cases[today$Country=='Russia'])/sum(today$Active_cases[today$Region=='Europe'])
sum(today$Active_cases[today$Country=='United_Koingdom'])/sum(today$Active_cases[today$Region=='Europe'])
sum(today$Active_cases[today$iso2_name=='UK'])/sum(today$Active_cases[today$Region=='Europe'])


sum(today$Active_cases[today$Country=='Brazil'])/sum(today$Active_cases[today$Sub_region=='Latin America and the Caribbean'])
sum(today$Active_cases[today$Country=='Peru'])/sum(today$Active_cases[today$Sub_region=='Latin America and the Caribbean'])
sum(today$Active_cases[today$Country=='Colombia'])/sum(today$Active_cases[today$Sub_region=='Latin America and the Caribbean'])
sum(today$Active_cases[today$Country=='Argentina'])/sum(today$Active_cases[today$Sub_region=='Latin America and the Caribbean'])
sum(today$Active_cases[today$Country=='Mexico'])/sum(today$Active_cases[today$Sub_region=='Latin America and the Caribbean'])

View(today)
today$Total_cases[today$Country=='Bangladesh']/today$Total_tests[today$Country=='Bangladesh']
today$Total_tests[today$Country=='Bangladesh']/today$Population[today$Country=='Bangladesh']*1000000
today$Active_cases[today$iso2_name.x == 'US']/sum(today$Active_cases[today$Sub_region=='Northern America'])
3178728/3183700

mean(today$Total_cases/today$Total_tests,na.rm = TRUE)
mean(today$Total_tests/today$Population*1000000,na.rm = TRUE)
today$Total_cases[today$Country=='Brazil']/today$Total_tests[today$Country=='Brazil']
china_name <- read_excel('/Users/qidiwang1/Desktop/百万美元死多少人.xlsx',sheet = 'result')
china_name$iso3_name <- as.character(china_name$iso3_name)
today$iso3_name <- as.character(today$iso3_name)
today <- today%>%
  left_join(china_name, by = 'iso3_name')
write_xlsx(today,'/Users/qidiwang1/Desktop/today222.xlsx')


