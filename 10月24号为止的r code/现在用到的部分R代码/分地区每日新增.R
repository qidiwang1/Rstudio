library(writexl)
data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
data <- data[data$Country!='China_Exhubei'&data$Country!='China_Hubei',]
data$Date <- factor(data$Date)
newcase <- data.frame()
for (a in levels(data$Date)){
  certain_date <- data[data$Date==a,]
  Eastern_Asia <- sum(certain_date[certain_date$sub.region=="Eastern Asia",]$New.cases)
  MENA <- sum(certain_date[certain_date$sub.region=="Northern Africa"|certain_date$sub.region=="Western Asia",]$New.cases)
  Western_Europe <- sum(certain_date[certain_date$sub.region=="Western Europe"|certain_date$sub.region=="Southern Europe"|certain_date$sub.region=="Northern Europe",]$New.cases)
  Oceania <- sum(certain_date[certain_date$sub.region=="Australia and New Zealand"|certain_date$sub.region=="Melanesia"|certain_date$sub.region=="Micronesia",]$New.cases)
  North_America <- sum(certain_date[certain_date$sub.region=="Northern America",]$New.cases)
  Latin <- sum(certain_date[certain_date$sub.region=="Latin America and the Caribbean",]$New.cases)
  Rest_of_Asia <- sum(certain_date[certain_date$sub.region=="South-eastern Asia"|certain_date$sub.region=="Southern Asia"|certain_date$sub.region=="Central Asia",]$New.cases)
  Sub_Saharan <- sum(certain_date[certain_date$sub.region=="Sub-Saharan Africa",]$New.cases)
  Eastern_Europe <- sum(certain_date[certain_date$sub.region=="Eastern Europe",]$New.cases)
  newcase <- rbind(newcase,cbind(Date = as.character(a),Eastern_Asia=Eastern_Asia, MENA=MENA, Western_Europe=Western_Europe, Oceania=Oceania,
                                 North_America=North_America, Latin=Latin, Rest_of_Asia=Rest_of_Asia, Sub_Saharan= Sub_Saharan,
                                 Eastern_Europe=Eastern_Europe))
}

write_xlsx(newcase,'/Users/qidiwang1/Desktop/分地区每日新增病例.xlsx')

stage <- read.csv('/Users/qidiwang1/Desktop/by_country.csv')


stage$date <- factor(stage$date)
pop_country <- data.frame()
for (a in levels(stage$date)){
  certain_date <- stage[stage$date==a,]
  aaa <- data.frame(index_dt = as.numeric(as.character(a)),
                    Recovery = nrow(certain_date[certain_date$id_7=='Recovery',]),
                    Response = nrow(certain_date[certain_date$id_7=='Response',]),
                    Trough = nrow(certain_date[certain_date$id_7=='Trough',]),
                    Ohters = nrow(certain_date[certain_date$id_7=='Others',]))
  pop_country <- rbind(pop_country,aaa)
}
write_xlsx(pop_country,'/Users/qidiwang1/Desktop/分阶段国家数量.xlsx')
