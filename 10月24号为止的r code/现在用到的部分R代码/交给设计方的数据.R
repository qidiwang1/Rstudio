library(dplyr)
merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
loss <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/loss.csv')
gdp <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/gdp$relative.csv')
stage <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/stage.csv')

gdp <- gdp[gdp$iso3_name!= 'GNB' & gdp$iso3_name!='LIE',]
gdp$iso3_name <- factor(gdp$iso3_name)

merge <- merge[merge$iso3_name%in%levels(gdp$iso3_name),]
merge$iso3_name <- factor(merge$iso3_name)
merge$iso3_name <- as.character(merge$iso3_name)
gdp$iso3_name <- as.character(gdp$iso3_name)

data <- merge%>%
  left_join(gdp, by = c('index_dt','iso3_name'))

stage$iso3_name <- as.character(stage$iso3_name)
data2 <- data%>%
  left_join(stage, by = 'iso3_name')

average_loss <- data.frame()
for (i in c(2:ncol(loss))){
  country <- data.frame(iso3_name = as.character(names(loss)[i]),
                        index_dt = loss$iso3_name,
                        average_loss = loss[,i])
  average_loss <- rbind(average_loss, country)
}

average_loss$iso3_name <- as.character(average_loss$iso3_name)
data3 <- data2%>%
  left_join(average_loss, by = c('iso3_name', 'index_dt'))

data3$million_test <- data3$Total_tests/data3$Population*1000000
data3$million_death <- data3$Total_death/data3$Population*1000000
data3$million_confirm <- data3$Total_cases/data3$Population*1000000
library(anchors)
data3 <- replace.value(data3, '到达recover大于cases日期', from='', to=NA, verbose = FALSE)

data3$开始下落日期 <- as.Date(data3$开始下落日期, '%Y/%m/%d')
data3$到达自由落体终点日期 <- as.Date(data3$到达自由落体终点日期, '%Y/%m/%d')
data3$到达recover大于cases日期 <- as.Date(data3$到达recover大于cases日期, '%Y/%m/%d')

data3$Date <- as.Date(data3$Date)

data3$Country.x <- factor(data3$Country.x)
data4 <- data.frame()
for (a in levels(data3$Country.x)){
  country <- data3[data3$Country.x==a,]
  if (country$Stage[1]=='Stage4'){
    country$lasting_day <- country$Date-country$开始下落日期+1
    country$lasting_day[country$lasting_day<0] <- 0
    max_day <- country$到达recover大于cases日期[1]-country$开始下落日期[1]+1
    country$lasting_day[country$lasting_day>max_day] <- max_day
  } else if (country$Stage[1] == 'Stage3'|country$Stage[1]=='Stage2'){
    country$lasting_day <- country$Date-country$开始下落日期+1
    country$lasting_day[country$lasting_day<0] <- 0
  } else if (country$Stage[1] == 'Stage1'){
    country$lasting_day <- 0
  }
  data4 <- rbind(data4, country)
}


data5 <- data.frame()
for (a in levels(data4$Country.x)){
  country <- data4[data4$Country.x==a,]
  if (country$Stage[1]=='Stage1'){
    country$tm_stage <- 'Preparation'
  } else if (country$Stage[1]=='Stage2') {
    country$tm_stage <- 'Preparation'
    country$tm_stage[country$Date>=country$开始下落日期[1]] <- 'Response'
  } else if (country$Stage[1]=='Stage3') {
    country$tm_stage <- 'Preparation'
    country$tm_stage[country$Date>=country$开始下落日期[1]&country$Date<country$到达自由落体终点日期[1]] <- 'Response'
    country$tm_stage[country$Date>=country$到达自由落体终点日期[1]] <- 'Trough'
  } else if (country$Stage[1]=='Stage4') {
    country$tm_stage <- 'Preparation'
    country$tm_stage[country$Date>=country$开始下落日期[1]&country$Date<country$到达自由落体终点日期[1]] <- 'Response'
    country$tm_stage[country$Date>=country$到达自由落体终点日期[1]&country$Date<country$到达recover大于cases日期[1]] <- 'Trough'
    country$tm_stage[country$Date>=country$到达recover大于cases日期[1]] <- 'Recovery'
  }
  data5 <- rbind(data5, country)
}


data6 <- data.frame()
for (a in levels(data5$Country.x)){
  country <- data5[data5$Country.x==a,]
  country$until_today <- 0
  country$until_today[country$tm_stage!='Preparation'] <- (country[country$tm_stage!='Preparation',]$Date - country$开始下落日期[1])+1
  country$CN_names[is.na(country$CN_names)] <- country$CN_names[is.na(country$CN_names)==FALSE][1]
  data6 <- rbind(data6, country)

}

data6$fatality_rate <- data6$Total_death/data6$Total_cases
data6 <- replace.value(data6, 'fatality_rate', from='NaN', to=NA, verbose = FALSE)
data6 <- replace.value(data6, 'fatality_rate', from='Inf', to=NA, verbose = FALSE)
data6$recover_rate <- data6$Total_recover/data6$Total_cases
data6 <- replace.value(data6, 'recover_rate', from='NaN', to=NA, verbose = FALSE)
data6 <- replace.value(data6, 'recover_rate', from='Inf', to=NA, verbose = FALSE)
data6$area_chn <- '亚洲'
data6$area_chn[data6$Region=='Africa'] <- '非洲'
data6$area_chn[data6$Region=='Europe'] <- '欧洲'
data6$area_chn[data6$Region=='Oceania'] <- '大洋洲'
data6$area_chn[data6$Sub_region=="Northern America"] <- '北美洲'
data6$area_chn[data6$Sub_region=="Latin America and the Caribbean"] <- '拉丁美洲'
data6$Region <- as.character(data6$Region)
data6$Region[data6$Sub_region=="Northern America"] <- "Northern America"
data6$Region[data6$Sub_region=="Latin America and the Caribbean"] <- "Latin America and the Caribbean"

data6$average_loss <- as.numeric(as.character(data6$average_loss))
data6 <- data6[data6$index_dt<=max(gdp$index_dt),]
data6 <- data6[data6$iso3_name!='LIE'&data6$iso3_name!='GNB'&data6$iso3_name!='TWN'&data6$iso3_name!='HKG',]

data6$Country.x<-factor(data6$Country.x)

data7 <- data.frame()
for (a in levels(data6$Country.x)){
  country <- data6[data6$Country.x==a,]
  country$last_day_eco <- country$gdp_relative
  for (i in c(2:nrow(country))){
    if (is.na(country$Rt[i])& is.na(country$Rt[i-1])==FALSE)
      country$Rt[i] <- country$Rt[i-1]
    if (is.na(country$mobility_7d[i]) & is.na(country$mobility_7d[i-1])==FALSE)
      country$mobility_7d[i] <-country$mobility_7d[i-1]
    if (is.na(country$last_day_eco[i] & is.na(country$last_day_eco[i-1])==FALSE))
      country$last_day_eco[i] <- country$last_day_eco[i-1]
  }
  data7 <- rbind(data7,country)
}

data7$CN_names <- as.character(data7$CN_names)
data7 <- replace.value(data7, 'CN_names', from='中国大陆', to='中国', verbose = FALSE)

data7$chn_stage <- '恢复期'

data7$chn_stage[data7$tm_stage=="Preparation"]<- '准备期'
data7$chn_stage[data7$tm_stage=="Response"] <- '应对期'
data7$chn_stage[data7$tm_stage=="Trough"] <- '低谷期'
data7 <- data7[data7$Date>='2020-01-16',]

data7$gdp_relative[is.na(data7$gdp_relative)&data7$index_dt<=20200505]<-1

data7$gdp_relative_fill <- data7$gdp_relative
for (i in c(1:nrow(data7))){
  if (is.na(data7$gdp_relative_fill[i]))
    data7$gdp_relative_fill[i] <- data7$gdp_relative_fill[i-1]
}

data7$first_day <- 0
data8 <- data.frame()
for (a in levels(data7$Country.x)){
  country <- data7[data7$Country.x==a,]
  country$first_day[which(country$chn_stage=='应对期')[1]] <- 1
  country$first_day[which(country$chn_stage=='低谷期')[1]] <- 2
  country$first_day[which(country$chn_stage=='恢复期')[1]] <- 3
  data8 <- rbind(data8,country)
}
library(readxl)
PET <- read_excel('/Users/qidiwang1/Desktop/疫情数据库/PET-selected-countries.xlsx')
PET$`IMF-A&E`
data9 <- data8%>%
  left_join(PET[,c('iso3_name','IMF-A&E')],by='iso3_name')
data9$`IMF-A&E`[is.na(data9$`IMF-A&E`)] <- 'Developing'
data6 <- data9
final_table <- data.frame(date = data6$index_dt,
                          country = data6$CN_names,
                          country_eng = data6$Country.x,
                          area = data6$area_chn,
                          area_eng = data6$Region,
                          id_1 = round(data6$million_test,1),
                          id_2 = round(data6$million_confirm,1),
                          id_3 = round(data6$million_death,1),
                          id_4 = round(data6$gdp_relative*100,1),
                          id_5 = round(data6$lasting_day),
                          id_6 = round(data6$Daily_Economics_Policy_Score*100),
                          id_7 = data6$tm_stage,
                          id_8 = round(data6$mobility_7d*100,1),
                          id_9 = round(data6$Rt,1),
                          id_10 = data6$smooth_confirm_doubling_days.x,
                          id_11 = round(data6$average_loss*100,1),
                          id_12 = data6$until_today,
                          id_13 = data6$iso3_name,
                          id_14 = round(data6$fatality_rate*100,1),
                          id_15 = round(data6$recover_rate*100,1),
                          id_16 = data6$chn_stage,
                          id_17 = round(data6$gdp_relative_fill*100,1),
                          id_18 = data6$first_day,
                          id_19 = data6$`IMF-A&E`)
final_table$country_eng <- gsub('_', ' ',final_table$country_eng)
View(final_table)
library(writexl)
write_xlsx(final_table, '/Users/qidiwang1/Desktop/by_country.xlsx')
write.csv(final_table,'/Users/qidiwang1/Desktop/by_country.csv')

asia <- data6[data6$Region=='Asia',]
europe <- data6[data6$Region=='Europe',]
oceania <- data6[data6$Region=='Oceania',]
africa <- data6[data6$Region=='Africa',]
north_america <- data6[data6$Sub_region=="Northern America",]
latin <- data6[data6$Sub_region=="Latin America and the Caribbean",]

by_area <- data.frame()
for (i in c(1:max(data6$JDATE))){
  asia_day <- asia[asia$JDATE==i,]
  europe_day <- europe[europe$JDATE==i,]
  oceania_day <- oceania[oceania$JDATE==i,]
  africa_day <- africa[africa$JDATE==i,]
  north_america_day <- north_america[north_america$JDATE==i,]
  latin_day <- latin[latin$JDATE==i,]
  world_day <- data6[data6$JDATE==i,]
  asia_million_death <- round(sum(asia_day$Total_death)/sum(asia_day$Population)*1000000,1)
  europe_million_death <- round(sum(europe_day$Total_death)/sum(europe_day$Population)*1000000,1)
  oceania_million_death <- round(sum(oceania_day$Total_death)/sum(oceania_day$Population)*1000000,1)
  africa_million_death <- round(sum(africa_day$Total_death)/sum(africa_day$Population)*1000000,1)
  north_america_million_death <- round(sum(north_america_day$Total_death)/sum(north_america_day$Population)*1000000,1)
  latin_million_death <- round(sum(latin_day$Total_death)/sum(latin_day$Population)*1000000,1)
  world_million_death <- round(sum(world_day$Total_death)/sum(world_day$Population)*1000000,1)
  asia_million_confirm <- round(sum(asia_day$Total_cases)/sum(asia_day$Population)*1000000,1)
  europe_million_confirm <- round(sum(europe_day$Total_cases)/sum(europe_day$Population)*1000000,1)
  oceania_million_confirm <- round(sum(oceania_day$Total_cases)/sum(oceania_day$Population)*1000000,1)
  africa_million_confirm <- round(sum(africa_day$Total_cases)/sum(africa_day$Population)*1000000,1)
  north_america_million_confirm <- round(sum(north_america_day$Total_cases)/sum(north_america_day$Population)*1000000,1)
  latin_million_confirm <- round(sum(latin_day$Total_cases)/sum(latin_day$Population)*1000000,1)
  world_million_confirm <- round(sum(world_day$Total_cases)/sum(world_day$Population)*1000000,1)
  asia_million_test <- round(sum(asia_day[is.na(asia_day$Total_tests)==FALSE,]$Total_tests)/sum(asia_day[is.na(asia_day$Total_tests)==FALSE,]$Population)*1000000,1)
  europe_million_test <- round(sum(europe_day[is.na(europe_day$Total_tests)==FALSE,]$Total_tests)/sum(europe_day[is.na(europe_day$Total_tests)==FALSE,]$Population)*1000000,1)
  oceania_million_test <- round(sum(oceania_day[is.na(oceania_day$Total_tests)==FALSE,]$Total_tests)/sum(oceania_day[is.na(oceania_day$Total_tests)==FALSE,]$Population)*1000000,1)
  africa_million_test <- round(sum(africa_day[is.na(africa_day$Total_tests)==FALSE,]$Total_tests)/sum(africa_day[is.na(africa_day$Total_tests)==FALSE,]$Population)*1000000,1)
  north_america_million_test <- round(sum(north_america_day[is.na(north_america_day$Total_tests)==FALSE,]$Total_tests)/sum(north_america_day[is.na(north_america_day$Total_tests)==FALSE,]$Population)*1000000,1)
  latin_million_test <- round(sum(latin_day[is.na(latin_day$Total_tests)==FALSE,]$Total_tests)/sum(latin_day[is.na(latin_day$Total_tests)==FALSE,]$Population)*1000000,1)
  world_million_test <- round(sum(world_day[is.na(world_day$Total_tests)==FALSE,]$Total_tests)/sum(world_day[is.na(world_day$Total_tests)==FALSE,]$Population)*1000000,1)
  asia_fatality <- round(sum(asia_day$Total_death)/sum(asia_day$Total_cases)*100,1)
  europe_fatality <- round(sum(europe_day$Total_death)/sum(europe_day$Total_cases)*100,1)
  oceania_fatality <- round(sum(oceania_day$Total_death)/sum(oceania_day$Total_cases)*100,1)
  africa_fatality <- round(sum(africa_day$Total_death)/sum(africa_day$Total_cases)*100,1)
  north_america_fatality <- round(sum(north_america_day$Total_death)/sum(north_america_day$Total_cases)*100,1)
  latin_fatality <- round(sum(latin_day$Total_death)/sum(latin_day$Total_cases)*100,1)
  world_fatality <- round(sum(world_day$Total_death)/sum(world_day$Total_cases)*100,1)
  asia_mobility <- round(mean(asia_day$mobility_7d,na.rm = TRUE)*100,1)
  europe_mobility <- round(mean(europe_day$mobility_7d,na.rm = TRUE)*100,1)
  oceania_mobility <- round(mean(oceania_day$mobility_7d,na.rm = TRUE)*100,1)
  africa_mobility <- round(mean(africa_day$mobility_7d,na.rm = TRUE)*100,1)
  north_america_mobility <- round(mean(north_america_day$mobility_7d,na.rm = TRUE)*100,1)
  latin_mobility <- round(mean(latin_day$mobility_7d,na.rm = TRUE)*100,1)
  world_mobility <- round(mean(world_day$mobility_7d,na.rm = TRUE)*100,1)
  asia_gdp <- round(mean(asia_day$gdp_relative,na.rm = TRUE)*100,1)
  europe_gdp <- round(mean(europe_day$gdp_relative,na.rm = TRUE)*100,1)
  oceania_gdp <- round(mean(oceania_day$gdp_relative,na.rm = TRUE)*100,1)
  africa_gdp <- round(mean(africa_day$gdp_relative,na.rm = TRUE)*100,1)
  north_america_gdp <- round(mean(north_america_day$gdp_relative,na.rm = TRUE)*100,1)
  latin_gdp <- round(mean(latin_day$gdp_relative,na.rm = TRUE)*100,1)
  world_gdp <- round(mean(world_day$gdp_relative,na.rm = TRUE)*100,1)
  asia_recovery <- round(sum(asia_day$Population[asia_day$tm_stage=='Recovery'])/sum(asia_day$Population)*100,1)
  europe_recovery <- round(sum(europe_day$Population[europe_day$tm_stage=='Recovery'])/sum(europe_day$Population)*100,1)
  oceania_recovery <- round(sum(oceania_day$Population[oceania_day$tm_stage=='Recovery'])/sum(oceania_day$Population)*100,1)
  africa_recovery <- round(sum(africa_day$Population[africa_day$tm_stage=='Recovery'])/sum(africa_day$Population)*100,1)
  north_america_recovery <- round(sum(north_america_day$Population[north_america_day$tm_stage=='Recovery'])/sum(north_america_day$Population)*100,1)
  latin_recovery <- round(sum(latin_day$Population[latin_day$tm_stage=='Recovery'])/sum(latin_day$Population)*100,1)
  world_recovery <- round(sum(world_day$Population[world_day$tm_stage=='Recovery'])/sum(world_day$Population)*100,1)
  ccc <- data.frame(date = asia_day$index_dt[1], 
                    area = c('全球','亚洲','欧洲','拉丁美洲', '北美洲','非洲','大洋洲'),
                    area_eng = c('World', 'Asia', 'Europe','Latin America', 'North America', 'Africa', 'Oceania'),
                    id_1 = c(world_million_test, asia_million_test, europe_million_test, latin_million_test, north_america_million_test, africa_million_test, oceania_million_test),
                    id_2 = c(world_million_confirm, asia_million_confirm, europe_million_confirm, latin_million_confirm, north_america_million_confirm, africa_million_confirm, oceania_million_confirm),
                    id_3 = c(world_million_death, asia_million_death, europe_million_death, latin_million_death, north_america_million_death, africa_million_death, oceania_million_death),
                    id_4 = c(world_fatality, asia_fatality, europe_fatality, latin_fatality, north_america_fatality, africa_fatality, oceania_fatality),
                    id_5 = c(world_mobility, asia_mobility, europe_mobility, latin_mobility, north_america_mobility, africa_mobility, oceania_mobility),
                    id_6 = c(world_gdp, asia_gdp, europe_gdp, latin_gdp, north_america_gdp, africa_gdp, oceania_gdp),
                    id_7 = c(world_recovery, asia_recovery, europe_recovery, latin_recovery, north_america_recovery, africa_recovery, oceania_recovery))
  by_area <- rbind(by_area,ccc)
}
by_area <- by_area[c(113:nrow(by_area)),]
by_area <- by_area[by_area$date==max(by_area$date),]
write_xlsx(by_area, '/Users/qidiwang1/Desktop/by_area.xlsx')




