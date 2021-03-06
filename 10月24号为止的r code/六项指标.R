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
    country$lasting_day <- country$Date-country$开始下落日期
    country$lasting_day[country$lasting_day<0] <- 0
    max_day <- country$到达recover大于cases日期[1]-country$开始下落日期[1]
    country$lasting_day[country$lasting_day>max_day] <- max_day
  } else if (country$Stage[1] == 'Stage3'|country$Stage[1]=='Stage2'){
    country$lasting_day <- country$Date-country$开始下落日期
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
  country$until_today[country$tm_stage!='Preparation'] <- (country[country$tm_stage!='Preparation',]$Date - country$开始下落日期[1])
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
world <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/gdp_world.csv')
world$iso3_name <- as.character(world$iso3_name)
data10 <- data9%>%
  left_join(world,by='iso3_name')

data11 <- data.frame()
for (a in levels(data10$Country.x)){
  country <- data10[data10$Country.x==a,]
  date = country$Date[country$gdp_relative_fill == min(country$gdp_relative_fill,na.rm = TRUE)]
  death = country$Total_death[country$gdp_relative_fill == min(country$gdp_relative_fill,na.rm = TRUE)]
  confirm = country$Total_cases[country$gdp_relative_fill == min(country$gdp_relative_fill,na.rm = TRUE)]
  country$trade_off1 <- (country$Total_death-death)/(country$gdp_relative_fill-min(country$gdp_relative_fill))/10
  country$trade_off1[country$Date<date] <- as.double(NA)
  country$trade_off2 <- (country$Total_cases-confirm)/(country$gdp_relative_fill-min(country$gdp_relative_fill))/10
  country$trade_off2[country$Date<date] <- as.double(NA)
  country$trade_off3 <- (country$Total_death-death)/((country$gdp_relative_fill-min(country$gdp_relative_fill))*country$X2019/1000000)
  country$trade_off3[country$Date<date] <- as.double(NA)
  country$trade_off4 <- (country$Total_cases-confirm)/((country$gdp_relative_fill-min(country$gdp_relative_fill))*country$X2019/1000000)
  country$trade_off4[country$Date<date] <- as.double(NA)
  data11 <- rbind(data11,country)
}

data6 <- data11
final_table <- data.frame(date = data6$index_dt,
                          country = data6$CN_names,
                          country_eng = data6$Country.x,
                          area = data6$area_chn,
                          area_eng = data6$Region,
                 
                          Cases_per_Million = round(data6$million_confirm,1),
                          Deaths_per_Million = round(data6$million_death,1),
               
                          Economic_contraction_days = round(data6$lasting_day),
                      
                          Cumulative_economic_loss = round(data6$average_loss*100,1),
                     
                          Recovery_rate = round(data6$recover_rate*100,1),
                          
                          thousand_gdp_recovery_per_death = data6$trade_off1,
                          million_dollars_per_death = data6$trade_off3,
                          thousand_gdp_recovery_per_confirm = data6$trade_off2,
                          million_dollars_per_confirm = data6$trade_off4,
                          IMF_AE = data6$`IMF-A&E`)
final_table$country_eng <- gsub('_', ' ',final_table$country_eng)
write_xlsx(final_table, '/Users/qidiwang1/Desktop/index_table.xlsx')





