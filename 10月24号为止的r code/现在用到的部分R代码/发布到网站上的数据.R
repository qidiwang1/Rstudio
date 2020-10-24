data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
loss <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/loss.csv')
gdp <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/gdp$relative.csv')
merge <- data.frame(Date = data$Date, index_dt = as.numeric(as.character(as.Date(data$Date,'%m/%d/%y'),'%Y%m%d')),
                    Country = data$Country, Total_deaths = data$death, Total_recoveries = data$Recover, Total_cases = data$confirm,
                    Total_tests = data$Total.tests, New_deaths = data$New.death,New_recoveries = data$New_Recover, New_cases = data$New.cases, 
                    Deaths_per_Million = data$death/data$Population*1000000, Recoveries_per_Million = data$Recover/data$Population*1000000, 
                    Cases_per_Million = data$confirm/data$Population*1000000, Tests_per_Million = data$Total.tests/data$Population*1000000,
                    Fatality_rate = data$death/data$confirm*100, Recovery_rate = data$Recover/data$confirm*100,
                    Policy_stringency_index = data$Daily_Economics_Policy_Score, 
                    Doubling_Days = data$smooth_confirm_doubling_days, iso2_name = data$alpha.2, iso3_name = data$alpha.3,
                    Population = data$Population, Development = data$Development, Income = data$Income, Region = data$region,
                    Sub_region = data$sub.region,
                    First_industry = data$First_Industry, Second_industry = data$Second_Industry, Third_industry = data$Third_Industry)

merge$iso3_name <- as.character(merge$iso3_name)
gdp$iso3_name <- as.character(gdp$iso3_name)
data <- merge%>%
  left_join(gdp[,c('iso3_name','index_dt','mobility_7d','gdp_relative')], by = c('index_dt','iso3_name'))
data$mobility_7d[data$mobility_7d>1] <- 1
names(data)[names(data)=='mobility_7d'] <- 'Mobility'
names(data)[names(data)=='gdp_relative'] <- 'Economic_activity'

average_loss <- data.frame()
for (i in c(2:ncol(loss))){
  country <- data.frame(iso3_name = as.character(names(loss)[i]),
                        index_dt = loss$iso3_name,
                        average_loss = loss[,i])
  average_loss <- rbind(average_loss, country)
}

average_loss$iso3_name <- as.character(average_loss$iso3_name)
data2 <- data%>%
  left_join(average_loss, by = c('iso3_name', 'index_dt'))



stage <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/stage.csv')

library(anchors)
#stage <- replace.value(stage, '到达recover大于cases日期', from='', to=NA, verbose = FALSE)
stage$开始下落日期 <- as.Date(stage$开始下落日期, '%m/%d/%y')
stage$到达自由落体终点日期 <- as.Date(stage$到达自由落体终点日期, '%m/%d/%y')
stage$到达recover大于cases日期 <- as.Date(stage$到达recover大于cases日期, '%m/%d/%y')

data2$Stage <- NA
data2$Date <- as.Date(data2$Date,'%m/%d/%y')

for (i in c(1:nrow(stage))){
  if (stage$Stage[i]=='Stage1'){
    data2$Stage[data2$iso3_name==stage$iso3_name[i]] <- 'Others'
  } else if (stage$Stage[i]=='Stage2'){
    data2$Stage[data2$iso3_name==stage$iso3_name[i]] <- 'Others'
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date>=stage$开始下落日期[i]] <- 'Response'
  } else if (stage$Stage[i]=='Stage3'){
    data2$Stage[data2$iso3_name==stage$iso3_name[i]] <- 'Others'
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date>=stage$开始下落日期[i]] <- 'Response'
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date>=stage$到达自由落体终点日期[i]] <- 'Trough'
  } else if (stage$Stage[i]=='Stage4'){
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date<stage$开始下落日期[i]] <- 'Others'
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date>=stage$开始下落日期[i]] <- 'Response'
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date>=stage$到达自由落体终点日期[i]] <- 'Trough'
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date>=stage$到达recover大于cases日期[i]] <- 'Recovery'
  }
}

names(data2)[names(data2)=='average_loss']<-'Average_loss'
names(data2)[names(data2)=='Stage'] <- 'PET_phase'
data2 <- data2[,-2]

library(writexl)
write_xlsx(data2,paste('/Users/qidiwang1/Desktop/',as.character(as.Date(as.Date(Sys.time()))),'-Covid-19 Data.xlsx'))
system.time()
Sys.time()
as.Date(Sys.time())
