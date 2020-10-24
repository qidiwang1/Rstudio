data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
loss <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/loss.csv')
gdp <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/gdp$relative.csv')
merge <- data.frame(JDATE = data$JDATE, Date = data$Date, index_dt = as.numeric(as.character(as.Date(data$Date),"%Y%m%d")),
                    Country = data$Country, Total_death = data$death, Total_recover = data$Recover, Total_cases = data$confirm,
                    Total_tests = data$Total.tests, New_death = data$New.death,New_recover = data$New_Recover, New_case = data$New.cases, 
                    Death_per_Million = data$death/data$Population*1000000, Recover_per_Million = data$Recover/data$Population*1000000, 
                    Cases_per_Million = data$confirm/data$Population*1000000, Tests_per_Million = data$Total.tests/data$Population*1000000,
                    Fatality_rate = data$death/data$confirm*100, Recovery_rate = data$Recover/data$confirm*100, Rt = data$RT, 
                    Daily_Economics_Policy_Score = data$Daily_Economics_Policy_Score,
                    Confirm_Doubling_Days = data$confirm_doubling_days,
                    smooth_confirm_doubling_days = data$smooth_confirm_doubling_days, Covid_Wave = data$COVID.wave,
                    Area_ID = data$area_id, iso2_name = data$alpha.2, iso3_name = data$alpha.3,
                    Population = data$Population, Development = data$Development, Income = data$Income, Region = data$region,
                    Sub_region = data$sub.region,
                    First_Industry = data$First_Industry, Second_Industry = data$Second_Industry, Third_Industry = data$Third_Industry,
                    C1_School.closing = data$C1_School.closing, C1_Flag = data$C1_Flag, C2_Workplace.closing = data$C2_Workplace.closing,
                    C2_Flag = data$C2_Flag, C3_Cancel.public.events = data$C3_Cancel.public.events, C3_Flag = data$C3_Flag, 
                    C4_Restrictions.on.gatherings = data$C4_Restrictions.on.gatherings, C4_Flag = data$C4_Flag, C5_Close.public.transport = data$C5_Close.public.transport,
                    C5_Flag = data$C5_Flag, C6_Stay.at.home.requirements = data$C6_Stay.at.home.requirements, C6_Flag = data$C6_Flag, 
                    C7_Restrictions.on.internal.movement = data$C7_Restrictions.on.internal.movement, C7_Flag = data$C7_Flag, 
                    C8_International.travel.controls = data$C8_International.travel.controls, E1_Income.support = data$E1_Income.support,
                    E1_Flag = data$E1_Flag, E2_Debt.contract.relief = data$E2_Debt.contract.relief, E3_Fiscal.measures = data$E3_Fiscal.measures, 
                    E4_International.support = data$E4_International.support, H1_Public.information.campaigns = data$H1_Public.information.campaigns,
                    H1_Flag = data$H1_Flag, H2_Testing.policy = data$H2_Testing.policy, H3_Contact.tracing = data$H3_Contact.tracing, 
                    H4_Emergency.investment.in.healthcare = data$H4_Emergency.investment.in.healthcare, H5_Investment.in.vaccines = data$H5_Investment.in.vaccines,
                    M1_Wildcard = data$M1_Wildcard)

merge$iso3_name <- as.character(merge$iso3_name)
gdp$iso3_name <- as.character(gdp$iso3_name)
data <- merge%>%
  left_join(gdp[,c('iso3_name','index_dt','mobility_7d','gdp_relative')], by = c('index_dt','iso3_name'))
data$mobility_7d[data$mobility_7d>1] <- 1


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




library(anchors)
stage <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/stage.csv')
#stage <- replace.value(stage, '到达recover大于cases日期', from='', to=NA, verbose = FALSE)
stage$开始下落日期 <- as.Date(stage$开始下落日期, '%Y/%m/%d')
stage$到达自由落体终点日期 <- as.Date(stage$到达自由落体终点日期, '%Y/%m/%d')
stage$到达recover大于cases日期 <- as.Date(stage$到达recover大于cases日期, '%Y/%m/%d')

data2$Stage <- NA
data2$Date <- as.Date(data2$Date)

for (i in c(1:nrow(stage))){
  if (stage$Stage[i]=='Stage1'){
    data2$Stage[data2$iso3_name==stage$iso3_name[i]] <- 'Preparation'
  } else if (stage$Stage[i]=='Stage2'){
    data2$Stage[data2$iso3_name==stage$iso3_name[i]] <- 'Preparation'
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date>=stage$开始下落日期[i]] <- 'Response'
  } else if (stage$Stage[i]=='Stage3'){
    data2$Stage[data2$iso3_name==stage$iso3_name[i]] <- 'Preparation'
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date>=stage$开始下落日期[i]] <- 'Response'
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date>=stage$到达自由落体终点日期[i]] <- 'Trough'
  } else if (stage$Stage[i]=='Stage4'){
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date<stage$开始下落日期[i]] <- 'Preparation'
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date>=stage$开始下落日期[i]] <- 'Response'
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date>=stage$到达自由落体终点日期[i]] <- 'Trough'
    data2$Stage[data2$iso3_name==stage$iso3_name[i] & data2$Date>=stage$到达recover大于cases日期[i]] <- 'Recovery'
  }
}

data <- data.frame()
for (a in levels(data2$Country)){
  country <- data2[data2$Country==a,]
  confirm_7day <- c()
  death_7day <- c()
  for (i in c(1:6)){
    confirm_7day <- c(confirm_7day,as.double(NA))
    death_7day <- c(death_7day,as.double(NA))
  }
  for (i in c(7:nrow(country))){
    confirm_7day <- c(confirm_7day,mean(country$New_case[(i-6):i]))
    death_7day <- c(death_7day, mean(country$New_death[(i-6):i]))
  }
  confirm_ratio <- c()
  death_ratio <- c()
  for (i in 1:nrow(country)){
    confirm_ratio <- c(confirm_ratio,confirm_7day[i]/max(confirm_7day,na.rm = TRUE))
    death_ratio <- c(death_ratio,death_7day[i]/max(death_7day,na.rm = TRUE))
  }
  country$confirm_ratio <- confirm_ratio
  country$death_ratio <- death_ratio
  data <- rbind(data,country)
}

google <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/google.csv')
google <- google[google$sub_region_1==''&google$metro_area=='',]
google$iso2_name <- as.character(google$country_region_code)
google$index_dt <- as.numeric(as.character(as.Date(google$date),'%Y%m%d'))
data$iso2_name <- as.character(data$iso2_name)
data <- data%>%
  left_join(google[,c(9:ncol(google))],by=c('index_dt','iso2_name'))

library(writexl)

write_xlsx(data,paste('/Users/qidiwang1/Desktop/PET_DATA_',as.character(max(data2$index_dt)),'.xlsx',sep = ''))
