merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
sign <- read_excel('/Users/qidiwang1/Desktop/区域打标.xlsx')
data <- sign%>%
  left_join(merge,by='iso3_name')

EE <- sqldf('select Date, sum(Total_cases) as Total_cases, sum(Total_death) as Total_death, sum(Population) as Population from data where is_EE == 1 group by Date')
DM <- sqldf('select Date, sum(Total_cases) as Total_cases, sum(Total_death) as Total_death, sum(Population) as Population  from data where is_DM == 1 group by Date')
EU <- sqldf('select Date, sum(Total_cases) as Total_cases, sum(Total_death) as Total_death, sum(Population) as Population  from data where is_EU == 1 group by Date')
WE <- sqldf('select Date, sum(Total_cases) as Total_cases, sum(Total_death) as Total_death, sum(Population) as Population  from data where is_WE == 1 group by Date')
EE$Region <- 'EE'
DM$Region <- 'DM'
EU$Region <- 'EU'
WE$Region <- 'WE'

data2 <- rbind(EE,DM,EU,WE)
Jdate <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/JDATE.csv')
Jdate <- na.omit(Jdate)
Jdate$Date <- as.Date(Jdate$Date,'%m/%d/%y')
data2$Date <- as.Date(data2$Date)
data2 <- data2%>%
  left_join(Jdate,by='Date')
data2$Jdate
data <- data.frame()
for (a in levels(factor(data2$Region))){
  country <- data2[data2$Region==a,]
  Jdate <- 0
  determine_Jdate <- 0
  doubling_day <- c()
  for (i in c(1:nrow(country))){
    cutoff <- country$Total_cases[i]/2
    subgroup <- country[country$Total_cases<=cutoff&country$Jdate<=country$Jdate[i],]
    Jdate <- tail(subgroup$Jdate,1)
    
    min_confirm <- tail(subgroup$Total_cases,1)
    max_confirm <- country[country$Jdate==(Jdate+1),]$Total_cases
    determine_Jdate <- Jdate + (cutoff-min_confirm)/(max_confirm-min_confirm)
    if (nrow(subgroup)==0){
      days <- 0 
      doubling_day <- c(doubling_day,days)
    } else if (country$Total_cases[i]==0) {
      days <- 0 
      doubling_day <- c(doubling_day,days)
    } else {
      days <- country$Jdate[i]-determine_Jdate
      doubling_day <- c(doubling_day,days)
    }
  }
  country$confirm_doubling_days <- doubling_day
  data <- rbind(data,country)
}



#以下为对Raw Death Doubling Days与Raw Confirm Doubling Days进行7天平滑处理，country$death_doubling_days请贴入Raw Death Doubling Days那一列
merge <- data.frame()
for (a in levels(factor(data$Region))){
  country <- data[data$Region==a,]
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


merge <- replace.value(merge,'smooth_confirm_doubling_days',0,as.double(NA))
write_xlsx(merge,'/Users/qidiwang1/Desktop/Region_doubling_days.xlsx')





