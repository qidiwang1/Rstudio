merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
merge <- merge[merge$Sub_region=='Southern Asia',]
merge$Date <- factor(merge$Date)

confirm_data <- data.frame()
for (a in levels(merge$Date)){
  certain_date <- merge[merge$Date==a,]
  aaa <- data.frame(index_dt = certain_date$index_dt[1],
                    Total_cases = sum(certain_date$Total_cases),
                    Total_death = sum(certain_date$Total_death),
                    Population = sum(certain_date$Population))
  confirm_data <- rbind(confirm_data,aaa)
}

jdate <- read.csv('/Users/qidiwang1/Desktop/us_jdate.csv')
data <- confirm_data%>%
  left_join(jdate, by = 'index_dt')

Jdate <- 0
determine_Jdate <- 0
doubling_day <- c()
for (i in c(1:nrow(data))){
  cutoff <- data$Total_cases[i]/2
  subgroup <- data[data$Total_cases<=cutoff,]
  Jdate <- tail(subgroup$Jdate,1)
    
  min_confirm <- tail(subgroup$Total_cases,1)
  max_confirm <- data[data$Jdate==(Jdate+1),]$Total_cases
  determine_Jdate <- Jdate + (cutoff-min_confirm)/(max_confirm-min_confirm)
  if (nrow(subgroup)==0){
    days <- 0 
    doubling_day <- c(doubling_day,days)
  } else if (data$Total_cases[i]==0) {
    days <- 0 
    doubling_day <- c(doubling_day,days)
  } else {
    days <- data$Jdate[i]-determine_Jdate
    doubling_day <- c(doubling_day,days)
  }
}
data$confirm_doubling_days <- doubling_day
library(smoother)

smooth_confirm <- c()
for (i in c(1:6)){
  smooth_confirm <- c(smooth_confirm, as.double(NA))
}
for (i in (7:nrow(data))){
  smooth_confirm <- c(smooth_confirm, mean(data$confirm_doubling_days[(i-6):i]))
}

data$smooth_confirm_doubling_days <- smooth_confirm
data$smooth_confirm_doubling_days <- ifelse(is.na(data$smooth_confirm_doubling_days)==TRUE,
                                                 data$confirm_doubling_days,data$smooth_confirm_doubling_days)
library(writexl)
write_xlsx(data,'/Users/qidiwang1/Desktop/南亚数据.xlsx')
