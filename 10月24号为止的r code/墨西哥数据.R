library(dplyr)
confirm <- read.csv('/Users/qidiwang1/Desktop/Mexico/confirm.csv')
death <- read.csv('/Users/qidiwang1/Desktop/Mexico/death.csv')
time <- seq(as.Date('2020-01-12'),as.Date('2020-01-12')+ncol(confirm)-4,'days')
mexico_confirm <- data.frame()
for (i in c(1:nrow(confirm))){
  state <- data.frame(Date = time,
                      State = as.character(confirm$nombre[i]),
                      Population = as.numeric(as.character(confirm$poblacion[i])),
                      New_cases = colMeans(confirm[i,c(4:ncol(confirm))]))
  mexico_confirm <- rbind(mexico_confirm,state)
}

time2 <- seq(as.Date('2020-03-17'),as.Date('2020-03-17')+ncol(death)-4,'days')
mexico_death <- data.frame()
for (i in c(1:nrow(death))){
  state <- data.frame(Date = time2,
                      State = as.character(death$nombre[i]),
                      New_deaths = colMeans(death[i,c(4:ncol(death))]))
  mexico_death <- rbind(mexico_death,state)
}

mexico_data <- mexico_confirm%>%
  left_join(mexico_death,by = c('Date','State'))

mexico_data <- mexico_data%>%
  mutate(New_deaths = tidyr::replace_na(New_deaths,0))%>%
  group_by(State)%>%
  mutate(Total_cases = cumsum(New_cases),
         Total_deaths = cumsum(New_deaths))
mexico_data <- mexico_data[mexico_data$State!='Nacional',]
google <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/google.csv')
data <- google[google$country_region=='Mexico'&google$sub_region_1!='',]
data$index_1 <- rowMeans(data[,c('grocery_and_pharmacy_percent_change_from_baseline','retail_and_recreation_percent_change_from_baseline')],na.rm = TRUE)/100+1
data$index_2 <- (100+data$workplaces_percent_change_from_baseline)/100
data$google_index <- rowMeans(data[,c('index_1','index_2')],na.rm = TRUE)

data$sub_region_1 <- factor(data$sub_region_1)
google_mobility <- data.frame()
for (a in levels(data$sub_region_1)){
  state <- data[data$sub_region_1==a,]
  google_avg <- c()
  for (i in c(1:6)){
    google_avg <- c(google_avg,as.double(NA))
  }
  for (i in c(7:nrow(state))){
    google_avg <- c(google_avg,mean(state$google_index[(i-6):i],na.rm = TRUE))
  }
  state$google_avg <- google_avg
  google_mobility <- rbind(google_mobility,state)
}
google_mobility$date <- as.Date(google_mobility$date,'%m/%d/%y')
google_mobility$google_avg[google_mobility$google_avg>1] <- 1
mexico_mobility <- data.frame(Date = google_mobility$date,
                             State = google_mobility$sub_region_1,
                             ISO = google_mobility$iso_3166_2_code,
                             Mobiity = google_mobility$google_avg,
                             Economic_Activity = google_mobility$google_avg*0.3886+0.61)

mexico_mobility$State <- as.character(mexico_mobility$State)
mexico_data$State <- as.character(mexico_data$State)
mexico_data$State <- tolower(mexico_data$State)
mexico_mobility$State <- tolower(mexico_mobility$State)
mexico_mobility$State[mexico_mobility$State=='michoacán'] <- 'michoacan'
mexico_mobility$State[mexico_mobility$State=='querétaro'] <- 'queretaro'
mexico_data <- mexico_data[mexico_data$State!='mexico',]
mexico_covid <- mexico_data%>%
  left_join(mexico_mobility, by = c('Date','State'))
mexico_covid$index_dt <- as.numeric(as.character(as.Date(mexico_covid$Date),'%Y%m%d'))

jdate <- read.csv('/Users/qidiwang1/Desktop/us_jdate.csv')
mexico_covid <- mexico_covid%>%
  left_join(jdate, by = 'index_dt')

mexico_covid$State <- factor(mexico_covid$State)
library(tidyverse)
merge <- data.frame()
for (a in levels(mexico_covid$State)){
  country <- mexico_covid[mexico_covid$State==a,]
  Jdate <- 0
  determine_Jdate <- 0
  doubling_day <- c()
  for (i in c(1:nrow(country))){
    cutoff <- country$Total_cases[i]/2
    subgroup <- country[country$Total_cases<=cutoff & country$Jdate<=country$Jdate[i],]
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
  country$Doubling_Days <- doubling_day
  merge <- bind_rows(merge,country)
}

merge <- merge[is.na(merge$Mobiity)==FALSE,]
View(merge)
library(writexl)
write_xlsx(merge,'/Users/qidiwang1/Desktop/Mexico/墨西哥数据.xlsx')







