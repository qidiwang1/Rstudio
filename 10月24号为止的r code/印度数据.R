library(dplyr)
india <- read.csv('/Users/qidiwang1/Desktop/India/india_total.csv')
india <- data.frame(Date = india$Date,
                    State = india$State.UnionTerritory,
                    Total_cases = india$Confirmed,
                    Total_deaths = india$Deaths,
                    Total_recoveries = india$Cured)
india$Date <- as.Date(india$Date,'%d/%m/%y')

india <- india[order(india$Date),]
india <- india[order(india$State),]
india_data <- data.frame()
for (a in levels(india$State)){
  state <- india[india$State==a,]
  newcase <- c()
  newdeath <- c()
  newrecover <- c()
  for (i in c(1:nrow(state))){
    if (i == 1){
      newcase <- c(newcase,state$Total_cases[i])
      newdeath <- c(newdeath,state$Total_deaths[i])
      newrecover <- c(newrecover,state$Total_recoveries[i])
    } else{
      newcase <- c(newcase,(state$Total_cases[i]-state$Total_cases[i-1]))
      newdeath <- c(newdeath,(state$Total_deaths[i]-state$Total_deaths[i-1]))
      newrecover <- c(newrecover,(state$Total_recoveries[i]-state$Total_recoveries[i-1]))
    }
  }
  state$New_cases <- newcase
  state$New_deaths <- newdeath
  state$New_recoveries <- newrecover
  india_data <- rbind(india_data,state)
}
india <- india_data
india$State <- as.character(india$State)
india$State[india$State=='Dadra and Nagar Haveli and Daman and Diu'] <- 'Dadra and Nagar Haveli'
india$State[india$State=='Telengana'] <- 'Telangana'
india$index_dt <- as.numeric(as.character(as.Date(india$Date),'%Y%m%d'))
pop <- read.csv('/Users/qidiwang1/Desktop/India/印度名称匹配.csv')
pop$State <- as.character(pop$State)
india$State <- gsub(' ', '_',india$State)
pop$State <- gsub(' ', '_',pop$State)
india <- india[!is.na(india$Date),]
data <- india%>%
  left_join(pop, by = 'State')


time <- data.frame(Date = seq(min(india$Date),max(india$Date),'days'))

data$State <- factor(data$State)

india_data <- data.frame()
for (a in levels(data$State)){
  country <- data[data$State==a,]
  bb <- time%>%
    left_join(country, by = 'Date')
  for (i in c(1:nrow(bb))) {
    if (is.na(bb$State[i])==TRUE) 
      bb$State[i] <- as.character(a)
    if (is.na(bb$Population[i]==TRUE))
      bb$Population[i] <- as.numeric(as.character(country$Population[which(is.na(country$Population)==FALSE)][1]))
    if (i == 1){
      bb$Total_cases[i] <- ifelse(is.na(bb$Total_cases[i]),0,bb$Total_cases[i])
      bb$Total_recoveries[i] <- ifelse(is.na(bb$Total_recoveries[i]),0,bb$Total_recoveries[i])
      bb$Total_deaths[i] <- ifelse(is.na(bb$Total_deaths[i]),0,bb$Total_deaths[i])
    } else {
      bb$Total_cases[i] <- ifelse(is.na(bb$Total_cases[i]),bb$Total_cases[i-1],bb$Total_cases[i])
      bb$Total_recoveries[i] <- ifelse(is.na(bb$Total_recoveries[i]),bb$Total_recoveries[i-1],bb$Total_recover[i])
      bb$Total_deaths[i] <- ifelse(is.na(bb$Total_deaths[i]),bb$Total_deaths[i-1],bb$Total_deaths[i])
    }
    
  }
  india_data <- rbind(india_data,bb)
}

india_data$New_cases[is.na(india_data$New_cases)] <- 0
india_data$New_deaths[is.na(india_data$New_deaths)] <- 0
india_data$New_recoveries[is.na(india_data$New_recoveries)] <- 0

india_data$index_dt <- as.numeric(as.character(india_data$Date,'%Y%m%d'))


google <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/google.csv')
data <- google[google$country_region=='India'&google$sub_region_1!=''&google$sub_region_2=='',]
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
google_mobility$date <- as.Date(google_mobility$date)
india_mobility <- data.frame(index_dt = as.numeric(as.character(google_mobility$date,'%Y%m%d')),
                              State = google_mobility$sub_region_1,
                              ISO = google_mobility$iso_3166_2_code,
                              Mobility = google_mobility$google_avg,
                             Economic_Activity = 0.3886*google_mobility$google_avg+0.61)

india_mobility$State <- as.character(india_mobility$State)
india_data$State <- as.character(india_data$State)


india_data <- india_data%>%
  left_join(india_mobility, by = c('index_dt','State'))
View(india_mobility)

jdate <- read.csv('/Users/qidiwang1/Desktop/us_jdate.csv')
india_data <- india_data%>%
  left_join(jdate, by = 'index_dt')
india_data$State <- factor(india_data$State)
india_data$Total_cases <- as.numeric(as.character(india_data$Total_cases))
india_data <- india_data[is.na(india_data$Mobility)==FALSE,]
india_data$State <- factor(india_data$State)


merge <- data.frame()
for (a in levels(india_data$State)){
  country <- india_data[india_data$State==a,]
  Jdate <- 0
  determine_Jdate <- 0
  doubling_day <- c()
  for (i in c(1:nrow(country))){
    cutoff <- country$Total_cases[i]/2
    subgroup <- country[country$Total_cases<=cutoff,]
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
  merge <- rbind(merge,country)
}

library(writexl)
write_xlsx(merge,'/Users/qidiwang1/Desktop/India/India_data.xlsx')
#merge$area_name <- factor(merge$area_name)
#write_xlsx(data.frame(area_name = levels(merge$area_name)),'/Users/qidiwang1/Desktop/India/Inida_stage.xlsx')
#india <- read_excel('/Users/qidiwang1/Desktop/India/India_data.xlsx',sheet = 'Sheet1')
