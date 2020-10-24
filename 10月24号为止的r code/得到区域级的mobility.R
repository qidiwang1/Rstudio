google <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/google.csv')
View(google)
US <- google[google$country_region_code=='US'&google$sub_region_2==''& !is.na(google$country_region_code) & google$sub_region_1!='',]


get_mobility_data <- function(data){
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
  return(data.frame(index_dt = as.numeric(as.character(google_mobility$date,'%Y%m%d')),
                               State = google_mobility$sub_region_1,
                               ISO = google_mobility$iso_3166_2_code,
                               Mobility = google_mobility$google_avg,
                               Economic_Activity = 0.3886*google_mobility$google_avg+0.61))
  
}

US_mobility <- get_mobility_data(US)
write_xlsx(US_mobility[US_mobility$index_dt==20200925,],'/Users/qidiwang1/Desktop/US_mobility3.xlsx')


India <- google[google$country_region_code=='IN'&google$sub_region_2==''& !is.na(google$country_region_code) & google$sub_region_1!='',]
IN_mobility <- get_mobility_data(India)

write_xlsx(IN_mobility[IN_mobility$index_dt==20200925,],'/Users/qidiwang1/Desktop/IN_mobility2.xlsx')













