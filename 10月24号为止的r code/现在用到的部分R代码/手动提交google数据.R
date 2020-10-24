#读取Google数据，选择国家层面的数据，并计算Raw Google Index
google <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/google.csv')

google <- google[google$sub_region_1==''&google$metro_area=='',]

retail <- data.frame(entity = google$country_region_code,
           property = 'retail_and_recreation',
           time_series = as.character(as.Date(google$date),'%Y%m%d'),
           index_value = google$retail_and_recreation_percent_change_from_baseline)
grocery <- data.frame(entity = google$country_region_code,
                      property = 'grocery_and_pharmacy',
                      time_series = as.character(as.Date(google$date),'%Y%m%d'),
                      index_value = google$grocery_and_pharmacy_percent_change_from_baseline)
workplace <- data.frame(entity = google$country_region_code,
                        property = 'workplaces',
                        time_series = as.character(as.Date(google$date),'%Y%m%d'),
                        index_value = google$workplaces_percent_change_from_baseline)

google_data <- rbind(retail,grocery,workplace)
google_data$index_value[is.na(google_data$index_value)] <- ''
library(readxl)
google_iso <- read_excel('/Users/qidiwang1/Desktop/疫情数据库/google_iso_to_id.xlsx')
colnames(google_data)[1] <- 'iso2_name'
google_data$iso2_name <- as.character(google_data$iso2_name)
google_data$iso2_name[is.na(google_data$iso2_name)] <- 'NA'
google_data <- google_data%>%
  left_join(google_iso,by='iso2_name')


write.csv(google_data,'/Users/qidiwang1/Desktop/google.csv')
