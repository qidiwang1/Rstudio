library(sqldf)
merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
merge <- merge[!is.na(merge$Total_tests),]

test_data <- data.frame()
for (a in levels(merge$Region)){
  region <- merge[merge$Region==a,]
  data <- sqldf('select Date, sum(Total_tests) as Total_tests, sum(Total_cases) as Total_cases,
                sum(Population) as Population from region group by Date')
  data$Region <- as.character(a)
  test_data <- rbind(test_data,data)
    
}

test_data$Population[test_data$Region=='Asia'] <- sum(merge[merge$Region=='Asia'&merge$index_dt==20201006,]$Population)

merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
ICL <- read.csv('/Users/qidiwang1/Desktop/ICL.csv')
ICL$Date <- as.Date(ICL$Date,'%m/%d/%y')
merge$Date <- as.Date(merge$Date)
ICL <- ICL[!is.na(ICL$ICL),]
ICL<- ICL%>%
  group_by(iso3_name)%>%
  mutate(ICL_total = cumsum(ICL))


library(dplyr)
data <- merge %>%
  left_join(ICL,by=c('iso3_name','Date'))

data2 <- data[data$iso3_name%in%levels(factor(ICL$iso3_name)),]

confirm_data <- data.frame()
for (a in levels(factor(data2$Region))){
  region <- data2[data2$Region==a,]
  data <- sqldf('select Date, sum(New_case) as New_case, sum(ICL) as ICL,
                sum(Population) as Population from region group by Date')
  data$Region <- as.character(a)
  confirm_data <- rbind(confirm_data,data)
  
}
confirm_data$Population[confirm_data$Region=='Asia'] <- sum(data2[data2$Region=='Asia'&data2$index_dt==20201006,]$Population)

library(writexl)
write_xlsx(test_data,'/Users/qidiwang1/Desktop/region_test_data.xlsx')
write_xlsx(confirm_data,'/Users/qidiwang1/Desktop/region_confirm_data.xlsx')
