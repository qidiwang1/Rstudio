merge<-read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
merge<-merge%>%
  group_by(Country)%>%
  mutate(newcase_7d=rollmean(New_case,7,fill=NA,align='right'),
         newdeath_7d=rollmean(New_death,7,fill=NA,align='right'),
         New_test = c(0,diff(Total_tests)))


Europe <- merge[merge$Region=='Europe',]
USA <- merge[merge$Country=='United_States_of_America',]
france <- merge[merge$Country=='France',]
max(france$newcase_7d[france$index_dt<=20200531],na.rm=TRUE)
spain <- merge[merge$Country=='Spain',]
max(spain$newcase_7d[spain$index_dt<=20200531],na.rm=TRUE)
USA$newcase_7d[USA$index_dt==20200925]
today <- Europe[Europe$index_dt==20200925,]
sum(today$newcase_7d)
today<-merge[merge$index_dt==20200925,]
sum(today$newcase_7d)
lastday<-merge[merge$index_dt==20200911,]
sum(lastday$newcase_7d)
new_data <- sqldf("select Date,sum(newcase_7d) from merge group by Date")
sum(today$Total_death)

levels(merge$Sub_region)
View(merge[merge$Sub_region%in%c('Western Europe','Northern Europe','Southern Europe'),])

estimate <- read.csv('/Users/qidiwang1/Desktop/ICL.csv')

estimate$Date <- as.Date(estimate$Date,'%m/%d/%y')

merge$Date <- as.Date(merge$Date)
merge <- merge%>%
  left_join(estimate,by=c('Date','iso3_name'))
merge <- merge[!is.na(merge$ICL),]

countries <- c('United_Kingdom','Russia','Norway','Sweden','United_States_of_Americas',
               'Austria','Belgium','Bulgaria','Croatia','Czechia','Cyprus','Denmark','Estonia',
               'Finland','France','Germany','Greece','Hungary','Ireland','Italy','Latvia','Lithuania',
               'Luxembourg','Malta','Netherlands','Poland','Portugal','Romania','Slovakia','Slovenia',
               'Spain','Switzerland','India','Brazil','Japan','Bosnia_and_Herzegovina','Montenegro',
               'Serbia','North_Macedonia')

East_Europe <- merge[merge$Country%in%c('Bulgaria','Croatia','Czechia','Estonia','Greece','Hungary',
                                        'Latvia','Lithuania','Poland','Romania','Slovakia','Slovenia',
                                        'Albania','Bosnia_and_Herzegovina','Montenegro',
                                        'Serbia','North_Macedonia'),]

West_Europe <- merge[merge$Country%in%c('Austria','Belgium','Cyprus','Denmark','Finland','France',
                                        'Germany','Ireland','Italy','Luxembourg','Malta','Netherlands',
                                        'Portugal','Spain','Sweden','United_Kingdom','Switzerland',
                                        'Norway'),]





West_Europe_case <- sqldf("select Date,sum(Population),sum(newcase_7d),sum(newdeath_7d), sum(New_test) from West_Europe group by Date")    
West_Europe1 <- West_Europe[!is.na(West_Europe$Total_tests),]
West_Europe_test <- sqldf("select Date,sum(Total_cases),sum(Population),sum(Total_tests) from West_Europe1 group by Date")

write_xlsx(list(West_Europe_case,West_Europe_test),'/Users/qidiwang1/Desktop/West_Europe.xlsx')

East_Europe_case <- sqldf("select Date,sum(Population),sum(newcase_7d),sum(newdeath_7d),sum(New_test) from East_Europe group by Date")    
East_Europe1 <- East_Europe[!is.na(East_Europe$Total_tests),]
East_Europe_test <- sqldf("select Date,sum(Total_cases),sum(Population),sum(Total_tests) from East_Europe1 group by Date")

write_xlsx(list(East_Europe_case,East_Europe_test),'/Users/qidiwang1/Desktop/East_Europe.xlsx')

West_Europe_case2 <- sqldf("select Date,sum(Population),sum(newcase_7d),sum(ICL) from West_Europe group by Date")
East_Europe_case2 <- sqldf("select Date,sum(Population),sum(newcase_7d),sum(ICL) from East_Europe group by Date")
write_xlsx(list(West_Europe_case2,East_Europe_case2),'/Users/qidiwang1/Desktop/New_cases_Europe.xlsx')



USA <- merge[merge$Country=='United_States_of_America',]
USA$newcase_7d[USA$index_dt==20200925]
6624.4285+3448.285+2492.71+2011.85+1973.28+1961.71+1520.28+1350.29+1264.57+1147.57
23794.97/43422.71
India$newcase_7d[India$index_dt==20200925]
11943-2492
10644-6624
10261-3448
India <- merge[merge$Country=='India',]

USA$newcase_7d[USA$index_dt==20200911]-USA$newcase_7d[USA$index_dt==20200925]
India$newcase_7d[India$index_dt==20200911]-India$newcase_7d[India$index_dt==20200925]
write_xlsx(list(USA,India),'/Users/qidiwang1/Desktop/0925宏观周报/USA&Inida2.xlsx')
