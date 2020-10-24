merge<-read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')

merge <- merge[!is.na(merge$Total_tests),]
sum(merge[merge$Sub_region=='Latin America and the Caribbean',]$total)
View(merge[merge$Sub_region=='Latin America and the Caribbean'&merge$index_dt==20201008,])
sum(merge[merge$Sub_region=='Latin America and the Caribbean'&merge$index_dt==20201008,]$Total_cases)
Latin_a <- merge[merge$Sub_region=='Latin America and the Caribbean',]
Latin <- Latin_a%>%
  group_by(Date)%>%
  mutate(Latin_test=sum(Total_tests),
         Latin_case=sum(Total_cases),
         Latin_population = sum(Population))
View(Latin)
library(sqldf)  
Asia <- sqldf("select Date, sum(Total_tests) as Asia_test, sum(Total_cases) as Asia_case, sum(Population) as Asia_population from merge where Region=='Asia' group by Date")
Europe <- sqldf("select Date, sum(Total_tests) as Europe_test, sum(Total_cases) as Europe_case, sum(Population) as Europe_population from merge where Region=='Europe' group by Date")
Africa <- sqldf("select Date, sum(Total_tests) as Africa_test, sum(Total_cases) as Africa_case, sum(Population) as Africa_population from merge where Region=='Africa' group by Date")
Oceania <- sqldf("select Date, sum(Total_tests) as Oceania_test, sum(Total_cases) as Oceania_case, sum(Population) as Oceania_population from merge where Region=='Oceania' group by Date")
#Latin <- sqldf("select Date, sum(Total_tests) as Latin_test, sum(Total_cases) as Latin_case, sum(Population) as Latin_population from merge where Sub_region=='Latin America and the Caribbean' group by Date")
North_america <- sqldf("select Date, sum(Total_tests) as NA_test, sum(Total_cases) as NA_case, sum(Population) as NA_population from merge where Sub_region=='Northern America' group by Date")


data <- Asia%>%
  left_join(Europe,by='Date')%>%
  left_join(Africa,by='Date')%>%
  left_join(Oceania,by='Date')%>%
  left_join(Latin[Latin$Country=='Argentina',c('Date','Latin_test','Latin_case','Latin_population')],by='Date')%>%
  left_join(North_america,by='Date')

write_xlsx(data,'/Users/qidiwang1/Desktop/test_data.xlsx')

Latin_b <- merge[merge$Sub_region=='Latin America and the Caribbean'&merge$Country!='Brazil',]
Latin <- Latin_b%>%
  group_by(Date)%>%
  mutate(Latin_test=sum(Total_tests),
         Latin_case=sum(Total_cases),
         Latin_population = sum(Population))

world <- sqldf("select Date, sum(Total_tests) as world_test, sum(Total_cases) as world_case, sum(Population) as world_population from merge where Region!='Europe' and iso3_name != 'USA' and iso3_name!='IND' and 
               iso3_name != 'RUS' and iso3_name != 'BRA' and Sub_region != 'Latin America and the Caribbean' group by Date")

data2 <- data.frame(Date = merge$Date[merge$iso3_name=='USA'],
                    USA_test=merge$Total_tests[merge$iso3_name=='USA'],
                    USA_case=merge$Total_cases[merge$iso3_name=='USA'],
                    USA_population=merge$Population[merge$iso3_name=='USA'],
                    India_test=merge$Total_tests[merge$iso3_name=='IND'],
                    India_case=merge$Total_cases[merge$iso3_name=='IND'],
                    India_population=merge$Population[merge$iso3_name=='IND'],
                    Brazil_test=merge$Total_tests[merge$iso3_name=='BRA'],
                    Brazil_case=merge$Total_cases[merge$iso3_name=='BRA'],
                    Brazil_population=merge$Population[merge$iso3_name=='BRA'],
                    Latin_test=Latin$Total_tests[Latin$iso3_name=='ARG'],
                    Latin_case=Latin$Total_cases[merge$iso3_name=='ARG'],
                    Latin_population=Latin$Population[merge$iso3_name=='ARG'],
                    Europe_test=Europe$Europe_test,
                    Europe_case=Europe$Europe_case,
                    Europe_population=Europe$Europe_population,
                    Russia_test=merge$Total_tests[merge$iso3_name=='RUS'],
                    Russia_case=merge$Total_cases[merge$iso3_name=='RUS'],
                    Russia_population=merge$Population[merge$iso3_name=='RUS'],
                    World_test=world$world_test,
                    World_case=world$world_case,
                    World_population=world$world_population)
write_xlsx(data2,'/Users/qidiwang1/Desktop/分国家_test_data.xlsx')

ICL <- read.csv('/Users/qidiwang1/Desktop/ICL.csv')
ICL$ICL[is.na(ICL$ICL)] <- 0
ICL <- ICL%>%
  group_by(iso3_name)%>%
  mutate(ICL_total = cumsum(ICL))
merge$Date <- as.Date(merge$Date)
ICL$Date <- as.Date(ICL$Date,'%m/%d/%y')
data <- merge%>%
  left_join(ICL,by=c('iso3_name','Date'))

data$million_cases <- data$ICL_total/data$Population*1000000
write_xlsx(data,'/Users/qidiwang1/Desktop/ICL_data.xlsx')






