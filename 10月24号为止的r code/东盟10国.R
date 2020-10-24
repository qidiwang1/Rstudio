data <- read_excel('/Users/qidiwang1/Desktop/PET_DATA_20201019.xlsx')
data$Date <- as.Date(data$Date)
ICL <- read.csv('/Users/qidiwang1/Desktop/ICL.csv')
data$gdp_relative[!is.na(data$gdp_relative)&data$gdp_relative>0.9886] <- 0.9886
ICL$Date <- as.Date(ICL$Date,'%m/%d/%y')
data <- data%>%
  left_join(ICL,by=c('iso3_name','Date'))
View(data)
library(zoo)
data <- data%>%
  group_by(Country)%>%
  mutate(newcase_7d=rollmean(New_case,7,fill=NA,align='right'))
data <- data[data$index_dt>=20200215&data$index_dt<=20201016,]
Vietnam <- data[data$Country=='Vietnam',]
Philippines <- data[data$Country=='Philippines',]
Indonesia <- data[data$Country=='Indonesia',]
Thailand <- data[data$Country=='Thailand',]
Malaysia <- data[data$Country=='Malaysia',]
Singapore <- data[data$Country=='Singapore',]
Brunei_Darussalam <- data[data$Country=='Brunei_Darussalam',]
Cambodia <- data[data$Country=='Cambodia',]
Laos <- data[data$Country=='Laos',]
Myanmar <- data[data$Country=='Myanmar',]

write_xlsx(list(Vietnam[,c('Date','Country','newcase_7d','ICL','gdp_relative','Daily_Economics_Policy_Score','Population')],
                Philippines[,c('Date','Country','newcase_7d','ICL','gdp_relative','Daily_Economics_Policy_Score','Population')],
                Indonesia[,c('Date','Country','newcase_7d','ICL','gdp_relative','Daily_Economics_Policy_Score','Population')],
                Thailand[,c('Date','Country','newcase_7d','ICL','gdp_relative','Daily_Economics_Policy_Score','Population')],
                Malaysia[,c('Date','Country','newcase_7d','ICL','gdp_relative','Daily_Economics_Policy_Score','Population')],
                Singapore[,c('Date','Country','ICL','newcase_7d','gdp_relative','Daily_Economics_Policy_Score','Population')],
                Brunei_Darussalam[,c('Date','Country','newcase_7d','ICL','gdp_relative','Daily_Economics_Policy_Score','Population')],
                Cambodia[,c('Date','Country','newcase_7d','ICL','gdp_relative','Daily_Economics_Policy_Score','Population')],
                Laos[,c('Date','Country','newcase_7d','ICL','gdp_relative','Daily_Economics_Policy_Score','Population')],
                Myanmar[,c('Date','Country','newcase_7d','ICL','gdp_relative','Daily_Economics_Policy_Score','Population')]),
           '/Users/qidiwang1/Desktop/东盟10国.xlsx')

