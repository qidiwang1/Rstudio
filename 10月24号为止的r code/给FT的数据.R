data <- read_excel('/Users/qidiwang1/Desktop/PET_DATA_20201019.xlsx')
data <- data[data$index_dt<=20201016,]


data <- data%>%
  group_by(Country)%>%
  mutate(days_to_max_policy_score=head(JDATE[Daily_Economics_Policy_Score==max(Daily_Economics_Policy_Score)],1)-head(JDATE[Total_cases>0],1),
         max_policy_score = max(Daily_Economics_Policy_Score))




p1 <- data[data$iso3_name%in%c('DEU','FRA','NOR','SWE','SVN','AUT'),c('Country','iso3_name','Date','smooth_confirm_doubling_days','gdp_relative','Death_per_Million','Stage')]

p2 <- data[data$Country%in%c('Brazil','Argentina','South_Africa','Nigeria','Rwanda','India'),c('Country','iso3_name','Date','smooth_confirm_doubling_days','gdp_relative','Death_per_Million','Stage')]

p3 <- data[data$Country%in%c('South_Korea','New_Zealand','United_Kingdom','United_States_of_America'),c('Country','iso3_name','Date','smooth_confirm_doubling_days','gdp_relative','Death_per_Million','Stage')]

p4 <- data[data$iso3_name%in%c('CHN','KOR','JPN','USA','ITA','GBR','NZL','AUS','ESP'),c('Country','iso3_name','Date','smooth_confirm_doubling_days','gdp_relative','Death_per_Million','Stage')]

p5 <- data[,c('Country','iso3_name','Date','days_to_max_policy_score','max_policy_score','Cases_per_Million','average_loss')]

p6 <- data[data$iso3_name%in%c('BRA','ARG','IND','PHI','ZAF','RWA'),c('Country','iso3_name','Date','smooth_confirm_doubling_days','gdp_relative','Death_per_Million','Stage')]

p7 <- data[data$iso3_name%in%c('CHN','THA','NZL','DEU','URY'),c('Country','iso3_name','Date','smooth_confirm_doubling_days','gdp_relative','Death_per_Million','Stage')]
write_xlsx(list(p1,p2,p3,p4,p5,p6,p7),'/Users/qidiwang1/Desktop/FT_Luohan_data.xlsx')
