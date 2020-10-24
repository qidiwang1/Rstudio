brazil <- read.csv('/Users/qidiwang1/Desktop/Brazil/brazil_covid.csv')
brazil <- brazil[brazil$estado!=''&brazil$municipio==''&brazil$populacaoTCU2019!='',]
brazil_data <- data.frame(Region = brazil$regiao,
                          ISO = brazil$estado,
                          Code = brazil$coduf,
                          Date = brazil$data,
                          Population = brazil$populacaoTCU2019,
                          Total_cases = brazil$casosAcumulado,
                          Total_deaths = brazil$obitosAcumulado,
                          New_cases = brazil$casosNovos,
                          New_deaths = brazil$obitosNovos)

brazil_data$index_dt <- as.numeric(as.character(as.Date(brazil_data$Date,'%m/%d/%y'),'%Y%m%d'))

jdate <- read.csv('/Users/qidiwang1/Desktop/us_jdate.csv')
brazil<- brazil_data%>%
  left_join(jdate, by = 'index_dt')

brazil$ISO <- factor(brazil$ISO)
merge <- data.frame()
for (a in levels(brazil$ISO)){
  country <- brazil[brazil$ISO==a,]
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

google <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/google.csv')
brazil_mobility <- google[google$country_region=='Brazil'&google$sub_region_1!='',]
brazil_mobility$index_1 <- rowMeans(brazil_mobility[,c('grocery_and_pharmacy_percent_change_from_baseline','retail_and_recreation_percent_change_from_baseline')],na.rm = TRUE)/100+1
brazil_mobility$index_2 <- (100+brazil_mobility$workplaces_percent_change_from_baseline)/100
brazil_mobility$google_index <- rowMeans(brazil_mobility[,c('index_1','index_2')],na.rm = TRUE)
brazil_mobility$sub_region_1 <- factor(brazil_mobility$sub_region_1)

google_mobility <- data.frame()
for (a in levels(brazil_mobility$sub_region_1)){
  state <- brazil_mobility[brazil_mobility$sub_region_1==a,]
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
brazil_mobility <- data.frame(index_dt = as.numeric(as.character(as.Date(google_mobility$date,'%m/%d/%y'),'%Y%m%d')),
                              ISO = substr(as.character(google_mobility$iso_3166_2_code),4,5),
                              State = google_mobility$sub_region_1,
                              Mobility = google_mobility$google_avg,
                              Economic_Activity = 0.3886*google_mobility$google_avg+0.61)
brazil_mobility$ISO <- as.character(brazil_mobility$ISO)
merge$ISO <- as.character(brazil$ISO)
brazil_data <- merge%>%
  left_join(brazil_mobility, by = c('index_dt','ISO'))
brazil_data <- brazil_data[!(is.na(brazil_data$Mobility)),]

write_xlsx(brazil_data,'/Users/qidiwang1/Desktop/Brazil/brazil_data.xlsx')



