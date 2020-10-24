library(writexl)
us_states <- read_excel('/Users/qidiwang1/Desktop/US/US_covid-19_0730.xlsx',sheet = '新增确诊')
time <- as.Date(us_states$指标名称)

us_data <- data.frame()
for (i in 2:ncol(us_states)){
  aaa <- data.frame(Date = time,
                    index_dt = as.character(time,'%Y%m%d'),
                    美国州名 = names(us_states)[i],
                    New_cases = rowSums(us_states[,i]),
                    Total_cases = rowSums(cumsum(us_states[,i])))
  us_data <- rbind(us_data,aaa)
}

state_name <- read.csv('/Users/qidiwang1/Desktop/US/美国州名字匹配.csv')
us_data$美国州名 <- as.character(us_data$美国州名)
state_name$美国州名 <- as.character(state_name$美国州名)
data <- us_data%>%
  left_join(state_name,by = '美国州名')
data$index_dt <- as.numeric(as.character(data$index_dt))
jdate <- read.csv('/Users/qidiwang1/Desktop/us_jdate.csv')
data <- data%>%
  left_join(jdate, by = 'index_dt')

us_death <- read_excel('/Users/qidiwang1/Desktop/US/US_covid-19_0730.xlsx',sheet = '累计死亡')

time <- as.Date(us_death$指标名称)
index_dt <- as.character(time,'%Y%m%d')

us_death_data <- data.frame()
for (i in 2:ncol(us_death)){
  aaa <- data.frame(index_dt = index_dt,
                    美国州名 = names(us_death)[i],
                    Total_deaths = rowSums(us_death[,i]),
                    New_deaths = c(as.numeric(us_death[1,i]),diff(rowSums(us_death[,i]))))
  us_death_data <- rbind(us_death_data,aaa)
}

us_death_data$美国州名 <- as.character(us_death_data$美国州名)
us_death_data$index_dt <- as.numeric(as.character(us_death_data$index_dt))
data <- data%>%
  left_join(us_death_data, by = c('美国州名','index_dt'))

us_recover <- read_excel('/Users/qidiwang1/Desktop/US/US_covid-19_0730.xlsx',sheet = '累计痊愈')
time <- as.Date(us_death$指标名称)
index_dt <- as.character(time,'%Y%m%d')

us_recover_data <- data.frame()
for (i in 2:ncol(us_recover)){
  aaa <- data.frame(index_dt = index_dt,
                    美国州名 = names(us_recover)[i],
                    Total_recoveries = rowSums(us_recover[,i]),
                    New_recoveries = c(as.numeric(us_recover[1,i]),diff(rowSums(us_recover[,i]))))
  us_recover_data <- rbind(us_recover_data,aaa)
}
us_recover_data$美国州名 <- as.character(us_recover_data$美国州名)
us_recover_data$index_dt <- as.numeric(as.character(us_recover_data$index_dt))
data <- data%>%
  left_join(us_recover_data, by = c('美国州名','index_dt'))



data$美国州名 <- factor(data$美国州名)
merge <- data.frame()
for (a in levels(data$美国州名)){
  country <- data[data$美国州名==a,]
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
  country$confirm_doubling_days <- doubling_day
  merge <- rbind(merge,country)
}

write_xlsx(merge,'/Users/qidiwang1/Desktop/us_data.xlsx')


haha <- data.frame()
for (a in levels(merge$美国州名)){
  country <- merge[merge$美国州名==a,]
  cases_7 <- c()
  recover_7 <- c()
  for (i in c(1:3)){
    cases_7 <- c(cases_7,0)
    recover_7 <- c(recover_7,0)
  }
  for (i in c(4:(nrow(country)-3))){
    cases_7 <- c(cases_7,mean(country$New_cases[(i-3):(i+3)]))
    recover_7 <- c(recover_7, mean(country$New_recoveries[(i-3):(i+3)]))
  }
  for (i in c((nrow(country)-2):nrow(country))){
    if (i == nrow(country)-2) {
      cases_7 <- c(cases_7,mean(country$New_cases[(i-3):nrow(country)]))
      recover_7 <- c(recover_7,mean(country$New_recoveries[(i-3):nrow(country)]))
    } else if (i == nrow(country)-1){
      cases_7 <- c(cases_7,mean(country$New_cases[(i-3):nrow(country)]))
      recover_7 <- c(recover_7,mean(country$New_recoveries[(i-3):nrow(country)]))
    } else if (i == nrow(country)){
      cases_7 <- c(cases_7,mean(country$New_cases[(i-3):nrow(country)]))
      recover_7 <- c(recover_7,mean(country$New_recoveries[(i-3):nrow(country)]))
    }
  }
  country$cases_7 <- cases_7
  country$recover_7 <- recover_7
  haha <- rbind(haha,country)
}

data2 <- haha[haha$recover_7>haha$cases_7,]

View(data2[,c('美国州名','美国州名英文','index_dt','recover_7','cases_7','confirm_doubling_days')])
View(merge)

View(data2[data2$confirm>=150,c('Country','Date','confirm','smooth_confirm_doubling_days','RT','cases_7','recover_7')])
#View(haha)

