confirm <- read.csv('/Users/qidiwang1/Desktop/us confirm.csv')

time <- seq(as.Date("2020-01-22"), as.Date("2020-01-22")+ncol(confirm)-12, "days")
time <- as.numeric(as.character(as.Date(time),'%Y%m%d'))

confirm2 <- data.frame()
for (a in levels(confirm$Province_State)){
  country <- confirm[confirm$Province_State==a,]
  aaa <- data.frame(index_dt = time,
                    area_name = as.character(a),
                    total_cases = colSums(country[,c(12:ncol(country))]))
  confirm2 <- rbind(confirm2,aaa)
}
rownames(confirm2) <- NULL



recover <- read.csv('/Users/qidiwang1/Desktop/recover.csv')
confirm <- read.csv('/Users/qidiwang1/Desktop/us_states.csv')
recover$area_name <- as.character(recover$area_name)
confirm$area_name <- as.character(confirm$area_name)
data <- recover%>%
  left_join(confirm, by = c('index_dt','area_name'))
data <- data[,c(1:4)]
data$area_name <- factor(data$area_name)
merge <- data.frame()
for (a in levels(data$area_name)){
  country <- data[data$area_name==a,]
  new_recover <- c()
  for (i in c(1:nrow(country))){
    if (i ==1){
      new_recover <- c(new_recover,country$total_recover[i])
    } else {
      new_recover <- c(new_recover, (country$total_recover[i]-country$total_recover[i-1]))
    }
  }
  country$new_recover <- new_recover
  merge <- rbind(merge, country)
}

data <- data.frame()
for (a in levels(merge$area_name)){
  country <- merge[merge$area_name==a,]
  confirm <- c()
  total_cases <- 0
  for (i in c(1:nrow(country))){
    total_cases <- total_cases + country$new_case[i]
    confirm <- c(confirm, total_cases)
  }
  country$total_cases <- confirm
  data <- rbind(data, country)
}

jdate <- read.csv('/Users/qidiwang1/Desktop/us_jdate.csv')

data <- data%>%
  left_join(jdate, by = 'index_dt')

merge <- data.frame()
for (a in levels(data$area_name)){
  country <- data[data$area_name==a,]
  Jdate <- 0
  determine_Jdate <- 0
  doubling_day <- c()
  for (i in c(1:nrow(country))){
    cutoff <- country$total_cases[i]/2
    subgroup <- country[country$total_cases<=cutoff,]
    Jdate <- tail(subgroup$Jdate,1)
    
    min_confirm <- tail(subgroup$total_cases,1)
    max_confirm <- country[country$Jdate==(Jdate+1),]$total_cases
    determine_Jdate <- Jdate + (cutoff-min_confirm)/(max_confirm-min_confirm)
    if (nrow(subgroup)==0){
      days <- 0 
      doubling_day <- c(doubling_day,days)
    } else if (country$total_cases[i]==0) {
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

data <- data.frame()
for (a in levels(merge$area_name)){
  country <- merge[merge$area_name==a,]
  smooth_confirm <- c()
  for (i in c(1:6)){
    smooth_confirm <- c(smooth_confirm, as.double(NA))
  }
  for (i in (7:nrow(country))){
    smooth_confirm <- c(smooth_confirm, mean(country$confirm_doubling_days[(i-6):i]))
  }
  country$smooth_confirm_doubling_days <- smooth_confirm
  country$smooth_confirm_doubling_days <- ifelse(is.na(country$smooth_confirm_doubling_days)==TRUE,
                                                 country$confirm_doubling_days,country$smooth_confirm_doubling_days)
  data <- rbind(data,country)
}

library(anchors)
data <- replace.value(data,'smooth_confirm_doubling_days',0,as.double(NA))
write_xlsx(data,'/Users/qidiwang1/Desktop/us_dd.xlsx')




haha <- data.frame()
for (a in levels(data$area_name)){
  country <- data[data$area_name==a,]
  cases_7 <- c()
  recover_7 <- c()
  for (i in c(1:3)){
    cases_7 <- c(cases_7,0)
    recover_7 <- c(recover_7,0)
  }
  for (i in c(4:(nrow(country)-3))){
    cases_7 <- c(cases_7,mean(country$new_case[(i-3):(i+3)]))
    recover_7 <- c(recover_7, mean(country$new_recover[(i-3):(i+3)]))
  }
  for (i in c((nrow(country)-2):nrow(country))){
    if (i == nrow(country)-2) {
      cases_7 <- c(cases_7,mean(country$new_case[(i-3):nrow(country)]))
      recover_7 <- c(recover_7,mean(country$new_recover[(i-3):nrow(country)]))
    } else if (i == nrow(country)-1){
      cases_7 <- c(cases_7,mean(country$new_case[(i-3):nrow(country)]))
      recover_7 <- c(recover_7,mean(country$new_recover[(i-3):nrow(country)]))
    } else if (i == nrow(country)){
      cases_7 <- c(cases_7,mean(country$new_case[(i-3):nrow(country)]))
      recover_7 <- c(recover_7,mean(country$new_recover[(i-3):nrow(country)]))
    }
  }
  country$cases_7 <- cases_7
  country$recover_7 <- recover_7
  haha <- rbind(haha,country)
}

data2 <- haha[haha$recover_7>haha$cases_7,]
data2$smooth_confirm_doubling_days
View(data2[,c('area_name','index_dt','recover_7','cases_7','smooth_confirm_doubling_days')])


View(data2[data2$confirm>=150,c('Country','Date','confirm','smooth_confirm_doubling_days','RT','cases_7','recover_7')])
#View(haha)
