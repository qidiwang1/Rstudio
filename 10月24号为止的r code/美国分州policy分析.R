us_confirm <- read.csv('/Users/qidiwang1/Desktop/us_confirm.csv')

seq(as.Date('2020-01-22'),as.Date('2020-09-26'),'days')
us_data <- data.frame()
for (a in levels(us_confirm$Province_State)){
  state <- us_confirm[us_confirm$Province_State==a,]
  aaa <- data.frame(State = as.character(a),
                    Date = seq(as.Date('2020-01-22'),as.Date('2020-09-26'),'days'),
                    Total_cases = colSums(state[,3:ncol(state)]))
  us_data <- rbind(us_data,aaa)
}
us_data <- us_data%>%
  group_by(State)%>%
  mutate(New_case = c(0,diff(Total_cases)))
us_data <- us_data%>%
  group_by(State)%>%
  mutate(newcase_7d = rollmean(New_case,7,fill=NA,align='right'))

View(us_data[us_data$Date=='2020-09-26',c('State','Total_cases','newcase_7d')])
for (a in levels(factor(us_data$State))){
  state <- us_data[us_data$State==a,]
  p <- ggplot()+
    geom_line(aes(x=))
}


policy <- read.csv('/Users/qidiwang1/Desktop/policy.csv')
us <- policy[policy$CountryCode=='USA',]
us <- us[us$RegionName!='',]
write.csv(us,'/Users/qidiwang1/Desktop/us_policy.csv')
