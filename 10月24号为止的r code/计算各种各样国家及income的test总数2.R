data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
merge <- data.frame()
for (a in levels(data$Country)){
  country <- data[data$Country==a,]
  if (sum(is.na(country$Total.test))!=nrow(country))
    merge <- rbind(merge,country)
}
data <- merge
data$Country <- factor(data$Country)
test_data <- data.frame()
for (a in levels(data$Country)){
  country <- data[data$Country==a,]
  for (i in c(1:nrow(country))){
    if (is.na(country$Total.tests[i])==TRUE)
      if (i == 1){
        country$Total.tests[i]<-0
      } else {
        country$Total.tests[i] <- country$Total.tests[i-1]
      }
  }
  test_data <- rbind(test_data,country)
}

data <- test_data
data$Population <- as.numeric(as.character(data$Population))
data$Total.tests <- as.numeric(as.character(data$Total.tests))


data$Date <- factor(data$Date)

plot_data<- data.frame()
for (a in levels(data$Date)){
  certain_date <- data[data$Date==a,]
  high <- sum(certain_date[certain_date$Income=="High income",]$Total.tests)/sum(certain_date[certain_date$Income=="High income",]$Population)*1000

  low <- sum(certain_date[certain_date$Income=="Low income",]$Total.tests)/sum(certain_date[certain_date$Income=="Low income",]$Population)*1000

  low_middle <- sum(certain_date[certain_date$Income=="Lower-Middle income",]$Total.tests)/sum(certain_date[certain_date$Income=="Lower-Middle income",]$Population)*1000

  upper_middle <- sum(certain_date[certain_date$Income=="Upper-Middle income",]$Total.tests)/sum(certain_date[certain_date$Income=="Upper-Middle income",]$Population)*1000

  plot_data <- rbind(plot_data, data.frame(Date = as.character(a),High_Income = high, Low_Income = low, 
                                           Lower_Middle_Income = low_middle, Upper_Middle_Income = upper_middle))
  
}

write.csv(plot_data,'/Users/qidiwang1/Desktop/TEST.csv')












