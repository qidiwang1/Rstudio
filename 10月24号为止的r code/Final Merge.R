policy <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/policy.csv')

policy$Date <- as.character(policy$Date)

date <- c()
for (i in c(1:nrow(policy))) {
  a <- paste(substr(policy[i,3],5,6),substr(policy[i,3],7,8),substr(policy[i,3],1,4),sep ="/")
  date <- c(date,a)
}

policy$Date <- date

processed_data <- read.csv('/Users/qidiwang1/Desktop/a.csv')
View(processed_data)
View(policy)

processed_data$Date <- as.character(processed_data$Date)
processed_data$alpha.3 <- as.character(processed_data$alpha.3)
policy$Date <- as.character(policy$Date)
policy$CountryCode <- as.character(policy$CountryCode)




fill_blank <- data.frame('','','','','','','','','','','','','','','','','','','','','','','','','','','',
                  '','','','','','','','','','','','','')
for (i in c(1:ncol(fill_blank))) {
  names(fill_blank)[i] <- names(policy)[i]
}


for (i in c(4,5,7,8,10,11,13,14,16,17,19,20,22,24,26,28,30,32,34,38,39)) {
  policy[,i] <- as.character(policy[,i])
}

for (i in c(4:40)) {
  policy[,i] <- as.character(policy[,i])
}
merge <- data.frame()
for (i in c(1:nrow(processed_data))) {
  a <- policy[policy[,2]==processed_data$alpha.3[i],]
  b <- a[a[,3]==processed_data[i,2],]
  if (nrow(b)==1){
    merge <- rbind(merge,cbind(processed_data[i,],b))
  } else {merge <- rbind(merge, cbind(processed_data[i,],fill_blank))}
}
View(merge)

write.csv(merge,'/Users/qidiwang1/Desktop/疫情数据库/merge.csv')


merge <- read.csv('/Users/qidiwang1/Desktop/merge.csv')
JDate <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/JDate.csv')
JDate <- na.omit(JDate)

JDate$Date <- as.character(JDate$Date)
merge$Date <- as.character(merge$Date)
JDate[JDate$Date==merge$Date[1],]
JDate$Date

jdate <- data.frame()
for (i in c(1:nrow(merge))) {
  a <- JDate[which(JDate$Date==merge$Date[i]),]
  jdate <- rbind(jdate,a)
}
jdate
write.csv(jdate, '/Users/qidiwang1/Desktop/jdate.csv')  









