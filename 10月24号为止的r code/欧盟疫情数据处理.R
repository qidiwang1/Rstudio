data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/cov.csv')

View(data)

processed_data <- data.frame()

###计算累计死亡和累计confirm并建立新表格
for (i in levels(data[,7])) {

  c <- data.frame()
  confirm <- 0
  death <- 0
  
  b <- data[data$countriesAndTerritories==i,]
  
  for (z in c(1:nrow(b))) {
    confirm <- confirm + b[z,5]
    death <- death + b[z,6]
    c <- rbind(c, cbind('Date'=as.character(b[z,1]),'Country'=as.character(b[z,7]),confirm,death,'Code'=as.character(b[z,8]),'Population'=b[z,10],
                        'New.cases'=b[z,5], 'New.death'=b[z,6]))
  }
  processed_data <- rbind(processed_data,c)
}



processed_data$Code <- factor(processed_data$Code)



data2 <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/Country List.csv')



final <- data.frame()
for (i in levels(processed_data$Code)) {
  b <- processed_data[which(processed_data$Code==i),]
  c <- data2[which(data2$alpha.2==i),][1,]
  d <- data.frame()
  for (j in c(1:nrow(b))) {
    d <- rbind(d,cbind(b[j,],c))
  }
  final <- rbind(final,d)
}


china <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/中国疫情每日更新.csv')

china$confirm <- as.character(china$confirm)
china$death <- as.character(china$death)
china$Population <- as.character(china$Population)
china$New.cases <- as.character(china$New.cases)
china$New.death <- as.character(china$New.death)


processed_data <- rbind(final,china)
View(processed_data)
processed_data$Date <- as.Date(processed_data$Date,'%m/%d/%y')
View(processed_data)
policy <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/policy.csv')

policy$Date <- as.character(policy$Date)

date <- c()
for (i in c(1:nrow(policy))) {
  a <- paste(substr(policy[i,3],5,6),substr(policy[i,3],7,8),substr(policy[i,3],1,4),sep ="/")
  date <- c(date,a)
}

policy$Date <- as.Date(date,'%m/%d/%y')

processed_data$alpha.3 <- as.character(processed_data$alpha.3)

policy$CountryCode <- as.character(policy$CountryCode)

policy$Date <- as.character(policy$Date)

processed_data$Date <- as.character(processed_data$Date)


fill_blank <- data.frame('','','','','','','','','','','','','','','','','','','','','','','','','','','',
                         '','','','','','','','','','','','','')
for (i in c(1:ncol(fill_blank))) {
  names(fill_blank)[i] <- names(policy)[i]
}


for (i in c(1,4:40)) {
  policy[,i] <- as.character(policy[,i])
}

recover <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/recover.csv')



doc <- data.frame()
for (i in c(1:nrow(recover))) {
  first_merge <- data.frame()
  for (j in 1:(ncol(recover)-1)){
    time <- as.Date('2020-01-21')+j
    b <- cbind(as.character(time),as.character(recover[i,1]), recover[i,(j+1)])
    first_merge <- rbind(first_merge,b)
  }
  doc <- rbind(doc,first_merge)
}
View(doc)

recover <- doc
View(recover)
recover$V1 <- as.character(recover$V1)
recover$V3 <- as.character(recover$V3)
recover$V2 <- as.character(recover$V2)
processed_data$Country <- as.character(processed_data$Country)

third_fill_blank <- data.frame('','','')
for (i in c(1:ncol(third_fill_blank))) {
  names(third_fill_blank)[i] <- names(recover)[i]
}





test <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/test.csv')

test$Date <- as.character(test$Date)

test$Date <- as.Date(test$Date,'%d-%b-%y')

test$Date <- as.character(test$Date)


second_fill_blank <- data.frame('','','','')
for (i in c(1:ncol(second_fill_blank))) {
  names(second_fill_blank)[i] <- names(test)[i]
}

test$Code <- as.character(test$Code)
#merge$alpha.3 <- as.character(merge$alpha.3)
test$Total.tests <- as.character(test$Total.tests)


merge <- data.frame()
for (i in c(1:nrow(processed_data))) {
  a <- policy[policy$CountryCode==processed_data$alpha.3[i],]
  b <- a[a$Date==processed_data$Date[i],]
  
  
  if (nrow(b)==1){
    aa <- cbind(processed_data[i,],b)
  } else {aa <- cbind(processed_data[i,],fill_blank)}

  c <- test[test$Code==processed_data$alpha.3[i],]
  d <- c[c$Date==processed_data$Date[i],]

  if (nrow(d)==1){
    bb <- cbind(aa,d)
  } else {bb <- cbind(aa,second_fill_blank)}

  e <- recover[recover$V2==processed_data$Country[i],]
  f <- e[e$V1==processed_data$Date[i],]
  if (nrow(f)==1){
    cc <- cbind(bb,f)
  } else {cc <- cbind(bb,third_fill_blank)}
  
  merge <- rbind(merge,cc)
}


#write.csv(merge,'/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
nrow(merge)
View(merge)


JDate <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/JDate.csv')
JDate <- na.omit(JDate)
View(JDate)
JDate$Date <- as.Date(JDate$Date,'%m/%d/%y')


JDate$Date <- as.character(JDate$Date)
merge$Date <- as.character(merge$Date)


jdate <- data.frame()
for (i in c(1:nrow(merge))) {
  a <- JDate[which(JDate$Date==merge$Date[i]),]
  jdate <- rbind(jdate,a)
}


merge$jdate <- jdate$Jdate


merge <- cbind(JDATE=merge[,ncol(merge)],merge[,1:(ncol(merge)-1)])
write.csv(merge,'/Users/qidiwang1/Desktop/疫情数据库/merge.csv')

