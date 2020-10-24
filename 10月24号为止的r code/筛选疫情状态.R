merge <- read.csv('/Users/qidiwang1/Desktop/merge.csv')
View(merge)
merge[(which(is.na(merge$Population))),]

for (i in c(1:nrow(merge))) {
  if (is.na(merge[i,8])==TRUE)
    merge$Population[i] <- 0
  
}

merge$confirm <- as.numeric(as.character(merge$confirm))
merge$Population <- as.numeric(as.character(merge$Population))
merge <- merge[which(merge$Population>5000000),]
merge$Country <- factor(merge$Country)

which(merge[merge$Country=='Afghanistan',]$confirm>0)[1]


merge$Jdate[4]-merge$Jdate[2]


linear_data <- data.frame()
for (i in levels(merge[,4])) {
  country <- merge[merge[,4]==i,]
  

  
  if (nrow(country)>=19 & max(country$confirm) > 100)
    
    
    linear_data <- rbind(linear_data, country[,c(2,3,4,5,6,9,10,11,12)])
}
linear_data$Country <- factor(linear_data$Country)
levels(linear_data$Country)

View(linear_data)

for (i in levels(linear_data$Country)){
  country <- linear_data[linear_data$Country==i,]
  if (nrow(country)<19)
    print(nrow(country))
}



for (i in c(4:9)){
  linear_data[,i] <- as.numeric(as.character(linear_data[,i]))
}


coefficient_data <- data.frame()

for (i in levels(linear_data[,3])) {
  
  my_country <- linear_data[linear_data[,3]==i,]
  jdate <- 100-my_country$Jdate[which(my_country$death>0)[1]]
  
  day <- data.frame('Day'=c(1:7))
  
  

  a <- my_country[c((nrow(my_country)-6):nrow(my_country)),]
  a <- cbind(a, day)
  linear <- lm(New.death~Day, data = a)
  coefficient_data <- rbind(coefficient_data, cbind(as.character(a$Country[7]),a$confirm[7],a$death[7],a$New.cases[7],a$New.death[7],a$New_cases_million[7],a$New_death_million[7],linear$coefficients[2],jdate))
}


View(coefficient_data)

coefficient_data$V2 <- as.numeric(as.character(coefficient_data$V2))
coefficient_data$V3 <- as.numeric(as.character(coefficient_data$V3))
coefficient_data$jdate <- as.numeric(as.character(coefficient_data$jdate))
sum(coefficient_data$V2<3)
sum(coefficient_data$V2>100)
summary(coefficient_data$jdate)
sum(coefficient_data$jdate<32)
sum(coefficient_data$jdate<68.5)
coefficient_data[1,]
post_pandemic <- data.frame(Status = 'post pandemic')
post_peak <- data.frame(Status = 'post peak')
pre_peak <- data.frame(Status = 'pre peak')
growth <- data.frame(Status = 'growth')
initial <- data.frame(Status = 'initial')

View(coefficient_data)

write.csv(coefficient_data,'/Users/qidiwang1/Desktop/death.csv')

final_data <- data.frame()
for (i in c(1:nrow(coefficient_data))) {
  

 
  if (coefficient_data$V3[i]<0 & coefficient_data$jdate[i]<51 & coefficient_data$jdate[i]>20) {
    final_data <- rbind(final_data,cbind(coefficient_data[i,],post_peak))
  } else if (coefficient_data$V3[i]>0 & coefficient_data$jdate[i]<51 & coefficient_data$jdate[i]>20) {
    final_data <- rbind(final_data,cbind(coefficient_data[i,],pre_peak))
  } else if (coefficient_data$V3[i]>0 coefficient_data$jdate[i]>=15 & coefficient_data$jdate[i]<20) {
    final_data <- rbind(final_data,cbind(coefficient_data[i,],growth))
  } else if (coefficient_data$jdate[i]<=14) {
    final_data <- rbind(final_data,cbind(coefficient_data[i,],initial))
  } else {final_data <- rbind(final_data,cbind(coefficient_data[i,],post_pandemic))}
}
nrow(final_data)
write.csv(final_data,'/Users/qidiwang1/Desktop/death.csv')





summary(coefficient_data$V2)
  
  linear_data$New.death
  coefficient_data <- data.frame()
  day_time <- 0
  for (j in c(7:13)){
      b <- a[c((j-6):j),]
      b <- cbind(b,day)
      linear <- lm(New_death_million~Day, data = b)
      day_time <- day_time+1
      coefficient_data <- rbind(coefficient_data,cbind(as.character(a[1,2]),linear$coefficients[2],day_time))
    }
    
    
    coefficient_data$V2 <- as.numeric(as.character(coefficient_data$V2))
    coefficient_data$day_time <- as.numeric(as.character(coefficient_data$day_time))
    
    linear2 <- lm(V2 ~ day_time, data = coefficient_data)
    if (is.na(linear2$coefficients[2])==FALSE) {
      c <- confint(linear2, 'day_time', level = 0.9)
      d <- confint(linear2, 'day_time', level = 0.5)
      e <- linear2$coefficients[2]
      f <- summary(linear2)$coefficients[4]
    } else{
      c <- 0 
      d <- c(0,0)
      e <- c(0,0)
      f <- 0}
    
    
    if (is.na(summary(linear2)$coefficient[8])==FALSE) {
      if (summary(linear2)$coefficient[8]<0.05 & e > 0){
        
        country_lower1 <- cbind(c[1],as.character(b[1,2]),'Increasing',as.character(b$Date[nrow(b)]),f)
        country_lower2 <- cbind(d[1],as.character(b[1,2]),'Increasing',as.character(b$Date[nrow(b)]),f)
        country_middle <- cbind(e, as.character(b[1,2]),'Increasing',as.character(b$Date[nrow(b)]),f)
        country_upper1 <- cbind(c[2],as.character(b[1,2]),'Increasing',as.character(b$Date[nrow(b)]),f)
        country_upper2 <- cbind(d[2],as.character(b[1,2]),'Increasing',as.character(b$Date[nrow(b)]),f)
      } else if (summary(linear2)$coefficient[8]<0.05 & e < 0){
        
        country_lower1 <- cbind(c[1],as.character(b[1,2]),'Decreasing',as.character(b$Date[nrow(b)]),f)
        country_lower2 <- cbind(d[1],as.character(b[1,2]),'Decreasing',as.character(b$Date[nrow(b)]),f)
        country_middle <- cbind(e, as.character(b[1,2]),'Descreaing',as.character(b$Date[nrow(b)]),f)
        country_upper1 <- cbind(c[2],as.character(b[1,2]),'Decreasing',as.character(b$Date[nrow(b)]),f)
        country_upper2 <- cbind(d[2],as.character(b[1,2]),'Decreasing',as.character(b$Date[nrow(b)]),f)
      } else if (summary(linear2)$coefficient[8]>=0.05 & summary(linear2)$coefficient[8]<=0.1 & e >0) {
        country_lower1 <- cbind(c[1],as.character(b[1,2]),'Slightly Increasing',as.character(b$Date[nrow(b)]),f)
        country_lower2 <- cbind(d[1],as.character(b[1,2]),'Slightly Increasing',as.character(b$Date[nrow(b)]),f)
        country_middle <- cbind(e, as.character(b[1,2]),'Slightly Increasing',as.character(b$Date[nrow(b)]),f)
        country_upper1 <- cbind(c[2],as.character(b[1,2]),'Slightly Increasing',as.character(b$Date[nrow(b)]),f)
        country_upper2 <- cbind(d[2],as.character(b[1,2]),'Slightly Increasing',as.character(b$Date[nrow(b)]),f)
      } else if (summary(linear2)$coefficient[8]>=0.05 & summary(linear2)$coefficient[8]<=0.1 & e <0) {
        country_lower1 <- cbind(c[1],as.character(b[1,2]),'Slightly Decreasing',as.character(b$Date[nrow(b)]),f)
        country_lower2 <- cbind(d[1],as.character(b[1,2]),'Slightly Decreasing',as.character(b$Date[nrow(b)]),f)
        country_middle <- cbind(e, as.character(b[1,2]),'Slightly Decreasing',as.character(b$Date[nrow(b)]),f)
        country_upper1 <- cbind(c[2],as.character(b[1,2]),'Slightly Decreasing',as.character(b$Date[nrow(b)]),f)
        country_upper2 <- cbind(d[2],as.character(b[1,2]),'Slightly Decreasing',as.character(b$Date[nrow(b)]),f)
      } else if (summary(linear2)$coefficient[8]>0.1) {
        country_lower1 <- cbind(c[1],as.character(b[1,2]),'Unsure',as.character(b$Date[nrow(b)]),f)
        country_lower2 <- cbind(d[1],as.character(b[1,2]),'Unsure',as.character(b$Date[nrow(b)]),f)
        country_middle <- cbind(e, as.character(b[1,2]),'Unsure',as.character(b$Date[nrow(b)]),f)
        country_upper1 <- cbind(c[2],as.character(b[1,2]),'Unsure',as.character(b$Date[nrow(b)]),f)
        country_upper2 <- cbind(d[2],as.character(b[1,2]),'Unsure',as.character(b$Date[nrow(b)]),f)
      } 
    } else {
      country_lower1 <- cbind(c[1],as.character(b[1,2]),'Unchanged',as.character(b$Date[nrow(b)]),f)
      country_lower2 <- cbind(d[1],as.character(b[1,2]),'Unchanged',as.character(b$Date[nrow(b)]),f)
      country_middle <- cbind(e, as.character(b[1,2]),'Unchanged',as.character(b$Date[nrow(b)]),f)
      country_upper1 <- cbind(c[2],as.character(b[1,2]),'Unchanged',as.character(b$Date[nrow(b)]),f)
      country_upper2 <- cbind(d[2],as.character(b[1,2]),'Unchanged',as.character(b$Date[nrow(b)]),f)
    }
    #country_lower1 <- cbind(rbind(c[1]),rbind(as.character(a[1,2])))
    #country_lower2 <- cbind(rbind(d[1]),rbind(as.character(a[1,2])))
    #country_upper1 <- cbind(rbind(c[2]),rbind(as.character(a[1,2])))
    #country_upper2 <- cbind(rbind(d[2]),rbind(as.character(a[1,2])))
    #country_middle <- cbind(e, as.character(a[1,2]))
    #colnames(country_data) <- a[1,2]
    
    
    
    final_upper1 <- rbind(final_upper1, country_upper1)
    final_upper2 <- rbind(final_upper2, country_upper2)
    final_lower1 <- rbind(final_lower1, country_lower1)
    final_lower2 <- rbind(final_lower2, country_lower2)
    final_middle <- rbind(final_middle, country_middle)
  }
  upper1 <- rbind(upper1,final_upper1)
  upper2 <- rbind(upper2,final_upper2)
  lower1 <- rbind(lower1,final_lower1)
  lower2 <- rbind(lower2,final_lower2)
  middle <- rbind(middle,final_middle)
}

