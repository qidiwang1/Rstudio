filenames <- list.files('/Users/qidiwang1/Desktop/Death Map', pattern = '*.csv$', full.names = TRUE)


my_data <- lapply(filenames, read.csv, skip = 2)

data <- my_data


for (a in c(1:length(data))){
  if (nrow(data[[a]])>0) 
    for (i in c(1:nrow(data[[a]]))){
      for (j in c(1:ncol(data[[a]]))){
        if (data[[a]][i,j] == '<1') data[[a]][i,j] <- NA
      }
      
    }
}

library(zoo)

vaccine <- data.frame()
for (a in c(1:length(data))){
  if (nrow(data[[a]])==0){
    print('NO')
  } else{
    bbb <- data.frame(Date = data[[a]]$Day,Country = substr(names(data[[a]])[2],17,nchar(names(data[[a]])[2])-1),
               vaccine = data[[a]][2])
    names(bbb)[3] <- 'vaccine'
    vaccine <- rbind(vaccine,
                     bbb)
  }
}
write.csv(vaccine,'/Users/qidiwang1/Desktop/vaccine.csv')



contact <- data.frame()
for (a in c(1:length(data))){
  if (nrow(data[[a]])==0){
    print('NO')
  } else{
    bbb <- data.frame(Date = data[[a]]$Day,Country = substr(names(data[[a]])[2],19,nchar(names(data[[a]])[2])-1),
                      contact = data[[a]][2])
    names(bbb)[3] <- 'contact'
    contact <- rbind(contact,
                     bbb)
  }
}
write.csv(contact,'/Users/qidiwang1/Desktop/contact.csv')



View(contact)

colMeans(data[[a]][2]
contact <- data.frame()
for (a in c(1:length(data))){
  if (nrow(data[[a]])==0){
    print('NO')
  } else{
  aaa <- rollmean(data[[a]][2], k = 7, align = 'right',fill = NA)
  num = as.numeric(tail(aaa,1)/max(aaa,na.rm = TRUE))
  contact <- rbind(contact,
                   data.frame(Country = substr(names(data[[a]])[2],19,nchar(names(data[[a]])[2])-1),
                              contact = num))
  }
}

data2$Country <- as.character(data2$Country)
contact$Country <- as.character(contact$Country)
data3 <- data2%>%
  left_join(contact,by='Country')

ggplot()+
  geom_point(aes(x=data3$confirm_ratio,y=data3$contact))+
  geom_text(aes(x=data3$confirm_ratio,y=data3$contact,label=data3$Country))








for (a in c(1:length(data))){
  data[[a]][,2] <- as.character(data[[a]][,2])
  data[[a]][,3] <- as.character(data[[a]][,3])
  data[[a]][,4] <- as.character(data[[a]][,4])
  data[[a]][,5] <- as.character(data[[a]][,5]) 
  data[[a]][,6] <- as.character(data[[a]][,6])
}
for (a in c(1:length(data))){
  data[[a]][,2] <- as.numeric(data[[a]][,2])
  data[[a]][,3] <- as.numeric(data[[a]][,3])
  data[[a]][,4] <- as.numeric(data[[a]][,4])
  data[[a]][,5] <- as.numeric(data[[a]][,5]) 
  data[[a]][,6] <- as.numeric(data[[a]][,6])
}



R <- data.frame()
dataset <- data.frame()
for (a in c(3,5)) {
  data[[a]][,2] <- ifelse(is.na(data[[a]][,2]),0,data[[a]][,2])
  data[[a]][,3] <- ifelse(is.na(data[[a]][,3]),0,data[[a]][,3])
  data[[a]][,4] <- ifelse(is.na(data[[a]][,4]),0,data[[a]][,4])
  data[[a]][,5] <- ifelse(is.na(data[[a]][,5]),0,data[[a]][,5])
  data[[a]][,6] <- ifelse(is.na(data[[a]][,6]),0,data[[a]][,6])
  
  a1 <- sum(data[[a]][c(1:91),2])/nrow(data1)
  a2 <- sum(data[[a]][c(1:91),3])/nrow(data1)
  a3 <- sum(data[[a]][c(1:91),4])/nrow(data1)
  a4 <- sum(data[[a]][c(1:91),5])/nrow(data1)
  a5 <- sum(data[[a]][c(1:91),6])/nrow(data1)
  a6 <- sum(a2,a3,a5)



  if (a1 >0 & a6 >0) {Baseline <- a6/a1
  } else {Baseline <- 0}

  data2 <- data.frame()

  for (i in c(98:nrow(data[[a]]))){

    c <- sum(data[[a]][c((i-6):i),3])/7
  

    d <- sum(data[[a]][c((i-6):i),4])/7
    e <- sum(data[[a]][c((i-6):i),6])/7

    colmean <- sum(c,d,e)

    colmean2 <- sum(data[[a]][c((i-6):i),2])/7
    result <- cbind(as.character(data[[a]][i,1]),colmean2,colmean)
    data2 <- rbind(data2,result)
  }


  data2[,2] <- as.numeric(as.character(data2[,2]))
  data2[,3] <- as.numeric(as.character(data2[,3]))
  names(data2)[3] <- names(data[[a]])[2]

  x <- c()
  y <- data.frame()
  for (i in 1:nrow(data2)) {
    #b <- sum(data2[i,3],data2[i,4],data2[i,6])
    if (data2[i,2]>0 & data2[i,3] >0) {
      R <- data2[i,3]/data2[i,2]
    } else {R <- 0}
    R_Real <- R-Baseline
    x <- c(x,R_Real)
    z <- cbind(as.character(data2[,1],R_Real))
    y <- rbind(y,z)
  }
  R <- rbind(R,y)
 
  data2[,1]<-as.Date(data2[,1])
  
  #is_chn = F
  
  plot <- ggplot() +
    geom_line(aes(x=data2[,1], y = x), size = 1) +
    #geom_smooth(method = glm)+
    #lha_theme(is_chn)+
    ylab('R-Index')+
    xlab('Date')+
    ggtitle(substr(names(data2)[3],13,nchar(names(data2)[3])-1))+
    scale_x_date(date_labels = '%b-%d')
  
  
  ggsave(plot,filename = paste('/Users/qidiwang1/Desktop/Test/',substr(names(data2)[3],13,nchar(names(data2)[3])-1),'.png'))
  
}
View(R)
View(data[[3]])

a1 <- sum(data[[3]][c(1:91),2])/91
a2 <- sum(data[[3]][c(1:91),3])/91
a3 <- sum(data[[3]][c(1:91),4])/91
a4 <- sum(data[[3]][c(1:91),5])/91
a5 <- sum(data[[3]][c(1:91),6])/91 
sum(a2,a3,a5)/a1
a <- sum(data[[3]][c(92:98),3])/7
a
  
for (i in c(98:186)){
  
  a <- sum(data[[3]][c(i-6:i),3])/7
  
  b <- sum(data[[3]][c(i-6:i),4])/7
  c <- sum(data[[3]][c(i-6:i),6])/7
}
  colmean <- sum(a,b,c)
  colmean2 <- sum(data[[a]][c(i-6:i),2])/7
  result <- cbind(as.character(data[[a]][i,1]),colmean2,colmean)
  data2 <- rbind(data2,result)
} 
