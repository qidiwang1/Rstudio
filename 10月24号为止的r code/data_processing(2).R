filenames <- list.files('/Users/qidiwang1/Desktop/Google Trend2/', pattern = '*.csv$', full.names = TRUE)


my_data <- lapply(filenames, read.csv, skip = 2)

View(my_data)


for (a in c(1:258)){
  if (nrow(my_data[[a]])>0)
    for (i in c(1:nrow(my_data[[a]]))){
      for (j in c(1:ncol(my_data[[a]]))){
        if (my_data[[a]][i,j] == '<1') my_data[[a]][i,j] <- NA
      }
    }
}

x <- data.frame('Country'=NA,'10.01-12.30 Influenza'=NA,'10.01-12.30 corona virus'=NA,
                '10.01-12.30 COVID'=NA,
                '12.31-1.22 Influenza'=NA,'12.31-1.22 corona virus'=NA,
                '12.31-1.22 COVID'=NA,
                '1.23-2.27 Influenza'=NA,'1.23-2.27 corona virus'=NA,
                '1.23-2.27 COVID'=NA,
                '2.28-3.10 Influenza'=NA,'2.28-3.10 corona virus'=NA,
                '2.28-3.10 COVID'=NA,
                '3.11-NOW Influenza'=NA,'3.11-NOW corona virus'=NA,
                '3.11-NOW COVID'=NA)

for (a in c(1:258)){
  my_data[[a]][,2] <- as.numeric(as.character(my_data[[a]][,2]))
  my_data[[a]][,3] <- as.numeric(as.character(my_data[[a]][,3]))
  my_data[[a]][,4] <- as.numeric(as.character(my_data[[a]][,4]))
  
  if (nrow(my_data[[a]]) >0)
    data1 <- my_data[[a]][c(1:91),]
    data2 <- my_data[[a]][c(92:114),]
    data3 <- my_data[[a]][c(115:150),]
    data4 <- my_data[[a]][c(151:162),]
    data5 <- my_data[[a]][c(163:nrow(my_data[[a]])),]
  
  a1 <- sum(data1[,2,drop = FALSE],na.rm = TRUE)/nrow(data1)
  a2 <- sum(data1[,3,drop = FALSE],na.rm = TRUE)/nrow(data1)
  a3 <- sum(data1[,4,drop = FALSE],na.rm = TRUE)/nrow(data1)
  
  a4 <- sum(data2[,2,drop = FALSE],na.rm = TRUE)/nrow(data2)
  a5 <- sum(data2[,3,drop = FALSE],na.rm = TRUE)/nrow(data2)
  a6 <- sum(data2[,4,drop = FALSE],na.rm = TRUE)/nrow(data2)

  a7 <- sum(data3[,2,drop = FALSE],na.rm = TRUE)/nrow(data3)
  a8 <- sum(data3[,3,drop = FALSE],na.rm = TRUE)/nrow(data3)
  a9 <- sum(data3[,4,drop = FALSE],na.rm = TRUE)/nrow(data3)

  a10 <- sum(data4[,2,drop = FALSE],na.rm = TRUE)/nrow(data4)
  a11 <- sum(data4[,3,drop = FALSE],na.rm = TRUE)/nrow(data4)
  a12 <- sum(data4[,4,drop = FALSE],na.rm = TRUE)/nrow(data4)
 
  a13 <- sum(data5[,2,drop = FALSE],na.rm = TRUE)/nrow(data5)
  a14 <- sum(data5[,3,drop = FALSE],na.rm = TRUE)/nrow(data5)
  a15 <- sum(data5[,4,drop = FALSE],na.rm = TRUE)/nrow(data5)
 
  x <- rbind(x,list(names(data1)[2], a1, a2, a3, a4, a5,a6,a7,a8,a9,
                    a10, a11, a12, a13, a14, a15))
}




write.csv(x,'/Users/qidiwang1/Desktop/add.csv')


View(x)



