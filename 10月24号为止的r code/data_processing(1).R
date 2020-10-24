filenames <- list.files('/Users/qidiwang1/Desktop/Google Trend/', pattern = '*.csv$', full.names = TRUE)


my_data <- lapply(filenames, read.csv, skip = 2)

View(my_data)


for (a in c(1:256)){
  if (nrow(my_data[[a]])>0)
    for (i in c(1:nrow(my_data[[a]]))){
      for (j in c(1:ncol(my_data[[a]]))){
       if (my_data[[a]][i,j] == '<1') my_data[[a]][i,j] <- NA
    }
  }
}

x <- data.frame('Country'=NA,'10.01-12.30 Influenza'=NA,'10.01-12.30 SARS'=NA,
                '10.01-12.30 MERS'=NA,'10.01-12.30 Swine'=NA,'10.01-12.30 Spanish'=NA,
                '12.31-1.22 Influenza'=NA,'12.31-1.22 SARS'=NA,
                '12.31-1.22 MERS'=NA,'12.31-1.22 Swine'=NA,'12.31-1.22 Spanish'=NA,
                '1.23-2.27 Influenza'=NA,'1.23-2.27 SARS'=NA,
                '1.23-2.27 MERS'=NA,'1.23-2.27 Swine'=NA,'1.23-2.27 Spanish'=NA,
                '2.28-3.10 Influenza'=NA,'2.28-3.10 SARS'=NA,
                '2.28-3.10 MERS'=NA,'2.28-3.10 Swine'=NA,'2.28-3.10 Spanish'=NA,
                '3.11-NOW Influenza'=NA,'3.11-NOW SARS'=NA,
                '3.11-NOW MERS'=NA,'3.11-NOW Swine'=NA,'3.11-NOW Spanish'=NA)

for (a in c(1:256)){
  my_data[[a]][,2] <- as.numeric(as.character(my_data[[a]][,2]))
  my_data[[a]][,3] <- as.numeric(as.character(my_data[[a]][,3]))
  my_data[[a]][,4] <- as.numeric(as.character(my_data[[a]][,4]))
  my_data[[a]][,5] <- as.numeric(as.character(my_data[[a]][,5]))
  my_data[[a]][,6] <- as.numeric(as.character(my_data[[a]][,6]))
  
  if (nrow(my_data[[a]]) >0)
    data1 <- my_data[[a]][c(1:91),]
    data2 <- my_data[[a]][c(92:114),]
    data3 <- my_data[[a]][c(115:150),]
    data4 <- my_data[[a]][c(151:162),]
    data5 <- my_data[[a]][c(163:nrow(my_data[[a]])),]
  
  a1 <- sum(data1[,2,drop = FALSE],na.rm = TRUE)/nrow(data1)
  a2 <- sum(data1[,3,drop = FALSE],na.rm = TRUE)/nrow(data1)
  a3 <- sum(data1[,4,drop = FALSE],na.rm = TRUE)/nrow(data1)
  a4 <- sum(data1[,5,drop = FALSE],na.rm = TRUE)/nrow(data1)
  a5 <- sum(data1[,6,drop = FALSE],na.rm = TRUE)/nrow(data1)
  a6 <- sum(data2[,2,drop = FALSE],na.rm = TRUE)/nrow(data2)
  a7 <- sum(data2[,3,drop = FALSE],na.rm = TRUE)/nrow(data2)
  a8 <- sum(data2[,4,drop = FALSE],na.rm = TRUE)/nrow(data2)
  a9 <- sum(data2[,5,drop = FALSE],na.rm = TRUE)/nrow(data2)
  a10 <- sum(data2[,6,drop = FALSE],na.rm = TRUE)/nrow(data2)
  a11 <- sum(data3[,2,drop = FALSE],na.rm = TRUE)/nrow(data3)
  a12 <- sum(data3[,3,drop = FALSE],na.rm = TRUE)/nrow(data3)
  a13 <- sum(data3[,4,drop = FALSE],na.rm = TRUE)/nrow(data3)
  a14 <- sum(data3[,5,drop = FALSE],na.rm = TRUE)/nrow(data3)
  a15 <- sum(data3[,6,drop = FALSE],na.rm = TRUE)/nrow(data3)
  a16 <- sum(data4[,2,drop = FALSE],na.rm = TRUE)/nrow(data4)
  a17 <- sum(data4[,3,drop = FALSE],na.rm = TRUE)/nrow(data4)
  a18 <- sum(data4[,4,drop = FALSE],na.rm = TRUE)/nrow(data4)
  a19 <- sum(data4[,5,drop = FALSE],na.rm = TRUE)/nrow(data4)
  a20 <- sum(data4[,6,drop = FALSE],na.rm = TRUE)/nrow(data4)
  a21 <- sum(data5[,2,drop = FALSE],na.rm = TRUE)/nrow(data5)
  a22 <- sum(data5[,3,drop = FALSE],na.rm = TRUE)/nrow(data5)
  a23 <- sum(data5[,4,drop = FALSE],na.rm = TRUE)/nrow(data5)
  a24 <- sum(data5[,5,drop = FALSE],na.rm = TRUE)/nrow(data5)
  a25 <- sum(data5[,6,drop = FALSE],na.rm = TRUE)/nrow(data5)
  x <- rbind(x,list(names(data1)[2], a1, a2, a3, a4, a5,a6,a7,a8,a9,
                    a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,
                    a21,a22,a23,a24,a25))
}


write.csv(x,'/Users/qidiwang1/Desktop/2key_words.csv')


View(x)



