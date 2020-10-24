gc();rm(list = ls())
#setwd("/Users//Users/qidiwang1/Desktop/plot_codes/")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/helper.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/lha_theme.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/plot_functions.R")

filenames <- list.files('/Users/qidiwang1/Desktop/Google', pattern = '*.csv$', full.names = TRUE)


my_data <- lapply(filenames, read.csv, skip = 2)
View(my_data)


for (a in c(1:length(my_data))) {
  if (nrow(my_data[[a]]) == 0)
    my_data[[a]]<-NULL
}
my_data[[59]]<-NULL
my_data[[124]]<-NULL
my_data[[145]]<-NULL
my_data[[123]]<-NULL
my_data[[143]]<-NULL
my_data[[142]]<-NULL
View(my_data)

data <- my_data
data

for (a in c(1:length(data))){
  if (nrow(data[[a]])>0) 
    for (i in c(1:nrow(data[[a]]))){
      for (j in c(1:ncol(data[[a]]))){
        if (data[[a]][i,j] == '<1') data[[a]][i,j] <- NA
      }
      
    }
}



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

View(data[[1]])

R <- data.frame()
dataset <- data.frame()
for (a in c(1:6)) {
  data[[a]][,2] <- ifelse(is.na(data[[a]][,2]),0,data[[a]][,2])
  data[[a]][,3] <- ifelse(is.na(data[[a]][,3]),0,data[[a]][,3])
  data[[a]][,4] <- ifelse(is.na(data[[a]][,4]),0,data[[a]][,4])
  data[[a]][,5] <- ifelse(is.na(data[[a]][,5]),0,data[[a]][,5])
  data[[a]][,6] <- ifelse(is.na(data[[a]][,6]),0,data[[a]][,6])

  data1 <- data[[a]][c(1:91),]
  a1 <- sum(data1[,2])/nrow(data1)
  a2 <- sum(data1[,3])/nrow(data1)
  a3 <- sum(data1[,4])/nrow(data1)
  a4 <- sum(data1[,5])/nrow(data1)
  a5 <- sum(data1[,6])/nrow(data1)
  a6 <- sum(a2,a3,a5)
  if (a1 >0 & a6 >0) {Baseline <- a6/a1
  } else {Baseline <- 0}

  let_go <- data[[a]][c(92:nrow(data[[a]])),]
  let_go[,2] <- as.character(let_go[,2])
  let_go[,3] <- as.character(let_go[,3])
  let_go[,4] <- as.character(let_go[,4])
  let_go[,5] <- as.character(let_go[,5]) 
  let_go[,6] <- as.character(let_go[,6])
  let_go[,2] <- as.numeric(let_go[,2])
  let_go[,3] <- as.numeric(let_go[,3])
  let_go[,4] <- as.numeric(let_go[,4])
  let_go[,5] <- as.numeric(let_go[,5]) 
  let_go[,6] <- as.numeric(let_go[,6])
  data2 <- data.frame()
  for (i in c(7:(nrow(let_go)))){
    colmean <- sum(data[[a]][c(i-6:i),c(3,4,6)])/7
    colmean2 <- sum(data[[a]][c(i-6:i),2])/7
    result <- cbind(as.character(let_go[i,1]),colmean2,colmean)
    data2 <- rbind(data2,result)
  }
  names(data2)[3] <- names(let_go)[2]

  data2[,2] <- as.character(data2[,2])
  data2[,3] <- as.character(data2[,3])
  data2[,2] <- as.numeric(data2[,2])
  data2[,3] <- as.numeric(data2[,3])
  #data2 <- data[[a]][c(92:nrow(data[[a]])),]
  x <- c()
  for (i in 1:nrow(data2)) {
    #b <- sum(data2[i,3],data2[i,4],data2[i,6])
    if (data2[i,2]>0 & data2[i,3] >0) {
      R <- data2[i,3]/data2[i,2]
    } else {R <- 0}
    R_Real <- R-Baseline
    x <- c(x,R_Real)
  }
  print(x)
}
  
  R <- rbind(R,x)

  
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
final <- data.frame()
for (a in c(1:length(data))) {
  data[[a]][,2] <- ifelse(is.na(data[[a]][,2]),0,data[[a]][,2])
  data[[a]][,3] <- ifelse(is.na(data[[a]][,3]),0,data[[a]][,3])
  data[[a]][,4] <- ifelse(is.na(data[[a]][,4]),0,data[[a]][,4])
  data[[a]][,5] <- ifelse(is.na(data[[a]][,5]),0,data[[a]][,5])
  data[[a]][,6] <- ifelse(is.na(data[[a]][,6]),0,data[[a]][,6])
  
  data1 <- data[[a]][c(1:91),]
  a1 <- sum(data1[,2])/nrow(data1)
  a2 <- sum(data1[,3])/nrow(data1)
  a3 <- sum(data1[,4])/nrow(data1)
  a4 <- sum(data1[,5])/nrow(data1)
  a5 <- sum(data1[,6])/nrow(data1)
  a6 <- sum(a2,a3,a5)
  if (a1 >0 & a6 >0) {Baseline <- a6/a1
  } else {Baseline <- 0}
  
  let_go <- data[[a]][c(92:nrow(data[[a]])),]
  let_go[,2] <- as.character(let_go[,2])
  let_go[,3] <- as.character(let_go[,3])
  let_go[,4] <- as.character(let_go[,4])
  let_go[,5] <- as.character(let_go[,5]) 
  let_go[,6] <- as.character(let_go[,6])
  let_go[,2] <- as.numeric(let_go[,2])
  let_go[,3] <- as.numeric(let_go[,3])
  let_go[,4] <- as.numeric(let_go[,4])
  let_go[,5] <- as.numeric(let_go[,5]) 
  let_go[,6] <- as.numeric(let_go[,6])
  data2 <- data.frame()
  for (i in 8:(nrow(let_go))){
    colmean <- sum(data[[a]][c(i-7:i),c(3,4,6)])/7
    colmean2 <- sum(data[[a]][c(i-7:i),2])/7
    result <- cbind(as.character(let_go[i,1]),colmean2,colmean)
    data2 <- rbind(data2,result)
  }
  names(data2)[3] <- names(let_go)[2]
  
  data2[,2] <- as.character(data2[,2])
  data2[,3] <- as.character(data2[,3])
  data2[,2] <- as.numeric(data2[,2])
  data2[,3] <- as.numeric(data2[,3])
  #data2 <- data[[a]][c(92:nrow(data[[a]])),]
  x <- c()
  y<-c()
  for (i in 1:nrow(data2)) {
    #b <- sum(data2[i,3],data2[i,4],data2[i,6])
    if (data2[i,2]>0 & data2[i,3] >0) {
      R <- data2[i,3]/data2[i,2]
    } else {R <- 0}
    R_Real <- R-Baseline
    x <- c(x,R_Real)
    y <- c(y, substr(names(data2)[3],13,nchar(names(data2)[3])-1))
  }
  xyz <- cbind(as.character(data2[,1]),x,y)
  final <- rbind(final,xyz)
}
write.csv(final,'/Users/qidiwang1/Desktop/RRRRRR.csv')
View(final)  
  




  
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
  
  
  ggsave(plot,filename = paste('/Users/qidiwang1/Desktop/test/',substr(names(data2)[3],13,nchar(names(data2)[3])-1),'.png'))
  
}
data[[2]]
let_go <- data[[2]][c(92:nrow(data[[2]])),]
data2 <- data.frame()
for (i in 4:(nrow(let_go)-3)){
  colmean <- (sum(data[[2]][c(i-3:i+3),3])+sum(data[[2]][c(i-3:i+3),4])+sum(data[[2]][c(i-3:i+3),6]))/7
  colmean2 <- sum(data[[2]][c(i-3:i+3),2])/7
  result <- cbind(as.character(let_go[i,1]),colmean2,colmean)
  data2 <- rbind(data2,result)
}
data[[2]][c(i-3,i+3),2]
data2

final <- data.frame()
for (a in c(1:length(data))) {
  data[[a]][,2] <- ifelse(is.na(data[[a]][,2]),0,data[[a]][,2])
  data[[a]][,3] <- ifelse(is.na(data[[a]][,3]),0,data[[a]][,3])
  data[[a]][,4] <- ifelse(is.na(data[[a]][,4]),0,data[[a]][,4])
  data[[a]][,5] <- ifelse(is.na(data[[a]][,5]),0,data[[a]][,5])
  data[[a]][,6] <- ifelse(is.na(data[[a]][,6]),0,data[[a]][,6])
  
  data1 <- data[[a]][c(1:91),]
  a1 <- sum(data1[,2])/nrow(data1)
  a2 <- sum(data1[,3])/nrow(data1)
  a3 <- sum(data1[,4])/nrow(data1)
  a4 <- sum(data1[,5])/nrow(data1)
  a5 <- sum(data1[,6])/nrow(data1)
  a6 <- sum(a2,a3,a5)
  if (a1 >0 & a2 >0) {Baseline <- a2/a1
  } else {Baseline <- 0}
  
  
  data2 <- data[[a]][c(92:nrow(data[[a]])),]
  x <- c()
  y <- c()
  for (i in 1:nrow(data2)) {
    b <- sum(data2[i,3],data2[i,4],data2[i,6])
    if (b>0 & data2[i,2] >0) {
      R <- b/data2[i,2]
    } else {R <- 0}
    R_Real <- R-Baseline
    x <- c(x,R_Real)
    y <- c(y, substr(names(data2)[2],13,nchar(names(data2)[2])-1))
  }
  xyz <- cbind(as.character(data2[,1]),x,y)
  final <- rbind(final,xyz)
}  

write.csv(final,'/Users/qidiwang1/Desktop/R.csv')

View(data[[2]])


View(final)  
  data2[,1]<-as.Date(data2[,1])
  
  is_chn = F
  
  plot <- ggplot() +
    geom_line(aes(x=data2[,1], y = x), size = 1) +
    lha_theme(is_chn)+
    ylab('R-Index')+
    xlab('Date')+
    ggtitle(substr(names(data2)[2],13,nchar(names(data2)[2])-1))+
    scale_x_date(date_labels = '%b-%d')
  
  
  ggsave(plot,filename = paste('/Users/qidiwang1/Desktop/test/',substr(names(data2)[2],13,nchar(names(data2)[2])-1),'.png'))
  
}

