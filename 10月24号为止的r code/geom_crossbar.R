merge <- read.csv('/Users/qidiwang1/Desktop/Book20.csv')
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



linear_data <- data.frame()
for (i in levels(merge[,4])) {
  country <- merge[merge[,4]==i,]


  if (nrow(country)>=19 & max(country$confirm) > 100)
    

    linear_data <- rbind(linear_data, country[,c(3,4,5,6,9,10,11,12)])
}

write.csv(linear_data,'/Users/qidiwang1/Desktop/linear.csv')
linear_data <- read.csv('/Users/qidiwang1/Desktop/linear.csv')
linear_data$Country <- factor(linear_data$Country)
levels(linear_data$Country)

View(linear_data)

for (i in levels(linear_data$Country)){
  country <- linear_data[linear_data$Country==i,]
  if (nrow(country)<19)
    print(nrow(country))
}

View(linear_data)


upper1 <- data.frame()
upper2 <- data.frame()
lower1 <- data.frame()
lower2 <- data.frame()
middle <- data.frame()



  for (i in levels(linear_data[,2])) {

    my_country <- linear_data[linear_data[,2]==i,]

    
    day <- data.frame('Day'=c(1:7))
    #final_data <- data.frame()
    final_upper1 <- data.frame()
    final_upper2 <- data.frame()
    final_lower1 <- data.frame()
    final_lower2 <- data.frame()
    final_middle <- data.frame()
    
    for (z in c(19:nrow(my_country))) {
      a <- my_country[c((z-12):z),]
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




View(upper1)
View(upper2)
View(lower1)
View(lower2)

write.csv(upper1,'/Users/qidiwang1/Desktop/R_transfer/7day_million_death_upper1.csv')
write.csv(upper2,'/Users/qidiwang1/Desktop/R_transfer/7day_million_death_upper2.csv')
write.csv(lower1,'/Users/qidiwang1/Desktop/R_transfer/7day_million_death_lower1.csv')
write.csv(lower2,'/Users/qidiwang1/Desktop/R_transfer/7day_million_death_lower2.csv')
write.csv(middle,'/Users/qidiwang1/Desktop/R_transfer/7day_million_death_middle.csv')



#final_data$Day <- as.numeric(as.character(final_data$Day))
final_upper1$V1 <- as.numeric(as.character(final_upper1$V1))
final_upper2$V1 <- as.numeric(as.character(final_upper2$V1))
final_lower1$V1 <- as.numeric(as.character(final_lower1$V1))
final_lower2$V1 <- as.numeric(as.character(final_lower2$V1))
final_middle$e <- as.numeric(as.character(final_middle$e))

final_middle
final_m
'ASEN <- c(38,49,59,76)
EC_ASIA <- c(17,35,43,68,70,75)
Latin1 <- c(3,9,10,16,18,20)
Latin2 <- c(24,25,33,50,85)
MENA <- c(2,26,40,51,65,77,78,80)
Anglo <- c(4,15,81,82)
G_Europe <- c(5,29,52,74)
N_Europe <- c(23,27,55,73)

E_Europe1 <- c(6,7,11,21,31,36,45)
E_Europe2 <- c(47,60,62,64,79,83)

S_Asia <- c(1,37,39,56,72)
Latin_Europe <- c(28,41,42,61,71)
SS_Africa <- c(14,19,22,30,46,53,54,66)'

ASEN <- c(13,38,49,59,76,85)
EC_ASIA <- c(17,35,34,43,68,70,75)
Latin1 <- c(3,9,10,16,18,20)
Latin2 <- c(24,25,33,50,85)
MENA <- c(2,26,40,51,65,77,78,80)
Anglo <- c(4,15,81,82)
G_Europe <- c(5,29,52,74)
N_Europe <- c(23,27,55,73)

E_Europe1 <- c(6,7,11,21,31,36,45)
E_Europe2 <- c(47,60,62,63,67,69)

S_Asia <- c(1,37,39,56,72)
Latin_Europe <- c(28,41,42,61,71)
SS_Africa1 <- c(14,19,22,30,32)
SS_Africa2 <- c(46,53,54,64,66)
final_middle

country_list <- SS_Africa2
df <- data.frame(
  trt = factor(final_upper1[country_list,]$V2),
  resp = final_upper1[country_list,]$V1,
  group = factor(final_upper1[country_list,]$V3),
  upper = final_upper1[country_list,]$V1,
  lower = final_lower1[country_list,]$V1
)

df2 <- data.frame(
  trt = factor(final_upper2[country_list,]$V2),
  resp = final_upper2[country_list,]$V1,
  group = factor(final_upper2[country_list,]$V3),
  upper = final_upper2[country_list,]$V1,
  lower = final_lower2[country_list,]$V1
)
df3 <- data.frame(
  trt = factor(final_middle[country_list,]$V2),
  resp = final_middle[country_list,]$e,
  group = factor(final_middle[country_list,]$V3),
  upper = final_middle[country_list,]$e,
  lower = final_middle[country_list,]$e
)

ggplot()+
  geom_crossbar(aes(x= df$trt, y= df$resp, ymin = df$lower, ymax = df$upper, color = df$group), width = 0.2)+
  geom_crossbar(aes(x= df2$trt, y= df2$resp, ymin = df2$lower, ymax = df2$upper, color = df2$group), width = 0.2)+
  geom_crossbar(aes(x= df3$trt, y= df2$resp, ymin = df3$lower, ymax = df3$upper, color = df3$group), width = 0.2)+
  xlab('Country')+
  ylab('Growth Rate') +
  scale_colour_manual(
    values = c("darkred",
               
               "brown1",
               "deepskyblue4",
               
               'darkslategray1',
               'chartreuse2'),
    limits = c('Increasing', 'Slightly Increasing', 'Decreasing','Slightly Decreasing','Unsure'))+
  ylim(-1,7)+
  theme(legend.title = element_blank())

  #theme(axis.text.x = element_text(angle = 90), legend.title = element_blank())
