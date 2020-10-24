merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
#merge <- read.csv('/Users/qidiwang1/Desktop/test_merge.csv')

library(smoother)

#这里是对每一个国家运算Raw Death Doubling Days，country$death请贴入total death那一列
#merge <- merge[merge$Country!='Benin',]
merge$Country <- factor(merge$Country)
data <- data.frame()
for (a in levels(merge$Country)){
  country <- merge[merge$Country==a,]
  Jdate <- 0
  determine_Jdate <- 0
  doubling_day <- c()
  for (i in c(1:nrow(country))){
    cutoff <- country$confirm[i]/2
    subgroup <- country[country$confirm<=cutoff,]
    Jdate <- tail(subgroup$JDATE,1)

    min_confirm <- tail(subgroup$confirm,1)
    max_confirm <- country[country$JDATE==(Jdate+1),]$confirm
    determine_Jdate <- Jdate + (cutoff-min_confirm)/(max_confirm-min_confirm)
    if (nrow(subgroup)==0){
      days <- 0 
      doubling_day <- c(doubling_day,days)
    } else if (country$confirm[i]==0) {
      days <- 0 
      doubling_day <- c(doubling_day,days)
    } else {
      days <- country$JDATE[i]-determine_Jdate
      doubling_day <- c(doubling_day,days)
    }
  }
  country$confirm_doubling_days <- doubling_day
  data <- rbind(data,country)
}

merge <- data.frame()
for (a in levels(data$Country)){
  country <- data[data$Country==a,]
  Jdate <- 0
  determine_Jdate <- 0
  doubling_day <- c()
  for (i in c(1:nrow(country))){
    cutoff <- country$death[i]/2
    subgroup <- country[country$death<=cutoff,]
    Jdate <- tail(subgroup$JDATE,1)
    min_death <- tail(subgroup$death,1)
    max_death <- country[country$JDATE==(Jdate+1),]$death
    determine_Jdate <- Jdate + (cutoff-min_death)/(max_death-min_death)
    if (nrow(subgroup)==0){
      days <- 0 
      doubling_day <- c(doubling_day,days)
    } else if (country$death[i]==0) {
      days <- 0 
      doubling_day <- c(doubling_day,days)
    } else {
      days <- country$JDATE[i]-determine_Jdate
      doubling_day <- c(doubling_day,days)
    }
  }
  country$death_doubling_days <- doubling_day
  merge <- rbind(merge,country)
}


#以下为对Raw Death Doubling Days与Raw Confirm Doubling Days进行高斯平滑处理，country$death_doubling_days请贴入Raw Death Doubling Days那一列
data <- data.frame()
for (a in levels(merge$Country)){
  country <- merge[merge$Country==a,]
  smooth_death <- smth(country$death_doubling_days,window = 7, mehtod = 'gaussian')
  smooth_confirm <- smth(country$confirm_doubling_days, window = 7, method = 'gaussian')
  country$smooth_death_doubling_days <- smooth_death
  country$smooth_death_doubling_days <- ifelse(is.na(country$smooth_death_doubling_days)==TRUE,
                                               country$death_doubling_days,country$smooth_death_doubling_days)
  country$smooth_confirm_doubling_days <- smooth_confirm
  country$smooth_confirm_doubling_days <- ifelse(is.na(country$smooth_confirm_doubling_days)==TRUE,
                                                 country$confirm_doubling_days,country$smooth_confirm_doubling_days)
  data <- rbind(data,country)
}

library(anchors)
data <- replace.value(data,'smooth_confirm_doubling_days',0,as.double(NA))

#write.csv(data,'/Users/qidiwang1/Desktop/data.csv')

#write.csv(data,'/Users/qidiwang1/Desktop/test_merge.csv')
#以下为Economics Policy Score的运算

policy_economics <- data.frame()
for (i in c(1:nrow(data))) {

  if (is.na(data$C1_School.closing[i])==TRUE) {
    a <- 0
  } else {
    if (is.na(data$C1_Flag[i])==TRUE|data$C1_Flag[i]==0) {
      a <- data$C1_School.closing[i]*0.5/3*0.025
    } else {
      a <- data$C1_School.closing[i] * data$C1_Flag[i]/3*0.025
    }
  }
  if (is.na(data$C2_Workplace.closing[i])==TRUE) {
    b <- 0
  } else {
    if (is.na(data$C2_Flag[i])==TRUE|data$C2_Flag[i]==0) {
      b <- data$C2_Workplace.closing[i]*0.5/3*0.35
    } else {
      b <- data$C2_Workplace.closing[i] * data$C2_Flag[i]/3*0.35
    }
  }
  if (is.na(data$C3_Cancel.public.events[i])==TRUE) {
    c <- 0
  } else {
    if (is.na(data$C3_Flag[i])==TRUE|data$C3_Flag[i]==0) {
      c <- data$C3_Cancel.public.events[i]*0.5/2*0.025
    } else {
      c <- data$C3_Cancel.public.events[i] * data$C3_Flag[i]/2*0.025
    }
  }
  if (is.na(data$C4_Restrictions.on.gatherings[i])==TRUE) {
    d <- 0
  } else {
    if (is.na(data$C4_Flag[i])==TRUE|data$C4_Flag[i]==0) {
      d <- data$C4_Restrictions.on.gatherings[i]*0.5/4*0.05
    } else {
      d <- data$C4_Restrictions.on.gatherings[i] * data$C4_Flag[i]/4*0.05
    }
  }
  if (is.na(data$C5_Close.public.transport[i])==TRUE) {
    e <- 0
  } else {
    if (is.na(data$C5_Flag[i])==TRUE|data$C5_Flag[i]==0) {
      e <- data$C5_Close.public.transport[i]*0.5/2*0.2
    } else {
      e <- data$C5_Close.public.transport[i] * data$C5_Flag[i]/2*0.2
    }
  }
  if (is.na(data$C6_Stay.at.home.requirements[i])==TRUE) {
    f <- 0
  } else {
    if (is.na(data$C6_Flag[i])==TRUE|data$C6_Flag[i]==0) {
      f <- data$C6_Stay.at.home.requirements[i]*0.5/3*0.1
    } else {
      f <- data$C6_Stay.at.home.requirements[i] * data$C6_Flag[i]/3*0.1
    }
  }
  if (is.na(data$C7_Restrictions.on.internal.movement[i])==TRUE) {
    g <- 0
  } else {
    if (is.na(data$C7_Flag[i])==TRUE|data$C7_Flag[i]==0) {
      g <- data$C7_Restrictions.on.internal.movement[i]*0.5/2*0.2
    } else {
      g <- data$C7_Restrictions.on.internal.movement[i] * data$C7_Flag[i]/2*0.2
    }
  }
  
  if (is.na(data$C8_International.travel.controls[i])==TRUE) {
    h <- 0
  } else {
    h <- data$C8_International.travel.controls[i]/4*0.05
    
  }
  index <- sum(a,b,c,d,e,f,g,h)
  policy_economics <- rbind(policy_economics,index)
  
}

merge <- cbind(data, policy_economics)

names(merge)[ncol(merge)] <- 'Daily_Economics_Policy_Score'

write.csv(merge,'/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
############################

