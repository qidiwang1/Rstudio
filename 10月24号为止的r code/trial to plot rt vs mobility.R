mobility <- read.csv('/Users/qidiwang1/Desktop/mobility.csv')
blog <- read.csv('/Users/qidiwang1/Desktop/blog.csv')
JDATE <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/JDATE.csv')
rt <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/new rt.csv')
mobility$Date <- as.Date(mobility$Date,'%m/%d/%y')
mobility$alpha.2 <- as.character(mobility$alpha.2)
blog$alpha.2 <- as.character(blog$alpha.2)
JDATE$Date <- as.Date(JDATE$Date,'%m/%d/%y')
rt$alpha.2 <- as.character(rt$alpha.2)
rt$Date <- as.Date(rt$Date,'%m/%d/%y')

data <- rt%>%
  left_join(mobility, by = c('alpha.2','Date'))
data <- data%>%
  left_join(blog,by='alpha.2')
data <- data%>%
  left_join(JDATE, by='Date')

names(data)
analysis <- data[,c('Date','Country','alpha.2','rt','google_index_7d','Jdate','lockdown_date','JDATE_take_off')]

View(analysis)

analysis$Country <- factor(analysis$Country)

for (a in levels(analysis$Country)){
  country <- analysis[analysis$Country==a,]
  if (is.na(country$JDATE_take_off[1])==FALSE) {
    country <- country[country$Jdate>=country$JDATE_take_off[1],]
    if (is.na(country$lockdown_date[1])==FALSE)
      country <- na.omit(country)
      if (nrow(country)>0){
    
    
        d <- country[country$Jdate<country$lockdown_date[1],]
        if (nrow(d)>0) {
          c1 <- d
        } else {
          c1 <- 0
        }
        e <- country[country$Jdate>=country$lockdown_date[1],]
        if (nrow(d)>0){
          c2 <- d
        } else {c2 <- 0}
        if (c1!= 0 & c2!=0){
          p <- ggplot()+
            geom_point(aes(x=c1$rt, y = c1$google_index_7d,color='before lockdown'))+
            geom_point(aes(x=c2$rt, y = c2$google_index_7d, color = 'after lockdown'))
          print(p)
        } else {print(paste(as.character(a),'NANANA'))}
      } else {print(paste('OMG!',as.character(a),'cannot be terminated'))}
  } else {print(paste('OMG!',as.character(a),'cannot be analyzed'))}
}

  