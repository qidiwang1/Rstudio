PET <- read_excel('/Users/qidiwang1/Desktop/PET_DATA_20200915.xlsx')
#PET <- PET[!is.na(PET$gdp_relative),]

world <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/gdp_world.csv')
world$iso3_name <- as.character(world$iso3_name)
merge <- PET%>%
  left_join(world,by='iso3_name')
sign <- read.csv('/Users/qidiwang1/Desktop/打标.csv')

get_region_data <- function(data1,data2){
  aaa <- data1%>%
    group_by(index_dt)%>%
    mutate(total_confirmed_cases = sum(Total_cases),
           million_cases = sum(Total_cases)/sum(Population)*1000000,
           total_death = sum(Total_death),
           million_death = sum(Total_death)/sum(Population)*1000000,
           fatality = sum(Total_death)/sum(Total_cases),
           mobility = sum(Population/sum(Population)*mobility_7d),
           newcase = sum(New_case),
           newdeath = sum((New_death)),
           loss = sum(X2019/sum(X2019)*average_loss))
  return(
    rbind(data.frame(Date = aaa$Date[c(1:nrow(data2))],
               index_dt = aaa$index_dt[c(1:nrow(data2))],
               Region = as.character(aaa$Sub_region[c(1:nrow(data2))]),
               Total_cases = aaa$total_confirmed_cases[c(1:nrow(data2))],
               Cases_per_Million = aaa$million_cases[c(1:nrow(data2))],
               Total_death = aaa$total_death[c(1:nrow(data2))],
               Death_per_Million = aaa$million_death[c(1:nrow(data2))],
               Fatality_rate = aaa$fatality[c(1:nrow(data2))],
               mobility_7d = aaa$mobility[c(1:nrow(data2))],
               New_case = aaa$newcase[c(1:nrow(data2))],
               New_death = aaa$newdeath[c(1:nrow(data2))],
               loss = aaa$loss[c(1:nrow(data2))]),
          data.frame(Date = data2$Date,
                     index_dt = data2$index_dt,
                     Region = as.character(data2$Country[1]),
                     Total_cases = data2$Total_cases,
                     Cases_per_Million = data2$Cases_per_Million,
                     Total_death = data2$Total_death,
                     Death_per_Million = data2$Death_per_Million,
                     Fatality_rate = data2$Fatality_rate,
                     mobility_7d = data2$mobility_7d,
                     New_case = data2$New_case,
                     New_death = data2$New_death,
                     loss = data2$average_loss))
    
  )
}



South_asia <- get_region_data(data1 =  merge[merge$Sub_region=='Southern Asia'&merge$Country!='India',],
                              data2 = merge[merge$Country=='India',])

east_asia <- merge[merge$Sub_region=='Eastern Asia'&merge$Country!='China',]
east_asia <- east_asia[!is.na(east_asia$Date),]
East_asia <- get_region_data(data1 = east_asia,
                             data2 = merge[merge$Country=='China',])

East_europe <- get_region_data(data1=merge[merge$Sub_region=='Eastern Europe'&merge$Country!='Russia',],
                             data2 = merge[merge$Country=='Russia',])


Latin <- get_region_data(data1 = merge[merge$Sub_region=='Latin America and the Caribbean'&merge$Country!='Brazil',],
                         data2 = merge[merge$Country=='Brazil',])


Africa <- get_region_data(data1 = merge[merge$Sub_region=='Sub-Saharan Africa'&merge$Country!='South_Africa',],
                          data2 = merge[merge$Country=='South_Africa',])

Europe <- get_region_data(data1 = merge[merge$Country%in%levels(sign$EU),],
                          data2 = merge[merge$iso3_name=='USA',])

DM <- get_region_data(data1 = merge[merge$Country=='Malaysia'|
                                      merge$Country=='Thailand'|merge$Country=='Philippines'|
                                      merge$Country=='Indonesia'|merge$Country=='Vietnam',],
                      merge[merge$Country=='United_Kingdom',])


data <- rbind(South_asia,East_asia,East_europe,Latin,Africa,Europe,DM)
write_xlsx(data,'/Users/qidiwang1/Desktop/每周热点4.xlsx')





sum(is.na(merge[merge$Country=='Singapore'|merge$Country=='Malaysia'|
             merge$Country=='Thailand'|merge$Country=='Philippines'|
             merge$Country=='Indonesia'|merge$Country=='Vietnam'|
             merge$Country=='Laos'|merge$Country=='Myanmar'|
             merge$Country=='Cambodia'|merge$Country=='Brunei_Darussalam',]$X2019))
data <- rbind(South_asia,East_asia,East_europe,Latin,Africa,Europe)
write_xlsx(data,'/Users/qidiwang1/Desktop/每周热点3.xlsx')




write_xlsx(list(South_asia,East_asia,West_asia,Latin,Africa,Europe),'/Users/qidiwang1/Desktop/每周热点.xlsx')

for (a in c(South_asia,East_asia,West_asia,Latin,Africa,Europe)){
  View(a)
}
South_asia%>%
  group_by(Region)%>%
  mutate(newcase_7d = rollmean(New_case,7,fill=NA,align='right'),
         newdeath_7d = rollmean(New_death,7,fill=NA,align='right'))
p1 <- ggplot()+
  geom_line(aes(x=as.Date(South_asia$Date),y=South_asia[,4],color = South_asia$Region))
ggsave(p1,filename = paste('/Users/qidiwang1/Desktop/每周热点/',as.character(South_asia$Region[1]),names(South_asia)[4],'.png'))

plot_data <- function(data){
  data$newcase_7d = rollmean(data$New_case,7,fill = NA, align='right')
  data$newdeath_7d = rollmean(data$New_death,7,fill = NA, aligh = 'right')

  for (i in c(4:ncol(data))){
    p1 <- ggplot()+
          geom_line(aes(x=as.Date(data$Date),y=data[,i],color = data$Region))+
      xlab('')+
      ylab(names(data)[i])+
      ggtitle(as.character(data$Region[1]))
    ggsave(p1,filename = paste('/Users/qidiwang1/Desktop/每周热点/',as.character(data$Region[1]),names(data)[i],'.png'))
  }
}
plot_data(South_asia)
plot_data(East_asia)
plot_data(West_aisa)
plot_data(Latin)
plot_data(Africa)
plot_data(Europe)

View(South_asia)
for (a in c(South_asia,East_asia,West_asia,Latin,Africa,Europe)){
  data <- a%>%
    group_by(Region)%>%
    mutate(newcase_7d = rollmean(New_case,7,fill=NA,align='right'),
           newdeath_7day = rollmean(New_death,fill=NA,align='right'))
  for (i in c(4:ncol(data))){
    p1 <- ggplot()+
      geom_line(aes(x=as.Date(a$Date),y=a[,i],color = a$Region))
    ggsave(p1,filename = paste('/Users/qidiwang1/Desktop/每周热点/',as.character(data$Region[1]),names(data)[1],'.png'))
  }
}



levels(factor(east_asia$Country))







