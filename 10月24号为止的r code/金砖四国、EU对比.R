merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
library(dplyr)
library(zoo)
brics <- merge[merge$Country=='Brazil'|merge$Country=='South_Africa'|merge$Country=='India'|
                 merge$Country=='Russia',]
brics <- brics%>%
  group_by(index_dt)%>%
  mutate(newcase = sum(New_case))%>%
  mutate(newcase_7d = rollmean(newcase,7,fill=NA,align='right'))

USA <- merge[merge$iso3_name == 'USA',]
USA <- USA%>%
  mutate(newcase = rollmean(New_case,7,fill=NA,align='right'))
sign <- read.csv('/Users/qidiwang1/Desktop/打标.csv')

EU <- merge[merge$Country%in%levels(sign$EU),]
EU <- EU%>%
  group_by(index_dt)%>%
  mutate(newcase = sum(New_case))%>%
  mutate(newcase_7d = rollmean(newcase,7,fill=NA,align='right'))
Latin <- merge[merge$Sub_region=='Latin America and the Caribbean'&merge$Country!='Brazil',]

Latin <- Latin%>%
  group_by(index_dt)%>%
  mutate(newcase = sum(New_case))%>%
  mutate(newcase_7d = rollmean(newcase,7,fill=NA,align='right'))

c(levels(factor(brics$iso3_name)),levels(factor(EU$iso3_name)),
  levels(factor(Latin$iso3_name)),'USA')

already_in <- merge[merge$iso3_name%in%c(levels(factor(brics$iso3_name)),levels(factor(EU$iso3_name)),
                                         levels(factor(Latin$iso3_name)),'USA'),]
already_in <- already_in%>%
  group_by(index_dt)%>%
  mutate(newcase = sum(New_case))%>%
  mutate(newcase_7d = rollmean(newcase,7,fill=NA,align='right'))


merge <- merge%>%
  group_by(index_dt)%>%
  mutate(newcase = sum(New_case))%>%
  mutate(newcase_7d = rollmean(newcase,7,fill=NA,align='right'))
View(USA)
total <- data.frame(index_dt = USA$index_dt,
                    Date = as.Date(USA$Date),
                    USA = USA$New_case,
                    brics = brics$newcase[1:nrow(USA)],
                    EU = EU$newcase[1:nrow(USA)],
                    Latin = Latin$newcase[1:nrow(USA)],
                    already_in = already_in$newcase[1:nrow(USA)],
                    world = merge$newcase[1:nrow(USA)])


library(writexl)
write_xlsx(total,'/Users/qidiwang1/Desktop/total.xlsx')



write_xlsx(brics,'/Users/qidiwang1/Desktop/brics.xlsx')
library(ggplot2)
ggplot()+
  geom_line(aes(x=as.Date(merge$Date[merge$Country=='India'|merge$Country=='Pakistan'|
                                       merge$Country=='Bangladesh']),y=merge$New_case[merge$Country=='India'|merge$Country=='Pakistan'|
                                                                                         merge$Country=='Bangladesh'],
                color = merge$Country[merge$Country=='India'|merge$Country=='Pakistan'|
                                        merge$Country=='Bangladesh']))+
  theme(legend.title = element_blank())+
  xlab('')+
  ylab('Daily New Cases')
            
            
            
            
            
            
            
            
            
            
            
            
            

