merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
merge <- merge%>%
  group_by(Country)%>%
  mutate(newcase_7d = rollmean(New_case,7,fill=NA,align='right'))
countries <- c('United_Kingdom','Norway','Sweden',
               'Austria','Belgium','Bulgaria','Croatia','Czechia','Cyprus','Denmark','Estonia',
               'Finland','France','Germany','Greece','Hungary','Ireland','Italy','Latvia','Lithuania',
               'Luxembourg','Malta','Netherlands','Poland','Portugal','Romania','Slovakia','Slovenia',
               'Spain','Switzerland','Bosnia_and_Herzegovina','Montenegro',
               'Serbia','North_Macedonia','Ukraine','Albania','Belarus')
latin_other <- merge[merge$Sub_region=='Latin America and the Caribbean'&merge$Country!='Brazil',]

latin_other = sqldf("select sum(newcase_7d) from latin_other group by Date")
Europe <- merge[merge$Country%in%countries,]
Europe =   sqldf("select sum(newcase_7d) from Europe group by Date")
world =   sqldf("select sum(newcase_7d) from merge group by Date")
  
  
  
  
  
data <- data.frame(Date = merge$Date[merge$Country=='United_States_of_America'],
                   美国 = merge$newcase_7d[merge$Country=='United_States_of_America'],
                   印度 = merge$newcase_7d[merge$Country=='India'],
                   俄罗斯 = merge$newcase_7d[merge$Country=='Russia'],
                   巴西 = merge$newcase_7d[merge$Country=='Brazil'],
                   拉丁美洲其他 = latin_other$`sum(newcase_7d)`,
                   欧洲 = Europe$`sum(newcase_7d)`,
                   世界其他 = world$`sum(newcase_7d)`-Europe$`sum(newcase_7d)`-
                     latin_other$`sum(newcase_7d)`-merge$newcase_7d[merge$Country=='Russia']-
                     merge$newcase_7d[merge$Country=='India']-merge$newcase_7d[merge$Country=='United_States_of_America'])
write_xlsx(data,'/Users/qidiwang1/Desktop/区域图2.xlsx')








