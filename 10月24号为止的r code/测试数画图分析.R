gc();rm(list = ls())
#setwd("/Users//Users/qidiwang1/Desktop/plot_codes/")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/helper.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/lha_theme.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/plot_functions.R")
merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
name_match <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/name_match.csv')
gdp <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/gdp_world.csv')
name_match$iso3_name <- as.character(name_match$iso3_name)
merge$iso3_name <- as.character(merge$iso3_name)
gdp$iso3_name <- as.character(gdp$iso3_name)
merge <- merge%>%
  left_join(name_match,by='iso3_name')%>%
  left_join(gdp,by = 'iso3_name')
merge$gdp_per_person <- merge$X2019/merge$Population
merge$Region2 <- 'Eastern Asia'
merge$Region2[merge$Sub_region=="Northern Africa"|merge$Sub_region=="Western Asia"] <- 'MENA'
merge$Region2[merge$Sub_region=='Western Europe'|merge$Sub_region=="Southern Europe"|merge$Sub_region=="Northern Europe"] <- 'Western Europe'
merge$Region2[merge$Sub_region=="Australia and New Zealand"|merge$Sub_region=="Melanesia"|merge$Sub_region=="Micronesia"] <- 'Oceania'
merge$Region2[merge$Sub_region=="Northern America"] <- 'Northern America'
merge$Region2[merge$Sub_region=="Latin America and the Caribbean"] <- 'Latin America and the Caribbean'
merge$Region2[merge$Sub_region=="South-eastern Asia"|merge$Sub_region=="Southern Asia"|merge$Sub_region=="Central Asia"] <- 'Rest of Asia'
merge$Region2[merge$Sub_region=="Sub-Saharan Africa"] <- 'Sub-Saharan Africa'
merge$Region2[merge$Sub_region=="Eastern Europe"] <- 'Eastern Europe'






data <- merge[!is.na(merge$Total_tests),]
data1 <- data[data$index_dt==max(data$index_dt),]
write_xlsx(data1,'/Users/qidiwang1/Desktop/plot.xlsx')



kernal

data2 <- data[data$index_dt==20200630,]
data3 <- data[data$index_dt==20200531,]
data4 <- data[data$index_dt==20200430,]

library(ggplot2)
p <- ggplot()+
  geom_point(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_tests/data1$Population*1000000,10),color = data1$Region))
p + annotate("text", x = data1$Total_cases/data1$Total_tests, y = log(data1$Total_tests/data1$Population*1000000,10), label = as.character(data1$iso3_name))  


p2 <- ggplot()+
  geom_point(aes(x=data2$Total_cases/data2$Total_tests,y=log(data2$Total_tests/data2$Population*1000000,10),color = data2$Region))
p2 + annotate("text", x = data2$Total_cases/data2$Total_tests, y = log(data2$Total_tests/data2$Population*1000000,10), label = as.character(data2$iso3_name))  

p3 <- ggplot()+
  geom_point(aes(x=data3$Total_cases/data3$Total_tests,y=log(data3$Total_tests/data3$Population*1000000,10),color = data3$Region))
p3 + annotate("text", x = data3$Total_cases/data3$Total_tests, y = log(data3$Total_tests/data3$Population*1000000,10), label = as.character(data3$iso3_name))  

p4 <- ggplot()+
  geom_point(aes(x=data4$Total_cases/data4$Total_tests,y=log(data4$Total_tests/data4$Population*1000000,10),color = data4$Region))
p4 + annotate("text", x = data4$Total_cases/data4$Total_tests, y = log(data4$Total_tests/data4$Population*1000000,10), label = as.character(data4$iso3_name))  

ggplot()+
  #geom_point(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_tests/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$iso3_name)))+
  geom_text(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_tests/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$iso3_name)),check_overlap = TRUE)
p + annotate("text", x = data1$Total_cases/data1$Total_tests, y = log(data1$Total_tests/data1$Population*1000000,10), label = as.character(data1$iso3_name))  

ggplot()+
  #geom_point(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_tests/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$iso3_name)))+
  geom_text(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_tests/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$iso3_name)),check_overlap = TRUE)+
  xlab('Tested Positive Rate')+
  ylab('Tests per Million Population')

ggplot()+
  #geom_point(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_tests/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$iso3_name)))+
  geom_text(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_cases/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$iso3_name)),check_overlap = TRUE)+
  xlab('Tested Positive Rate')+
  ylab('Confirmed Cases per Million Population')


ggplot()+
  #geom_point(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_tests/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$iso3_name)))+
  geom_text(aes(x=log(data1$Total_tests/data1$Population*1000000,10),y=log(data1$Total_cases/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$iso3_name)),check_overlap = TRUE)+
  xlab('Tests per Million Population')+
  ylab('Confirmed Cases per Million Population')

ggplot()+
  #geom_point(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_tests/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$iso3_name)))+
  geom_text(aes(x=data1$Total_death/data1$Total_cases,y=data1$Total_recover/data1$Total_cases,color = data1$Region, label = as.character(data1$iso3_name)),check_overlap = TRUE)+
  xlab('Fatality Rate')+
  ylab('Recovery Rate') 
library(showtext)
showtext_auto()
is_chn = T
ggplot()+
  #geom_point(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_tests/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$iso3_name)))+
  geom_text(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_tests/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$CN_names)),check_overlap = TRUE)+
  lha_theme(is_chn) +
  xlab('测试阳性率')+
  ylab('每百万人测试数')
  theme_bw(base_family = "wqy-microhei", base_size = 24)

ggplot()+
  #geom_point(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_tests/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$iso3_name)))+
  geom_text(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_cases/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$CN_names)),check_overlap = TRUE)+
  xlab('Tested Positive Rate')+
  ylab('Confirmed Cases per Million Population')


ggplot()+
  #geom_point(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_tests/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$iso3_name)))+
  geom_text(aes(x=log(data1$Total_tests/data1$Population*1000000,10),y=log(data1$Total_cases/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$CN_names)),check_overlap = TRUE)+
  xlab('Tests per Million Population')+
  ylab('Confirmed Cases per Million Population')

ggplot()+
  #geom_point(aes(x=data1$Total_cases/data1$Total_tests,y=log(data1$Total_tests/data1$Population*1000000,10),color = data1$Region, label = as.character(data1$iso3_name)))+
  geom_text(aes(x=data1$Total_death/data1$Total_cases,y=data1$Total_recover/data1$Total_cases,color = data1$Region, label = as.character(data1$CN_names)),check_overlap = TRUE)+
  xlab('Fatality Rate')+
  ylab('Recovery Rate') 



