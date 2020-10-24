gc();rm(list = ls())
#setwd("/Users//Users/qidiwang1/Desktop/plot_codes/")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/helper.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/lha_theme.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/plot_functions.R")


data <- read.csv('/Users/qidiwang1/Desktop/Trade Volume.csv')
View(data)


is_chn = FALSE

ggplot()+
  geom_col(aes(x=data$Date,y=data$sum,fill='Intraregional'))+
  geom_col(aes(x=data$Date,y=data$Interregional,fill='Interregional'))+
  scale_x_continuous(name = "", breaks = seq(2005, 2014, by = 1))+
  theme(legend.title = element_blank())+
  xlab('Year')+
  ylab('Thousands of Gigabits per Second')+
  lha_theme(is_chn)+
  scale_fill_manual(values = c("#999999","#56B4E9"))+
  ggtitle('Total Used Cross-boarder Bandwidth')

data <- read.csv('/Users/qidiwang1/Desktop/Google search.csv')
ggplot()+
  geom_line(aes(x=data$X,y=data$Google.searches.per.year),size = 1, color = "#56B4E9")+
  scale_x_continuous(name = "", breaks = seq(2000, 2012, by = 1))+
  #xlab('Year')+
  ylab('Queries')+
  lha_theme(is_chn)+
  ggtitle('The Number of Google Searches per Year')

data <- read.csv('/Users/qidiwang1/Desktop/US Internet.csv')
data$Date <- as.Date(data$Date,'%d/%m/%y')
View(data)
ggplot()+
  geom_line(aes(x=data$Date,y=data$MEGABITES.PER.SECOND), size = 1, color = "#56B4E9")+
  ylab('Megabits per Second')+

  scale_x_date(name = '',date_breaks = "1 year",date_labels = '%Y') +
  lha_theme(is_chn)+
  ggtitle('Average Internet Connection Speed in the US')






