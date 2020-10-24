data <- read.csv('/Users/qidiwang1/Desktop/阿里/2018-2010/美国总体进口状况.csv')
data_china <- data[data$Partner.Name == 'China', c(2,3,6,7)]
data_china
data_canada <- data[data$Partner.Name == 'Canada', c(2,3,6,7)]
data_canada
data_mexico <- data[data$Partner.Name == 'Mexico', c(2,3,6,7)]
data_mexico

library(ggplot2)
ggplot() +
  geom_line(aes(x = data_china$Year, y = data_china$Import..US..Thousand., color = data_china$Partner.Name)) +
  geom_line(aes(x = data_canada$Year, y = data_canada$Import..US..Thousand., color = data_canada$Partner.Name)) +
  geom_line(aes(x = data_mexico$Year, y = data_mexico$Import..US..Thousand., color = data_mexico$Partner.Name)) +
  ggtitle('2010-2018 imports from China, Canada & Mexico') +
  scale_colour_manual("", 
                      breaks = c("China", "Canada", "Mexico"),
                      values = c("green", "red", "blue"))+
  xlab('Year') +
  ylab('Imports') 


ggplot() +
  geom_line(aes(x = data_china$Year, y = data_china$Share...., color = data_china$Partner.Name)) +
  geom_line(aes(x = data_canada$Year, y = data_canada$Share...., color = data_canada$Partner.Name)) +
  geom_line(aes(x = data_mexico$Year, y = data_mexico$Share...., color = data_mexico$Partner.Name)) +
  ggtitle('2010-2018 imports share from China, Canada & Mexico') +
  scale_colour_manual("", 
                      breaks = c("China", "Canada", "Mexico"),
                      values = c("green", "red", "blue"))+
  xlab('Year') +
  ylab('Imports share')