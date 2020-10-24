gc();rm(list = ls())
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/helper.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/lha_theme.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/plot_functions.R")

data <- read.csv('/Users/qidiwang1/Desktop/tomorrow.csv')

data$confirm <- as.numeric(as.character(data$confirm))
data$death <- as.numeric(as.character(data$death))
data$New.cases.x <- as.numeric(as.character(data$New.cases.x))
data$New.death <- as.numeric(as.character(data$New.death))

levels(data$sub.region)

africa <- data[data$sub.region=='Eastern Asia'&data$Country.x!='China'&data$Country.x!='Hubei'&data$Country.x!='China_Exhubei',]

africa$Country.x <- factor(africa$Country.x)
africa$Date <- factor(africa$Date)


africa_total <- data.frame()
for (a in levels(africa$Date)) {
  country <- africa[africa$Date==a,]
  africa_total <- rbind(africa_total,
                         cbind(death = sum(country$death),
                               confirm = sum(country$confirm),
                               new.case = sum(country$New.cases.x),
                               new.death = sum(country$New.death),
                               fatality.rate = sum(country$death)/sum(country$confirm),
                               confirm.per.million = sum(country$confirm)/sum(country$Population),
                               death.per.million = sum(country$death)/sum(country$Population),
                               Date = a))
}

africa_total$Date <- as.Date(africa_total$Date,'%m/%d/%y')
africa_total$confirm <- as.numeric(as.character(africa_total$confirm))
africa_total$death <- as.numeric(as.character(africa_total$death))
africa_total$fatality.rate <- as.numeric(as.character(africa_total$fatality.rate))
africa_total <- africa_total[africa_total$confirm>0,]

africaaa 
southasia
latin 
eastasia 
eastasia <- eastasia[c(1:(nrow(eastasia)-1)),]
write.csv(africaaa,'/Users/qidiwang1/Desktop/4 c/africa.csv')
write.csv(southasia,'/Users/qidiwang1/Desktop/4 c/southasia.csv')
write.csv(latin,'/Users/qidiwang1/Desktop/4 c/latin.csv')
write.csv(eastasia,'/Users/qidiwang1/Desktop/4 c/eastasia.csv')
is_chn = F
US <- data[data$Country.x=='United_States_of_America',]
US$Date <- as.Date(US$Date,'%m/%d/%y')
write.csv(US,'/Users/qidiwang1/Desktop/4 c/US.csv')
ggplot()+
  geom_line(aes(x=africaaa$Date,y=africaaa$death,color = 'Africa'))+
  geom_line(aes(x=southasia$Date, y = southasia$death, color = 'Southern Asia'))+
  geom_line(aes(x=latin$Date, y= latin$death, color = 'Latin America and the Caribbean'))+
  geom_line(aes(x=eastasia$Date, y = eastasia$death, color = 'Eastern Asia'))+
  geom_line(aes(x=US$Date,y=US$death,color='United States of America'))+

  theme(legend.title = element_blank())+
  lha_theme(is_chn)+
  xlab('Date')+
  ylab('Total Death')


is_chn = F
ggplot()+
  geom_line(aes(x=africa_total$Date,y=africa_total$confirm,color = 'confirm'))+
  geom_line(aes(x=africa_total$Date,y=africa_total$death,color = 'death'))+
  theme(legend.title = element_blank())+
  lha_theme(is_chn)+
  xlab('Date')+
  ylab('Population')+
  ggtitle('Latin America and the Caribbean')
ggplot()+
  geom_line(aes(x=africa_total$Date,y=africa_total$fatality.rate))+
  lha_theme(is_chn)+
  xlab('Date')+
  ylab('Fatality Rate')+
  ggtitle('Latin America and the Caribbean')


############
data$Date <- as.Date(data$Date,'%m/%d/%y')
china <- data[data$Country.x=='China',]

india <- data[data$Country.x=='India' & data$Date>='2020-01-12' & data$Date<='2020-05-05',]
brazil <- data[data$Country.x=='Brazil' & data$Date>='2020-01-12' & data$Date<='2020-05-05',]
sf <- data[data$Country.x=='South_Africa' & data$Date>='2020-01-12' & data$Date<='2020-05-05',]
russia <- data[data$Country.x=='Russia' & data$Date>='2020-01-12' & data$Date<='2020-05-05',]

ggplot() +
  geom_area(aes(x=russia$Date, y = china$confirm+sf$confirm+brazil$confirm+india$confirm+russia$confirm,fill = 'Russia'), size = 1) +
  geom_area(aes(x=india$Date, y = china$confirm+sf$confirm+brazil$confirm+india$confirm, fill = 'India'), size = 1) +
  geom_area(aes(x=brazil$Date, y = china$confirm+sf$confirm+brazil$confirm, fill = 'Brazil'), size = 1) +
  geom_area(aes(x=sf$Date, y = china$confirm+sf$confirm, fill = 'South Africa'), size = 1) +
  geom_area(aes(x=china$Date, y = china$confirm, fill =  'China'), size = 1) +
  lha_theme(is_chn) +
  scale_fill_manual(
    values = c("yellow", 
               "red", 
               "green",
               "blue",
               "orange")) +
  theme(legend.title = element_blank())+
  ylab('Total Confirmed Cases')+
  
  xlab('Date') 

ggplot() +
  geom_area(aes(x=russia$Date, y = china$death+sf$death+brazil$death+india$death+russia$death,fill = 'Russia'), size = 1) +
  geom_area(aes(x=india$Date, y = china$death+sf$death+brazil$death+india$death, fill = 'India'), size = 1) +
  geom_area(aes(x=brazil$Date, y = china$death+sf$death+brazil$death, fill = 'Brazil'), size = 1) +
  geom_area(aes(x=sf$Date, y = china$death+sf$death, fill = 'South Africa'), size = 1) +
  geom_area(aes(x=china$Date, y = china$death, fill =  'China'), size = 1) +
  lha_theme(is_chn) +
  scale_fill_manual(
    values = c("yellow", 
               "red", 
               "green",
               "blue",
               "orange")) +
  theme(legend.title = element_blank())+
  ylab('Total Death')+
  
  xlab('Date')   

ggplot() +
  geom_line(aes(x=russia$Date, y = russia$death/russia$confirm,color = 'Russia'), size = 1) +
  geom_line(aes(x=india$Date, y = india$death/india$confirm, color = 'India'), size = 1) +
  geom_line(aes(x=brazil$Date, y = brazil$death/brazil$confirm, color = 'Brazil'), size = 1) +
  geom_line(aes(x=sf$Date, y = sf$death/sf$confirm, color = 'South Africa'), size = 1) +
  geom_line(aes(x=china$Date, y = china$death/china$confirm, color =  'China'), size = 1) +
  lha_theme(is_chn) +
  scale_color_manual(
    values = c("yellow", 
               "red", 
               "green",
               "blue",
               "orange")) +
  theme(legend.title = element_blank())+
  ylab('Fatality Rate')+
  
  xlab('Date') 

ggplot() +
  geom_line(aes(x=c(1:nrow(russia[russia$confirm>=100,])), y = russia[russia$confirm>=100,]$rt.y,color = 'Russia'), size = 1) +
  geom_line(aes(x=c(1:nrow(india[india$confirm>=100,])), y = india[india$confirm>=100,]$rt.y, color = 'India'), size = 1) +
  geom_line(aes(x=c(1:nrow(brazil[brazil$confirm>=100,])), y = brazil[brazil$confirm>=100,]$rt.y, color = 'Brazil'), size = 1) +
  geom_line(aes(x=c(1:nrow(sf[sf$confirm>=100,])), y = sf[sf$confirm>=100,]$rt.y, color = 'South Africa'), size = 1) +
  geom_line(aes(x=c(1:nrow(china[china$confirm>=100,])), y = china[china$confirm>=100,]$rt.y, color =  'China'), size = 1) +
  lha_theme(is_chn) +
  scale_color_manual(
    values = c("yellow", 
               "red", 
               "green",
               "blue",
               "orange")) +
  theme(legend.title = element_blank())+
  ylab('R0')+
  
  xlab('Date Starting From 100 Confirmed Cases') 

mexico <- data[data$Country.x=='Mexico'&data$Date>='2020-02-28',]
indonesia <- data[data$Country.x=='Indonesia'&data$Date>='2020-02-28',]
turkey <- data[data$Country.x=='Turkey'&data$Date>='2020-02-28',]
nigeria <- data[data$Country.x=='Nigeria'&data$Date>='2020-02-28',]

ggplot() +
  geom_area(aes(x=mexico$Date, y = nigeria$confirm+turkey$confirm+indonesia$confirm+mexico$confirm,fill = 'Mexico'), size = 1) +
  geom_area(aes(x=indonesia$Date, y = nigeria$confirm+turkey$confirm+indonesia$confirm, fill = 'Indonesia'), size = 1) +
  geom_area(aes(x=turkey$Date, y = nigeria$confirm+turkey$confirm, fill = 'Turkey'), size = 1) +
  geom_area(aes(x=nigeria$Date, y = nigeria$confirm, fill = 'Nigeria'), size = 1) +

  lha_theme(is_chn) +
  scale_fill_manual(
    values = c("blue", 
               "green", 
               "red",
               "yellow")) +
  theme(legend.title = element_blank())+
  ylab('Total Confirmed Cases')+
  
  xlab('Date') 

ggplot() +
  geom_area(aes(x=mexico$Date, y = nigeria$death+turkey$death+indonesia$death+mexico$death,fill = 'Mexico'), size = 1) +
  geom_area(aes(x=indonesia$Date, y = nigeria$death+turkey$death+indonesia$death, fill = 'Indonesia'), size = 1) +
  geom_area(aes(x=turkey$Date, y = nigeria$death+turkey$death, fill = 'Turkey'), size = 1) +
  geom_area(aes(x=nigeria$Date, y = nigeria$death, fill = 'Nigeria'), size = 1) +
  
  lha_theme(is_chn) +
  scale_fill_manual(
    values = c("blue", 
               "green", 
               "red",
               "yellow")) +
  theme(legend.title = element_blank())+
  ylab('Total Death')+
  
  xlab('Date')   

ggplot() +
  geom_line(aes(x=mexico$Date, y = mexico$death/mexico$confirm,color = 'Mexico'), size = 1) +
  geom_line(aes(x=indonesia$Date, y = indonesia$death/indonesia$confirm, color = 'Indonesia'), size = 1) +
  geom_line(aes(x=turkey$Date, y = turkey$death/turkey$confirm, color = 'Turkey'), size = 1) +
  geom_line(aes(x=nigeria$Date, y = nigeria$death/nigeria$confirm, color = 'Nigeria'), size = 1) +
 
  lha_theme(is_chn) +
  scale_color_manual(
    values = c("blue", 
               "green", 
               "red",
               "yellow")) +
  theme(legend.title = element_blank())+
  ylab('Fatality Rate')+
  
  xlab('Date') 

ggplot() +
  geom_line(aes(x=c(1:nrow(mexico[mexico$confirm>=100,])), y = mexico[mexico$confirm>=100,]$rt.y,color = 'Mexico'), size = 1) +
  geom_line(aes(x=c(1:nrow(indonesia[indonesia$confirm>=100,])), y = indonesia[indonesia$confirm>=100,]$rt.y, color = 'Indonesia'), size = 1) +
  geom_line(aes(x=c(1:nrow(turkey[turkey$confirm>=100,])), y = turkey[turkey$confirm>=100,]$rt.y, color = 'Turkey'), size = 1) +
  geom_line(aes(x=c(1:nrow(nigeria[nigeria$confirm>=100,])), y = nigeria[nigeria$confirm>=100,]$rt.y, color = 'Nigeira'), size = 1) +
  lha_theme(is_chn) +
  scale_color_manual(
    values = c("blue", 
               "green", 
               "red",
               "yellow")) +
  theme(legend.title = element_blank())+
  ylab('R0')+
  
  xlab('Date Starting From 100 Confirmed Cases') 



