data <- read.csv('/Users/qidiwang1/Desktop/Book2.csv')
View(data)

library(ngramr)
information <- ngram('information', corpus = "eng_2012", year_start = 1800, year_end = 2004,
      smoothing = 0, count = FALSE, tag = NULL, case_ins = TRUE)
Information <- ngram('Information', corpus = "eng_2012", year_start = 1800, year_end = 2004,
                     smoothing = 0, count = FALSE, tag = NULL, case_ins = TRUE)
  
data <- ngram('data', corpus = "eng_2012", year_start = 1800, year_end = 2004,
           smoothing = 0, count = FALSE, tag = NULL, case_ins = TRUE)
Data <- ngram('Data', corpus = "eng_2012", year_start = 1800, year_end = 2004,
              smoothing = 0, count = FALSE, tag = NULL, case_ins = TRUE)
DATA <- ngram('DATA', corpus = "eng_2012", year_start = 1800, year_end = 2004,
              smoothing = 0, count = FALSE, tag = NULL, case_ins = TRUE)


google_books <- information %>%
  left_join(Information, by = c('Year','Corpus'))%>%
  left_join(data, by = c('Year','Corpus'))%>%
  left_join(Data, by = c('Year','Corpus'))%>%
  left_join(DATA, by = c('Year','Corpus'))

google_books
write.csv(google_books,'/Users/qidiwang1/Desktop/google_books.csv')
########




gc();rm(list = ls())
#setwd("/Users//Users/qidiwang1/Desktop/plot_codes/")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/helper.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/lha_theme.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/plot_functions.R")
data <- read.csv('/Users/qidiwang1/Desktop/google_books.csv')
worldgdp <- read.csv('/Users/qidiwang1/Desktop/隐私项目数据/worldgdp.csv')
library(smoother)
library(ggplot2)
max(worldgdp$X.int....)/max(data$sum_information)
is_chn = F
ggplot()+
  geom_line(aes(x=worldgdp$Year,y=worldgdp$X.int..../1.858974e+17, color = 'GDP'), size = 2)+
  geom_line(aes(x=data$Year, y = data$sum_information, color = 'information'),size = 1)+
  geom_line(aes(x=data$Year, y = data$sum_data, color = 'data'), size = 1)+
  xlab('Year')+
  ylab('')+
  lha_theme(is_chn) +
  scale_colour_manual(values = c('chartreuse3','#0072B2', 'grey'),limits = c('data', 'information', 'GDP'))+
  theme(legend.position = 'top', legend.title = element_blank())+
  scale_y_continuous(sec.axis = sec_axis(~ . * 1.858974e+17,name = 'GDP'))+
  geom_vline(xintercept = 1970, size = 0.8, color = "#990000", linetype = "dashed")
  ggtitle('"Information" or "Data" mentioned on the book each year as GDP Grows ')


