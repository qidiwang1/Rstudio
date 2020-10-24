data2 <- read_excel('/Users/qidiwang1/Desktop/小微企业调研/小微企业调研汇总表.xlsx')
cor(data2$Index.online,data2$)
names(data2)

library(corrplot)
par(family="FZLanTingHeiS-R-GB")
corrplot(cor(data2[,c('Index.online','Index.offline_lha','Index.offline_loan','New_cases(three months)','7月工业同比','7月产品销售率同比','7月亏损工业企业同比','7月累计平均用工人数同比','7月工业利润总额累计值同比')]), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

View(data2)
data <- data2[data2$Province!='新疆维吾尔自治区'&data2$Province!='北京市',]
ggplot()+
  geom_point(aes(x=data$Index.offline_lha,y=data$`New_cases(three months)`))
data$`New_cases(three months)`
linear <- lm(Index.online~Mobility,data=data)      
View(Europe2)
summary(linear)
data2$`第二季度零售总额累计增长%`
corrplot(cor(data[,c('Avg','Index.online','Index.offline_lha','Index.offline_loan','New_cases(three months)','7月工业同比','7月产品销售率同比','7月亏损工业企业同比','7月累计平均用工人数同比','7月工业利润总额累计值同比',
                     '第二季度GDP同比','第二季度零售总额累计增长%','Mobility')]), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
linear <- lm(Index.online~`7月亏损工业企业同比`+`7月累计平均用工人数同比`+
               `7月工业利润总额累计值同比`+`New_cases(three months)`,data=data)   

par(family="FZLanTingHeiS-R-GB")
data$Province
ggplot(data,aes(x=`7月工业同比`,y=Index.online))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('7月工业同比（%）')+
  ylab('线上调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))
ggplot(data,aes(x=`7月亏损工业企业同比`,y=Index.online))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('7月亏损工业企业同比（%）')+
  ylab('线上调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))
ggplot(data,aes(x=`7月累计平均用工人数同比`,y=Index.online))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('7月累计平均用工人数同比（%）')+
  ylab('线上调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))
ggplot(data,aes(x=`7月工业利润总额累计值同比`,y=Index.online))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('7月工业利润总额累计值同比（%）')+
  ylab('线上调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))

ggplot(data,aes(x=`New_cases(three months)`,y=Index.online))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('近三月新增感染人数')+
  ylab('线上调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))

ggplot(data,aes(x=`New_cases(three months)`,y=Index.offline_lha))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('近三月新增感染人数')+
  ylab('罗汉堂线下调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))
ggplot(data,aes(x=`New_cases(three months)`,y=Index.offline_loan))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('近三月新增感染人数')+
  ylab('网商银行线下调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))


ggplot(data,aes(x=`7月工业同比`,y=Index.offline_lha))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('7月工业同比（%）')+
  ylab('罗汉堂线下调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))

ggplot(data,aes(x=`7月工业同比`,y=Index.offline_loan))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('7月工业同比（%）')+
  ylab('网商银行线下调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))
ggplot(data,aes(x=`7月亏损工业企业同比`,y=Index.offline_lha))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('7月亏损工业企业同比（%）')+
  ylab('罗汉堂线下调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))

ggplot(data,aes(x=`7月亏损工业企业同比`,y=Index.offline_loan))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('7月亏损工业企业同比（%）')+
  ylab('网商银行线下调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))
ggplot(data,aes(x=`7月工业利润总额累计值同比`,y=Index.offline_lha))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('7月工业利润总额累计值同比（%）')+
  ylab('罗汉堂线下调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))

ggplot(data,aes(x=`7月工业利润总额累计值同比`,y=Index.offline_loan))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('7月工业利润总额累计值同比（%）')+
  ylab('网商银行线下调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))

ggplot(data,aes(x=`7月累计平均用工人数同比`,y=Index.offline_lha))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('7月累计平均用工人数同比（%）')+
  ylab('罗汉堂线下调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))

ggplot(data,aes(x=`7月累计平均用工人数同比`,y=Index.offline_loan))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="FZLanTingHeiS-R-GB")+
  xlab('7月累计平均用工人数同比（%）')+
  ylab('网商银行线下调研指数')+
  geom_smooth(se = FALSE,method = lm)+
  theme(plot.title=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        text=element_text(size=14, family="FZLanTingHeiS-R-GB"), 
        axis.text.x=element_text(family="FZLanTingHeiS-R-GB"))

gc();rm(list = ls())
#setwd("/Users//Users/qidiwang1/Desktop/plot_codes/")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/helper.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/lha_theme.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/plot_functions.R")
data2 <- read_excel('/Users/qidiwang1/Desktop/小微企业调研/小微企业调研汇总表.xlsx')
offline_lha <- read_excel('/Users/qidiwang1/Desktop/小微企业调研/offline_lha.xlsx')
offline_loan <- read_excel('/Users/qidiwang1/Desktop/小微企业调研/offline_loan.xlsx')
online <- read_excel('/Users/qidiwang1/Desktop/小微企业调研/online.xlsx')
merge <- data2%>%
  left_join(offline_lha,by='Province')%>%
  left_join(offline_loan,by='Province')%>%
  left_join(online,by='Province')

write_xlsx(merge,'/Users/qidiwang1/Desktop/小微企业调研汇总表.xlsx')

merge <- read_excel('/Users/qidiwang1/Desktop/小微企业调研汇总表.xlsx')
data <- merge[merge$Province!='Xinjiang',]
View(data)

corrplot(cor(data[,c(2:ncol(data))]), order = "hclust", 
         tl.col = "black", tl.srt = 45)

is_chn = F
p1 <- ggplot(data,aes(x=`Industrial production YoY (July)`,y=`Index (Offline Survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')


ggsave(p1,filename = '/Users/qidiwang1/Desktop/小微企业调研/罗汉堂/7月工业同比vs总指标.png',width = 16, height = 9)

p2 <- ggplot(data,aes(x=`Industrial production YoY (July)`,y=`Current economic situation (Offline survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p2,filename = '/Users/qidiwang1/Desktop/小微企业调研/罗汉堂/7月工业同比vs经济形势指标.png',width = 16, height = 9)

p3 <- ggplot(data,aes(x=`Industrial production YoY (July)`,y=`Economic outlook (Offline survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p3,filename = '/Users/qidiwang1/Desktop/小微企业调研/罗汉堂/7月工业同比vs预期指标.png',width = 16, height = 9)


p4 <- ggplot(data,aes(x=`Industrial production YoY (July)`,y=`Online situation (Offline survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')

ggsave(p4,filename = '/Users/qidiwang1/Desktop/小微企业调研/罗汉堂/7月工业同比vsonline指标.png',width = 16, height = 9)



corrplot(cor(data[,c('Index.online','current_situation.online','economic_outlook.online','online_situation.online','New_cases(three months)','7月工业同比','7月产品销售率同比','7月亏损工业企业同比','7月累计平均用工人数同比','7月工业利润总额累计值同比')]), order = "hclust", 
         tl.col = "black", tl.srt = 45)

p1 <- ggplot(data,aes(x=`Industrial production YoY (July)`,y=`Index (Offline Survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p1,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月工业同比vs总指标.png',width = 16, height = 9)

p2 <- ggplot(data,aes(x=`Industrial production YoY (July)`,y=`Current economic situation (Online survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p2,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月工业同比vs经济形势指标.png',width = 16, height = 9)

p3 <- ggplot(data,aes(x=`Industrial production YoY (July)`,y=`Economic outlook (Online survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p3,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月工业同比vs预期指标.png',width = 16, height = 9)


p4 <- ggplot(data,aes(x=`Industrial production YoY (July)`,y=`Online situation (Online survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')

ggsave(p4,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月工业同比vsonline指标.png',width = 16, height = 9)




p5 <- ggplot(data,aes(x=`Number of defective industrial companies YoY (July)`,y=`Index (Online Survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p5,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月亏损工业企业同比vs总指标.png',width = 16, height = 9)

p6 <- ggplot(data,aes(x=`Number of defective industrial companies YoY (July)`,y=`Current economic situation (Online survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p6,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月亏损工业企业同比vs经济形势指标.png',width = 16, height = 9)

p7 <- ggplot(data,aes(x=`Number of defective industrial companies YoY (July)`,y=`Economic outlook (Online survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p7,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月亏损工业企业同比vs预期指标.png',width = 16, height = 9)


p8 <- ggplot(data,aes(x=`Number of defective industrial companies YoY (July)`,y=`Online situation (Online survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p8,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月亏损工业企业同比vsonline指标.png',width = 16, height = 9)


p9 <- ggplot(data,aes(x=`Cumulative number of workers YoY (July)`,y=`Index (Online Survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p9,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月累计平均用工人数同比vs总指标.png',width = 16, height = 9)

p10 <- ggplot(data,aes(x=`Cumulative number of workers YoY (July)`,y=`Current economic situation (Online survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p10,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月累计平均用工人数同比vs经济形势指标.png',width = 16, height = 9)

p11 <- ggplot(data,aes(x=`Cumulative number of workers YoY (July)`,y=`Economic outlook (Online survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p11,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月累计平均用工人数同比vs预期指标.png',width = 16, height = 9)


p12 <- ggplot(data,aes(x=`Cumulative number of workers YoY (July)`,y=`Online situation (Online survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')

ggsave(p12,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月累计平均用工人数同比vsonline指标.png',width = 16, height = 9)



p13 <- ggplot(data,aes(x=`Cumulative industrial profit YoY (July)`,y=`Index (Online Survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p13,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月工业利润总额累计值同比vs总指标.png',width = 16, height = 9)

p14 <- ggplot(data,aes(x=`Cumulative industrial profit YoY (July)`,y=`Current economic situation (Online survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p14,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月工业利润总额累计值同比vs经济形势指标.png',width = 16, height = 9)

p15 <- ggplot(data,aes(x=`Cumulative industrial profit YoY (July)`,y=`Economic outlook (Online survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')
ggsave(p15,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月工业利润总额累计值同比vs预期指标.png',width = 16, height = 9)


p16 <- ggplot(data,aes(x=`Cumulative industrial profit YoY (July)`,y=`Online situation (Online survey)`))+
  geom_point()+
  geom_text(aes(label=factor(Province)),family="Verdana",color = '#13448c',vjust = -0.5)+
  lha_theme(is_chn)+
  theme(axis.text=element_text(family="Verdana",size = 14,color = '#13448c'),
        axis.title = element_text(family="FZLanTingHeiS-B-GB",size = 16,color = '#13448c'))+
  geom_smooth(se = FALSE,method = lm,linetype = 'dashed')

ggsave(p16,filename = '/Users/qidiwang1/Desktop/小微企业调研/线上问卷/7月工业利润总额累计值同比vsonline指标.png',width = 16, height = 9)
















