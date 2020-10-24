data <- read.csv('/Users/qidiwang1/Desktop/glindex.csv')
write.csv(data,'/Users/qidiwang1/Desktop/glindex1.csv')
View(data)
df_60 <- c()
df_84 <- c()
df_85 <- c()
df_87 <- c()
levels(data$industry_code)
for (i in c(1:nrow(data))) {
  if (data[i,3]=='61+62') 
    df_60 <- rbind(df_60,data[i,])
  else if (data[i,3]=='84') 
    df_84 <- rbind(df_84, data[i,])
  else if (data[i,3]=='85') 
    df_85 <- rbind(df_85, data[i,])
  else if (data[i,3]=='87') 
    df_87 <- rbind(df_87, data[i,])  
}

df_60
df_84
df_85
df_87

paste('type',3)


split <- function(df) {
  df1 <- c()
  df2 <- c()
  df3 <- c()
  for (i in c(1:nrow(df))) {
    if (df[i,2] == "ASEAN"|df[i,2]=="Brazil"|df[i,2]=="China"|df[i,2]=="EU-28"|
        df[i,2] == "France")
      df1 <- rbind(df1,df[i,])
    else if (df[i,2] == "Germany"|df[i,2] == "Italy"| df[i,2] == "Japan"|
             df[i,2] == "Mexico"| df[i,2] == "Rep. of Korea")
      df2 <- rbind(df2,df[i,])
    else if (df[i,2] == "Spain"| df[i,2]=="Switzerland"| 
             df[i,2] == "United Kingdom"| df[i,2]=="USA")
      df3 <- rbind(df3,df[i,])
  }
  graph1 <- ggplot() +
    geom_line(aes(x = df1$year, y = df1$gl_index, color = df1$country)) +
    ylim(c(0,1)) +
    ylab('Grubel-Llyod Index') +
    xlab('Year') +
    ggtitle(paste('Grubel-Llyod Index vs Year'),subtitle = paste('(','Type',df[1,3],')'))+
    theme(legend.title = element_blank())+
    font("xy.text", size = 10) +
    
    bbc_style() +
    theme(plot.title = element_text(color = "#063376",size = 24)) +
    theme(plot.subtitle = element_text(size = 14)) +
    theme(legend.text = element_text(size = 10)) +
    theme(legend.justification = 'center') +
    theme(legend.position = 'bottom')
  
  graph2 <- ggplot() +
    geom_line(aes(x = df2$year, y = df2$gl_index, color = df2$country)) +
    ylim(c(0,1)) +
    ggtitle(paste('Grubel-Llyod Index vs Year'),subtitle = paste('(','Type',df[1,3],')'))+
    ylab('Grubel-Llyod Index') +
    xlab('Year')+
    theme(legend.title = element_blank())+
    font("xy.text", size = 10) +
    
    bbc_style() +
    theme(plot.title = element_text(color = "#063376",size = 24)) +
    theme(plot.subtitle = element_text(size = 14)) +
    theme(legend.text = element_text(size = 10)) +
    theme(legend.justification = 'center') +
    theme(legend.position = 'bottom')
  
  graph3 <-  ggplot() +
    geom_line(aes(x = df3$year, y = df3$gl_index, color = df3$country)) +
    ylim(c(0,1)) +
    ggtitle(paste('Grubel-Llyod Index vs Year'),subtitle = paste('(','Type',df[1,3],')'))+
    ylab('Grubel-Llyod Index') +
    xlab('Year')+
    theme(legend.title = element_blank())+
    font("xy.text", size = 10) +
    
    bbc_style() +
    theme(plot.title = element_text(color = "#063376",size = 24)) +
    theme(plot.subtitle = element_text(size = 14)) +
    theme(legend.text = element_text(size = 10)) +
    theme(legend.justification = 'center') +
    theme(legend.position = 'bottom')
  
  return(list(graph1,graph2,graph3))
}
split(df_60)
split(df_87)

df_60
df_84
df_85
df_87

finalise_plot(p, source_name = "Source: Kaggle", 
              save_filepath = "base_bbplot.png",
              width_pixels = 640, height_pixels = 450,
              logo_image_path = "pokemongo_logo.png")
df <- data[1,]

for (i in c(2:nrow(data))){
  if (data[i,3] == data[i-1,3])
    df <- rbind(df,data[i,])
  else
    cbind(df)
}
df

library(ggplot2)
library(bbplot)
library(ggpubr)
ggplot() +
  geom_line(aes(x = df$year, y = df$gl_index, color = df$country))