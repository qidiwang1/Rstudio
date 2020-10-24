data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
View(data)
data$Date <- as.Date(data$Date,'%m/%d/%y')


library(ggplot2)
library(ggpubr)
data$confirm <- as.numeric(as.character(data$confirm))
data$Policy.Score <-as.numeric(as.character(data$Policy.Score))


BB <- data.frame()
for (a in levels(data$Country)) {
  country <- data[data$Country==a,]

  graph <- data.frame()
  if (nrow(country)>=7) 
    
    
    for (i in c(7:nrow(country))){
      if (max(country$Policy.Score==0)) {
        rescaled_score <- 0
      } else {
        if (is.na(country$Policy.Score[i]) == TRUE){
          rescaled_score <- 0
          
        } else {
          rescaled_score <- (country$Policy.Score[i])/(max(country$Policy.Score))*100
        }  
      }
      

  
      if (i == 7){
        confirm_growth_rate <- 0
      } else {
        calculated_confirm <- sum(country$New.cases[c((i-6):i)])/7
        pre_calculated_confirm <- sum(country$New.cases[c((i-7):(i-1))])/7
        if (pre_calculated_confirm==0){
          confirm_growth_rate<-0
        } else{
          confirm_growth_rate <- (calculated_confirm-pre_calculated_confirm)/pre_calculated_confirm*100
        }
        
      }
  
      if (i == 7){
        death_growth_rate <- 0
      } else {
        calculated_death <- sum(country$New.death[c((i-6):i)])/7
        pre_calculated_death <- sum(country$New.death[c((i-7):(i-1))])/7
        if (pre_calculated_death==0){
          death_growth_rate<-0
        } else{
          death_growth_rate <- (calculated_death-pre_calculated_death)/pre_calculated_death*100
        }
        
      }
  
      bb <- cbind(country$JDATE[i],country$confirm[i],rescaled_score, confirm_growth_rate,death_growth_rate, country$Policy.Score[i])
      #names(bb)[1] <- 'V1'
      #names(bb)[2] <- 'rescaled_score'
      #names(bb)[3] <- 'confirm_growth_rate'
      #names(bb)[4] <- 'rescaled_newcases'
      graph <- rbind(graph,cbind(bb,as.character(a)))
      #graph <- graph[graph$V2>50,]

  
      
    }
 # BB <- rbind(BB, graph)
#}

#write.csv(BB,'/Users/qidiwang1/Desktop/BB.csv')
#View(BB)





#graph
  #graph
  if (nrow(graph) > 0){
    
  
    graph$V1 <- as.numeric(as.character(graph$V1))
    graph$rescaled_score <- as.numeric(as.character(graph$rescaled_score))
    graph$confirm_growth_rate <- as.numeric(as.character(graph$confirm_growth_rate))
    graph$death_growth_rate <- as.numeric(as.character(graph$death_growth_rate))
    graph$V6 <- as.numeric(as.character(graph$V6))
  
  #graph
    #graph$V1 <- as.Date(graph$V1)
    plot1 <- ggplot()+
      geom_line(aes(x=graph$V1,y=graph$rescaled_score, color = 'Rescaled Score'))+
      geom_line(aes(x=graph$V1, y=graph$confirm_growth_rate, color = 'Cases Growth'))+
      xlab('Date')+
      ylab('')+
      theme(legend.title = element_blank())+
      #ylim(0,100)+
      ggtitle(a)
    plot2 <- ggplot()+
      geom_point(aes(x=graph$V6,y=graph$confirm_growth_rate))+
      xlab('Policy Score')+
      ylab('Cases Growth Rate')+
      theme(legend.title = element_blank(),legend.position = 'top')
      #ylim(0,100)
    plot3 <- ggplot()+
      geom_line(aes(x=graph$V1,y=graph$rescaled_score, color = 'Rescaled Score'))+
      geom_line(aes(x=graph$V1, y=graph$death_growth_rate, color = 'Death Growth'))+
      xlab('Date')+
      ylab('')+
      theme(legend.title = element_blank())
    plot4 <- ggplot()+
      geom_point(aes(x=graph$V6,y=graph$death_growth_rate))+
      xlab('Policy Score')+
      ylab('Death Growth Rate')+
      theme(legend.title = element_blank(),legend.position = 'top')
    figure <- ggarrange(plot1, plot2, plot3, plot4,ncol = 2 , nrow= 2)
    #ggsave(plot1, filename = paste('/Users/qidiwang1/Desktop/score vs new cases/',a,'.png'))
    ggsave(figure, filename = paste('/Users/qidiwang1/Desktop/平滑new cases/',a,'.png'))
  } else {print(c('You are not on the list', a))}
  
}

graph
