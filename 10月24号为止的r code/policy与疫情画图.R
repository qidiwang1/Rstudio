policy <- read.csv('/Users/qidiwang1/Desktop/policy.csv')
merge <- read.csv('/Users/qidiwang1/Desktop/merge.csv')
View(policy)
View(merge)

policy$DATE <- as.Date(policy$DATE,'%m/%d/%Y')
merge$Date <- as.Date(merge$Date,'%m/%d/%Y')

policy$DATE <- as.character(policy$DATE)
merge$Date <- as.character(merge$Date)
policy$POLICY2 <- as.numeric(as.character(policy$POLICY2))
levels(merge$Country)

typeof(policy$POLICY2)

policy_covid <- data.frame()
for (i in levels(merge$Country)) {
  country <- merge[merge$Country==i,]
  policyaaa <- policy[policy$COUNTRYNAME==i,]

  aaa <- data.frame()
  for (j in c(1:nrow(country))){
    if (nrow(policyaaa[which(policyaaa$DATE==country$Date[j]),])==1) {
      a <- cbind(POLICY2=as.character(policyaaa[which(policyaaa$DATE==country$Date[j]),][,5]),country[j,])

    } else {
      a <- cbind(data.frame(POLICY2=''),country[j,])
    }
    aaa <- rbind(aaa,a)

  }

  policy_covid <- rbind(policy_covid, aaa)

}



policy_covid$POLICY2 <- as.character(policy_covid$POLICY2)
policy_covid$POLICY2 <- as.numeric(policy_covid$POLICY2)

policy_covid2 <- policy_covid
policy_covid2$POLICY2 <- as.numeric(policy_covid2$POLICY2)



policy_covid2$Country <- factor(policy_covid2$Country)

for (i in c(1:nrow(policy_covid2))){
  if (is.na(policy_covid2$POLICY2[i])==TRUE)
    policy_covid2$POLICY2[i] <- 0
}

View(policy_covid2)

for (i in levels(policy_covid2$Country)) {
  a <- policy_covid2[policy_covid2$Country==i,]

  
  plot <- ggplot()+
    geom_point(aes(x=a$death,y=a$POLICY2))+
    
    ggtitle(i)+
    xlab('New Death per million')+
    ylab('Policy Rate')
  ggsave(plot,filename = paste('/Users/qidiwang1/Desktop/Death/',i,'.png'))
}



policy <- na.omit(policy)

