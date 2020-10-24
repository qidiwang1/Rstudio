data <- read.csv('/Users/qidiwang1/Desktop/Data_mining.csv')
levels(data$education)
sum(data$education=="中专" )/nrow(data)*100
sum(data$education=="中技" )/nrow(data)*100
sum(data$education=="初中及以下" )/nrow(data)*100
sum(data$education=="博士" )/nrow(data)*100
sum(data$education=="大专" )/nrow(data)*100
sum(data$education=="本科" )/nrow(data)*100
sum(data$education=="硕士" )/nrow(data)*100
sum(data$education=="高中" )/nrow(data)*100
levels(data$companytype)
sum(data$companytype=="10000人以上")/nrow(data)*100
sum(data$companytype=="150-500人")/nrow(data)*100
sum(data$companytype=="500-1000人")/nrow(data)*100
sum(data$companytype=="上市公司")/nrow(data)*100
sum(data$companytype=="事业单位")/nrow(data)*100
sum(data$companytype=="创业公司")/nrow(data)*100
sum(data$companytype=="合资")/nrow(data)*100
sum(data$companytype=="国企")/nrow(data)*100
sum(data$companytype=="外企代表处")/nrow(data)*100
sum(data$companytype=="外资（欧美）")/nrow(data)*100
sum(data$companytype=="外资（非欧美）")/nrow(data)*100
sum(data$companytype=="少于50人")/nrow(data)*100
sum(data$companytype=="政府机关" )/nrow(data)*100
sum(data$companytype=="民营公司")/nrow(data)*100
sum(data$companytype=="非营利组织")/nrow(data)*100

levels(data$direction)
sum(data$direction=="专业服务(咨询、人力资源、财会)")/nrow(data)*100

Direction <- data.frame()
for (i in levels(data$direction)){
  a <- sum(data$direction==i)/nrow(data)*100
  Direction <- rbind(Direction,cbind(i,a))
}
Education <- data.frame()
for (i in levels(data$education)) {
  a <- sum(data$education==i)/nrow(data)*100
  Education <- rbind(Education,cbind(i,a))
}

number <- data.frame()
for (i in levels(data$number)) {
  a <- sum(data$number==i)/nrow(data)*100
  number <- rbind(number,cbind(i,a))
}
View(number)
View(Education)
write.csv(Direction,'/Users/qidiwang1/Desktop/Direction.csv')
write.csv(Education,'/Users/qidiwang1/Desktop/education.csv')
write.csv(number,'/Users/qidiwang1/Desktop/number.csv')

data$area <- as.character(data$area)

for (i in c(1:nrow(data))) {
  if (nchar(data$area[i])>3) {
    data$area[i] <- as.character(substr(data$area[i],1,2))} else {
      data$area[i] <- as.character(data$area[i])
      
    }
}

data$area <- as.factor(data$area)

area <- data.frame()
for (i in levels(as.factor(data$area))){
  a <- sum(data$area==i)/nrow(data)*100
  area <- rbind(area,cbind(i,a))
}
 write.csv(area, '/Users/qidiwang1/Desktop/area.csv')

direction_area <- data.frame()
for (i in levels(data$direction)){
  b <- data.frame()
  for (j in c(1:nrow(data))) {
    if (data[j,10]==i)
      b <- rbind(b,data[j,])
  }
  c <- data.frame()
  for (a in levels(b[,4])) {
    percent <- sum(b[,4]==a)/nrow(b)*100

    
    c <- rbind(c, cbind(i,a,percent))
  }
  direction_area <- rbind(direction_area,c)
  
}

write.csv(direction_area,'/Users/qidiwang1/Desktop/direction_area.csv')

direction_companytype <- data.frame()
for (i in levels(data$direction)){
  b <- data[data$direction==i,]
  c <- data.frame()
  for (a in levels(b[,9])) {
    percent<- sum(b[,9]==a)/nrow(b)*100
    
    
    c <- rbind(c, cbind(i,a,percent))
  }
  direction_companytype <- rbind(direction_companytype,c)
  
}
write.csv(direction_companytype,'/Users/qidiwang1/Desktop/direction_companytype.csv')

direction_number <- data.frame()
for (i in levels(data$direction)){
  b <- data[data$direction==i,]
  c <- data.frame()
  for (a in levels(b[,7])) {
    percent <- sum(b[,7]==a)/nrow(b)*100
    
    
    c <- rbind(c, cbind(i,a,percent))
  }
  direction_number <- rbind(direction_number,c)
  
}

write.csv(direction_number,'/Users/qidiwang1/Desktop/direction_number.csv')

direction_education <- data.frame()
for (i in levels(data$direction)){
  b <- data[data$direction==i,]
  c <- data.frame()
  for (a in levels(b[,6])) {
    percent <- sum(b[,6]==a)/nrow(b)*100
    
    
    c <- rbind(c, cbind(i,a,percent))
  }
  direction_education <- rbind(direction_education,c)
  
}

write.csv(direction_education,'/Users/qidiwang1/Desktop/direction_education.csv')


levels(data$experience)
experience <- data.frame()
for (i in levels(data$experience)) {
  a <- sum(data$experience==i)/nrow(data)*100
  experience <- rbind(experience,cbind(i,a))
}

write.csv(experience,'/Users/qidiwang1/Desktop/experience.csv')

direction_experience <- data.frame()
for (i in levels(data$direction)){
  b <- data[data$direction==i,]
  c <- data.frame()
  for (a in levels(b[,5])) {
    percent <- sum(b[,5]==a)/nrow(b)*100
    
    
    c <- rbind(c, cbind(i,a,percent))
  }
  direction_experience <- rbind(direction_experience,c)
  
}
write.csv(direction_experience,'/Users/qidiwang1/Desktop/direction_experience.csv')

area_direction <- data.frame()
for (i in levels(data$area)){
  b <- data[data$area==i,]
  c <- data.frame()
  for (a in levels(b[,10])) {
    percent <- sum(b[,10]==a)/nrow(b)*100
    
    
    c <- rbind(c, cbind(i,a,percent))
  }
  area_direction <- rbind(area_direction,c)
  
}
write.csv(area_direction,'/Users/qidiwang1/Desktop/area_direction.csv')

area_companytype <- data.frame()
for (i in levels(data$area)){
  b <- data[data$area==i,]
  c <- data.frame()
  for (a in levels(b[,9])) {
    percent <- sum(b[,9]==a)/nrow(b)*100
    
    
    c <- rbind(c, cbind(i,a,percent))
  }
  area_companytype <- rbind(area_companytype,c)
  
}
write.csv(area_companytype,'/Users/qidiwang1/Desktop/area_companytype.csv')
