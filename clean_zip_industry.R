setwd('/mnt/hgfs/CyberRecon/learn_machinely/air_bnb')

df <- read.csv("zbp15detail.txt")
head(df)
df2 <- df[which(as.character(df$naics)=="722511"),]
df2 <- df2[,c('zip','est')]
write.csv(df2,"food_industry.csv")
