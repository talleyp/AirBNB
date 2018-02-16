setwd('/mnt/hgfs/CyberRecon/learn_machinely/air_bnb')
path = "data/"
zpath="data/zillow/"

#train <- read.csv(paste0(path,"train.csv"),stringsAsFactors = TRUE)
#test <- read.csv(paste0(path,"test.csv"), stringsAsFactors = TRUE)
#ids = test$id
#test$log_price <- NA
#all_data <- rbind(train,test)

#data_zips <- unique(sapply(all_data$zipcode, function(x) as.numeric(strtrim(unique(as.character(x)),5)) ))
#write.csv(data_zips,paste(path,'zips.csv'))
data_zips <- read.csv(paste(path,"zips.csv"))$x

zip_time <- read.csv(paste0(zpath,"Zip_time_series.csv"))
