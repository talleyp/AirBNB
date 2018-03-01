library(ggmap)

setwd('/run/media/renge/Gamma/Documents/AirBNB')

path = "data/"
zpath="data/zillow/"

train <- read.csv(paste0(path,"train.csv"),stringsAsFactors = TRUE)
test <- read.csv(paste0(path,"test.csv"), stringsAsFactors = TRUE)
ids = test$id
test$log_price <- NA
all_data <- rbind(train,test)

#data_zips <- unique(sapply(all_data$zipcode, function(x) as.numeric(strtrim(unique(as.character(x)),5)) ))
#write.csv(data_zips,paste(path,'zips.csv'))
#coords <- paste(all_data$longitude, all_data$latitude)
gf <- function(x,y){
  out <- tryCatch({
    revgeocode(c(x,y),output="more",messaging=FALSE,override_limit=TRUE)$postal_code
  }, error=function(e){
    return(NA)
  }, warning=function(w){
    return(NA)
  })
  return(out)
}
sub1 <- all_data[1:20000,]
geo_zip_out1 <- mapply(function(x,y) gf(x,y), sub1$longitude,sub1$latitude)
save(geo_zip_out1,file='data/geo_1-20.Rdata')

sub2 <- all_data[20001:40000,]
geo_zip_out2 <- mapply(function(x,y) gf(x,y), sub2$longitude,sub2$latitude)
#geo_zip_out2 <- lapply(geo_zip_out2, as.numeric)
save(geo_zip_out2,file='data/geo_20-40.Rdata')

sub3 <- all_data[40001:60000,]
geo_zip_out3 <- mapply(function(x,y) gf(x,y), sub3$longitude,sub3$latitude)
#geo_zip_out3 <- lapply(geo_zip_out3, as.numeric)
save(geo_zip_out3,file='data/geo_40-60.Rdata')

sub4 <- all_data[60001:80000,]
geo_zip_out4 <- mapply(function(x,y) gf(x,y), sub4$longitude,sub4$latitude)
#geo_zip_out4 <- lapply(geo_zip_out4, as.numeric)
save(geo_zip_out4,file='data/geo_60-80.Rdata')

sub5 <- all_data[80001:99569,]
geo_zip_out5 <- mapply(function(x,y) gf(x,y), sub5$longitude,sub5$latitude)
#geo_zip_out5 <- lapply(geo_zip_out5, as.numeric)
save(geo_zip_out5,file='data/geo_80-end.Rdata')

geo_all <- c(geo_zip_out1,geo_zip_out2,geo_zip_out3,geo_zip_out4,geo_zip_out5)
geo_lvls <- sapply(geo_all, levels)
geo_clean <- sapply(geo_lvls[], function(x) as.numeric(x))
save(geo_clean,file="data/geo_all.Rdata")

