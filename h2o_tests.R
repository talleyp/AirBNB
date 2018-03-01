library(h2o)
source("silent_score.R")

setwd('/run/media/renge/Gamma/Documents/AirBNB')

# Load in Data
path = "data/"
train <- read.csv(paste0(path,"train.csv"),stringsAsFactors = TRUE)
test <- read.csv(paste0(path,"test.csv"), stringsAsFactors = TRUE)

median_home <- read.csv(paste0(path,"Sale_Prices_Msa.csv"))
food_ind <- read.csv(paste0(path,'food_industry.csv'))
food_ind <- food_ind[,c('zip','est')]
all_desc_score <- read.csv(paste0(path,'desc_scores.csv'))$x

ids = test$id
test$log_price <- NA

all_data <- rbind(train,test)
train_set = 1:nrow(train)
test_set <- (nrow(train)+1):(nrow(train) + nrow(test))

# Clean Data
cities <- c('Los Angeles, CA','New York, NY','Boston, MA','Chicago, IL','San Francisco, CA','Washington, DC')
sub_median <- median_home[which(median_home$RegionName %in% cities),c('RegionName','X2017.07')]
sub_median$city <- sapply(sub_median$RegionName, function(x) full_to_city(x))

# Custom columns
all_data$n_amenities <- sapply(all_data$amenities, function(x) length(unlist(strsplit(as.character(x),','))) )
all_data <- merge(all_data, sub_median, by='city',all.x=TRUE)
all_data$clean_zip <- sapply(all_data$zipcode, function(x) as.numeric(strtrim(unique(as.character(x)),5)) )
all_data <- merge(all_data, food_ind, by.x='clean_zip',by.y='zip',all.x=TRUE)

all_data$X2017.07 <- sapply(all_data$X2017.07, function(x) log(x))
all_data$host_response_rate <- sapply(all_data$host_response_rate, function(x) un_percent(x) )
all_data$desc_score <- all_desc_score


