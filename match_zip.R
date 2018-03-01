setwd('/run/media/renge/Gamma/Documents/AirBNB')

# Load libraries
library(dplyr)
library(randomForest)

# Load Data
source("silent_score.R")
path = "data/"
zpath="data/zillow/Zip/"
#cpath="data/crime/"

train <- read.csv(paste0(path,"train.csv"),stringsAsFactors = TRUE)
test <- read.csv(paste0(path,"test.csv"), stringsAsFactors = TRUE)
load(file = paste0(path,"RegionName.Rdata"))
z1br <- read.csv(paste0(zpath,"Zip_MedianListingPrice_1Bedroom.csv"))
zindx <- read.csv(paste0(zpath,"BuyerSellerIndex_Zip.csv"))
food_ind <- read.csv(paste0(path,'food_industry.csv'))
all_desc_score <- read.csv(paste0(path,'desc_scores.csv'))$x

# Join Data
zip_data <- left_join(found_zips,z1br,by="RegionName")
zip_data <- left_join(zip_data,zindx,by="RegionName")
zip_data <- zip_data[,c("RegionName","SizeRank","X2017.06","CBSA.Title","SizeRankCity","SizeRankMetro","PctPriceCut","DaysOnMarket","BuyerSellerIndex","BuyerSellerIndexMetro","SizeRank")]

food_ind <- food_ind[,c('zip','est')]
names(food_ind) <- c("RegionName","num_est")
zip_data <- left_join(zip_data,food_ind,by='RegionName')

ids = test$id
test$log_price <- NA
all_data <- rbind(train,test)
train_set = 1:nrow(train)
test_set <- (nrow(train)+1):(nrow(train) + nrow(test))

# Create Data
all_data <- cbind(all_data,zip_data)
all_data$n_amenities <- sapply(all_data$amenities, function(x) length(unlist(strsplit(as.character(x),','))) )

all_data$X2017.06 <- sapply(all_data$X2017.06, function(x) log(x))
all_data$host_response_rate <- sapply(all_data$host_response_rate, function(x) un_percent(x) )
all_data$desc_score <- all_desc_score

# Subset All Data
keep_cols <- c('property_type','cancellation_policy',
               'bathrooms', 'number_of_reviews','host_response_rate',
               'review_scores_rating','log_price',"host_identity_verified",
               'desc_score',"accommodates","SizeRankMetro",
               "cleaning_fee","PctPriceCut","DaysOnMarket","BuyerSellerIndexMetro",
               'X2017.06','n_amenities',"BuyerSellerIndex")

sub_data <- all_data[,keep_cols]

# Impute missing values with 0
fillna <- function(column) {
  column[is.na(column)] <- 0
  return(column)
}
col_type <- sapply(sub_data,class)
numeric_type <- !(col_type %in% c("character","factor"))
sub_data[,numeric_type] <- sapply(sub_data[,numeric_type], fillna)

# Train a Random Forest model with cross-validation
cv_folds <- sample(1:3, size = nrow(train), replace = TRUE)
for(i in 1:1) {
  # Train the model using the training sets
  fit <- randomForest(log_price ~ .,
                      data = sub_data[train_set[cv_folds !=i],],
                      ntree = 500, importance=TRUE)
  
  # Make predictions using the testing set
  preds <- predict(fit, sub_data[train_set[cv_folds == i],])
  
  # Calculate RMSE for current cross-validation split
  print(mean((preds - sub_data[train_set[cv_folds == i],'log_price'])^2)^.5)
  #[1] 0.4128166
}
importance(fit)
# Create submission file

fit <- randomForest(log_price ~ ., data = sub_data[train_set,], ntree = 1)
prediction <- predict(fit, sub_data[test_set,])
sample_submission <- data.frame(id = ids, log_price = prediction)
write.csv(sample_submission, "sample_submission.csv", row.names = FALSE)