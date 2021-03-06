# Set directory
setwd('/run/media/renge/Gamma/Documents/AirBNB')

source("silent_score.R")

# Load libraries
library(randomForest)
library(magrittr)
library(dplyr)
library(sentimentr)
#library(ggmap)

# Load data
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

# Subset External Data
# out <- revgeocode(c(lon,lat),output='all')
# out$results[[3]]$address_components[[7]]$long_name gives zip code

cities <- c('Los Angeles, CA','New York, NY','Boston, MA','Chicago, IL','San Francisco, CA','Washington, DC')
sub_median <- median_home[which(median_home$RegionName %in% cities),c('RegionName','X2017.07')]
sub_median$city <- sapply(sub_median$RegionName, function(x) full_to_city(x))


# Custom columns
#all_data$desc_score <- sapply(all_data$description, function(x) mean(sentiment(get_sentences(as.character(x)))$sentiment) )
all_data$n_amenities <- sapply(all_data$amenities, function(x) length(unlist(strsplit(as.character(x),','))) )
all_data <- merge(all_data, sub_median, by='city',all.x=TRUE)
all_data$clean_zip <- sapply(all_data$zipcode, function(x) as.numeric(strtrim(unique(as.character(x)),5)) )
all_data <- merge(all_data, food_ind, by.x='clean_zip',by.y='zip',all.x=TRUE)

all_data$X2017.07 <- sapply(all_data$X2017.07, function(x) log(x))
all_data$host_response_rate <- sapply(all_data$host_response_rate, function(x) un_percent(x) )
all_data$desc_score <- all_desc_score

# Select a subset of the data

keep_cols <- c('property_type','cancellation_policy',
               'bathrooms', 'number_of_reviews','host_response_rate',
               'review_scores_rating','log_price',
               'desc_score','est',
               'X2017.07','n_amenities')

all_data <- all_data[,keep_cols]

# Impute missing values with 0

fillna <- function(column) {
  column[is.na(column)] <- 0
  return(column)
}

#all_data$X2017.07 <- sapply(all_data$X2017.07, function(x) log(x))
col_type <- sapply(all_data,class)
numeric_type <- !(col_type %in% c("character","factor"))
all_data[,numeric_type] <- sapply(all_data[,numeric_type], fillna)

# Train a Random Forest model with cross-validation

cv_folds <- sample(1:3, size = nrow(train), replace = TRUE)

for(i in 1:1) {
  # Train the model using the training sets
  fit <- randomForest(log_price ~ .,
                      data = all_data[train_set[cv_folds !=i],],
                      ntree = 500, importance=TRUE)
  
  # Make predictions using the testing set
  preds <- predict(fit, all_data[train_set[cv_folds == i],])
  
  # Calculate RMSE for current cross-validation split
  print(mean((preds - all_data[train_set[cv_folds == i],'log_price'])^2)^.5)
  #[1] 0.4128166
}
importance(fit)
# Create submission file

fit <- randomForest(log_price ~ ., data = all_data[train_set,], ntree = 1)
prediction <- predict(fit, all_data[test_set,])

#sample_submission <- data.frame(id = ids, log_price = prediction)
#write.csv(sample_submission, "sample_submission.csv", row.names = FALSE)

