library(tidyverse)
library(leaps)
library(caret)
library(ggthemes)
library(glmnet)
library(ranger)
library(caTools)
library(ggplot2)
library(lattice)
library(caret)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(stringr)
library(zipcodeR)
library(gbm)



setwd('D:/Columbia/Spring 2021/APAN Frameworks and methods 1/Session 7 - Kaggle')


analysisData = read.csv(file = 'analysisData.csv', stringsAsFactors = F)
scoringData = read.csv(file = 'scoringData.csv', stringsAsFactors = F)

# Add the price column to scoringData because it is not included. Note = we are setting the value to 0

# Create a column name called price
# Add the column above to the scoringData with a zero value
# Reorder the scoringData dataset to be id, price, then all everything else that isn't id and price

namevector <- c("price")
scoringData[,namevector] <- 0
scoringData <- scoringData %>%
  select(id,price, everything())

all_equal(analysisData, scoringData, convert = TRUE)




analysisData$zipcode <- as.character(analysisData$zipcode)
scoringData$zipcode <- as.character(scoringData$zipcode)

analysisData$host_is_superhost <- as.logical(analysisData$host_is_superhost)
analysisData$host_has_profile_pic <- as.logical(analysisData$host_has_profile_pic)
analysisData$host_identity_verified <- as.logical(analysisData$host_identity_verified)
analysisData$instant_bookable <- as.logical(analysisData$instant_bookable)
analysisData$require_guest_profile_picture <- as.logical(analysisData$require_guest_profile_picture)
analysisData$require_guest_phone_verification <- as.logical(analysisData$require_guest_phone_verification)
analysisData$is_location_exact <- as.logical(analysisData$is_location_exact)

# If everything is OK we should only see a "Different number of rows" difference between the two datasets.

all_equal(analysisData, scoringData, convert = TRUE)



##Step 1

set.seed(5656)
ksplit <- createDataPartition(y = analysisData$price, p=.7, list=F, groups=50)
train <- analysisData[ksplit,]
test <- analysisData[-ksplit,]

##Step 2

train$train_test_score <- "train"
test$train_test_score <- "test"
scoringData$train_test_score <- "score"
baseData <- bind_rows(train,test,scoringData)

##Step 3
# Begin Data Wrangling Process
baseData$bed_type <- factor(baseData$bed_type)
baseData$property_type <- factor(baseData$property_type)
baseData$instant_bookable <- factor(baseData$instant_bookable)
#baseData$amenities <- factor(baseData$amenities)
baseData$neighbourhood_group_cleansed <- factor(baseData$neighbourhood_group_cleansed)


# Continue with other Data Wrangling, Imputation tasks, dropping rows that are pesky.....

baseData$minimum_nights[is.na(baseData$minimum_nights)] <- 1
baseData$bedrooms[is.na(baseData$bedrooms)] <- 1
baseData$number_of_reviews[is.na(baseData$number_of_reviews)] <- 1
baseData$instant_bookable[is.na(baseData$instant_bookable)] <- FALSE
baseData$review_scores_cleanliness[is.na(baseData$review_scores_cleanliness)] <- 1
baseData$review_scores_rating[is.na(baseData$review_scores_rating)] <- 1
baseData$cleaning_fee[is.na(baseData$cleaning_fee)] <- 1
baseData$security_deposit[is.na(baseData$security_deposit)] <- 1
baseData$calculated_host_listings_count_shared_rooms[is.na(baseData$calculated_host_listings_count_shared_rooms)] <- 1
baseData$calculated_host_listings_count_private_rooms[is.na(baseData$calculated_host_listings_count_private_rooms)] <- 1

x <- baseData %>% 
      mutate(parse_character(amenities))             

##Step 4
# Find the property types which are not present in all datasets.
baseData %>% 
  count(property_type, train_test_score) %>% 
  group_by(property_type) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n)) %>% 
  filter(is.na(train) || is.na(test) || is.na(score)) %>%
  mutate(score = coalesce(score, 0)) %>%
  mutate(test = coalesce(test, 0)) %>%
  mutate(train = coalesce(train, 0))


#Scenario 1
baseData %>% 
  count(property_type, train_test_score) %>% 
  group_by(property_type) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n)) %>% 
  filter(is.na(train) || is.na(test) || is.na(score)) %>% 
  filter(property_type == 'Cabin')


#Scenario 2
baseData %>% 
  count(property_type, train_test_score) %>% 
  group_by(property_type) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n)) %>% 
  filter(is.na(train) || is.na(test) || is.na(score)) %>% 
  filter(property_type == 'Train')


#Scenario 3
baseData %>% 
  count(property_type, train_test_score) %>% 
  group_by(property_type) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n)) %>% 
  filter(is.na(train) || is.na(test) || is.na(score)) %>% 
  filter(property_type == 'Farm stay')


#Fixing the data
baseData %>% 
  count(property_type, train_test_score) %>% 
  group_by(property_type) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n)) %>% 
  filter(is.na(train)) %>%
  mutate(score = coalesce(score, 0)) %>%
  mutate(test = coalesce(test, 0)) %>%
  mutate(train = coalesce(train, 0))


# Next, which property_types should we look to move these orphaned types too (hint: anything that has some Train data)
baseData %>% 
  count(property_type, train_test_score) %>% 
  group_by(property_type) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n))


#Remapping
pt <- baseData %>%
  select(property_type)  %>% 
  mutate(property_type_char = as.character(property_type)) %>%
  mutate(
    property_type_upd = case_when( 
      property_type_char == "Barn" ~ "Earth house",
      property_type_char == "Dorm" ~ "Hostel",
      property_type_char == "Farm stay" ~ "Earth house", 
      property_type_char == "Yurt" ~ "Earth house",
      property_type_char == "In-law" ~ "Apartment",
      property_type_char == "Bus" ~ "Earth house",
      property_type_char == "Train" ~ "Resort",
      TRUE ~ property_type_char))

baseData$property_type_upd <- as.factor(pt$property_type_upd)


# If Everything worked nothing should return
baseData %>% 
  count(property_type_upd, train_test_score) %>% 
  group_by(property_type_upd) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n)) %>% 
  filter(is.na(train)) %>%
  mutate(score = coalesce(score, 0)) %>%
  mutate(test = coalesce(test, 0)) %>%
  mutate(train = coalesce(train, 0))


##Step 5
train <- baseData  %>% 
  filter(train_test_score == "train")
test <- baseData  %>% 
  filter(train_test_score == "test")
score <- baseData  %>% 
  filter(train_test_score == "score")



# Test to ensure our datasets match in terms of the number of rows
nrow(analysisData); nrow(train); nrow(test); nrow(score);

library(gbm)
##Step 6
modelBoost = gbm(price~minimum_nights+number_of_reviews+ 
                   property_type_upd + bedrooms + instant_bookable + 
                   review_scores_cleanliness + review_scores_rating + 
                   neighbourhood_group_cleansed + security_deposit + 
                   calculated_host_listings_count_shared_rooms + 
                       calculated_host_listings_count_private_rooms, 
                 data=train,
                 distribution="gaussian",
                 n.trees = 5000,
                 interaction.depth = 1,
                 shrinkage = 0.04)
summary(modelBoost)


predBoostTrain <- predict(modelBoost, newdata=train)
rmse_boost_train = sqrt(mean((predBoostTrain-train$price)^2)); rmse_boost_train


##Step 7
predLinearTest <- predict(modelBoost, newdata=test)
rmse_ranger_test = sqrt(mean((predLinearTest-test$price)^2)); rmse_ranger_test


##Step 8
predLinearScore = predict(modelBoost, newdata=score)
submissionFile = data.frame(id = score$id, price = predLinearScore)
write.csv(submissionFile, './MayaBayram_Predictions.csv',row.names = F)

view(submissionFile)

