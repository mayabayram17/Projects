library(tidyverse)
library(leaps)
library(caret)
library(ggthemes)
library(glmnet)
library(skimr)

setwd('D:/Columbia/Spring 2021/APAN Frameworks and methods 1/Session 7 - Kaggle')

options(scipen = 999)
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



# Continue with other Data Wrangling, Imputation tasks, dropping rows that are pesky.....
baseData$price[is.na(baseData$price)] <- 106.1141
baseData$minimum_nights[is.na(baseData$minimum_nights)] <- 8
baseData$bedrooms[is.na(baseData$bedrooms)] <- 2
baseData$number_of_reviews[is.na(baseData$number_of_reviews)] <- 23
baseData$instant_bookable[is.na(baseData$instant_bookable)] <- FALSE
baseData$review_scores_cleanliness[is.na(baseData$review_scores_cleanliness)] <- 5
baseData$review_scores_rating[is.na(baseData$review_scores_rating)] <- 5
baseData$cleaning_fee[is.na(baseData$cleaning_fee)] <- 88
baseData$security_deposit[is.na(baseData$security_deposit)] <- 1
baseData$host_has_profile_pic[is.na(baseData$host_has_profile_pic)] <- FALSE
baseData$host_identity_verified[is.na(baseData$host_identity_verified)] <- FALSE
baseData$guests_included[is.na(baseData$guests_included)] <- 1





baseData$host_response_rate <- str_extract(baseData$host_response_rate, "[[:digit:]]+")
baseData$host_response_rate <- as.numeric(baseData$host_response_rate)
unique(baseData$host_response_rate)
baseData$host_response_rate[is.na(baseData$host_response_rate) == TRUE] <- 0

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



formula1 <- as.formula(price~bathrooms+bedrooms+beds+review_scores_accuracy+
                         accommodates+number_of_reviews+reviews_per_month+security_deposit+
                         guests_included+minimum_nights+host_response_rate)

formula2 <- as.formula(price~accommodates+square_feet+number_of_reviews+
                         reviews_per_month+calculated_host_listings_count_private_rooms+cleaning_fee+
                         security_deposit+guests_included+extra_people+minimum_nights+maximum_nights+
                         room_type)

formula3 <- as.formula(price~bathrooms+bedrooms+beds+
                         accommodates+square_feet+reviews_per_month+
                         cleaning_fee+neighbourhood+security_deposit+guests_included+extra_people+minimum_nights+
                         host_response_rate+neighbourhood_cleansed+host_response_rate+city+
                         calculated_host_listings_count+weekly_price+review_scores_location+
                         calculated_host_listings_count_entire_homes+host_is_superhost+
                         availability_30+host_total_listings_count+is_business_travel_ready)

##Step 6
modelLinear = lm(formula3, data=train)
summary(modelLinear)


predLinearTrain <- predict(modelLinear, newdata=train)
caret::postResample(pred = predLinearTrain, train$price)


##Step 7
predLinearTest <- predict(modelLinear, newdata=test)
caret::postResample(pred = predLinearTest, test$price)


##Step 8
predLinearScore = predict(modelLinear, newdata=score)
submissionFile = data.frame(id = score$id, price = predLinearScore)
write.csv(submissionFile, './MayaBayram_FirstSubmission.csv',row.names = F)

view(submissionFile)



#Report
ggplot(analysisData, aes(price))+
  geom_histogram() + xlim(0, 1500) +
  geom_vline(xintercept = mean(analysisData$price), color = "blue")


ggplot(analysisData, aes(x= accommodates, y= price))+
  geom_smooth() +
  geom_vline(aes(xintercept = mean(analysisData$accommodates), color = "Mean accommodates"))+
  geom_hline(aes(yintercept = mean(analysisData$price), color = "Mean price")) +   scale_colour_manual(name="Means", values=c("red", "blue"))


str(analysisData)

ggplot(data=analysisData,mapping = aes(x=price,y=factor(room_type)))+
  geom_boxplot()+ xlim(0, 1500) 

ggplot(analysisData, aes(factor(room_type)), stat = "count")+
  geom_bar()

ggplot(analysisData, aes(factor(room_type), price))+
  geom_bar(stat = "identity")


