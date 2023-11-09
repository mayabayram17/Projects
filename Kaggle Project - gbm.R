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


data = read.csv(file = 'analysisData.csv', stringsAsFactors = F)
scoringData = read.csv(file = 'scoringData.csv', stringsAsFactors = F)


length(data)

scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA


## combining data to jointly deal with factor labels and missing data
combinedData <- bind_rows(data, scoringData)

#####################AMENITIES#####################


#creating varaibles for each separate amenity (with >300 occurences)
#function to clean up the amenities string and then convert into a vector
amenitiesToVector = function(string){ 
  return(strsplit(gsub("([a-z])([A-Z])", "\\1 \\2", gsub("\\W","",gsub("TV","Tv",gsub("translation missing: en.hosting_amenity_50","",gsub("translation missing: en.hosting_amenity_49","",string))))), " ")[[1]])
}
combinedData$amenities2 = lapply(combinedData$amenities,amenitiesToVector) #takes time to run


#creating a fucntion to count the frequency of each amenity
counter = function(vector_list){
  #' vector is a list of vectors 
  #' returns hte 
  every_amen = c()
  for(i in 1:length(vector_list)){
    every_amen = c(every_amen, vector_list[[i]])
  }
  return(table(every_amen))
}
amenities_count =counter(combinedData$amenities2)   



#this fucntion retruns a table similar to this (the following is converted to list)
amenities_count
#amentieis_freq         = list(35619, 34335, 33902, 32538,31039, 30300, 25082, 23874, 23863, 23496, 22253, 21598, 21033, 17181, 15272, 13934, 13796, 12732, 12371, 11892, 11684, 10702, 9933, 9680, 9451, 8952, 7352, 6791, 6603, 6517, 6447, 6363, 6328, 6006, 5931, 5427, 4995, 4739, 4256, 3938, 3749, 3685, 3674, 3653, 3637, 3489, 3411, 3336, 3167, 2985, 2761, 2546, 2445, 2401, 2065, 2043, 1746, 1652, 1631, 1593, 1448, 1426, 1422, 1415, 1333, 1255, 1226, 1167, 1088, 1052, 1033, 936, 909, 872, 860, 847, 841, 815, 798, 752, 750, 728, 532, 512, 509, 491, 476, 468, 396, 371, 352, 344, 340, 315, 312)
#names(amentieis_freq) = c('Heating','Kitchen','Essentials','Airconditioning','Smokedetector','Hangers','TV','Shampoo','Carbonmonoxidedetector','Hairdryer','Laptopfriendlyworkspace','Iron','Internet','Familykidfriendly','Washer','Dryer','Fireextinguisher','Firstaidkit','Buzzerwirelessintercom','translationmissing:en.hostingamenity50','Lockonbedroomdoor','CableTV','Elevator','translationmissing:en.hostingamenity49','Hotwater','Refrigerator','Dishesandsilverware','Bedlinens','Stove','Cookingbasics','Selfcheckin','Oven','Microwave','24hourcheckin','Coffeemaker','Extrapillowsandblankets','Stepfreeaccess','Safetycard','Petsallowed','Privateentrance','Longtermstaysallowed','Petsliveonthisproperty','Hostgreetsyou','Lockbox','Breakfast','Luggagedropoffallowed','Freeparkingonpremises','Freestreetparking','Dishwasher','Bathtub','Privatelivingroom','Widedoorway','Gym','Wheelchairaccessible','Doorman','Smokingallowed','Welllitpathtoentrance','Cats','Dogs','Keypad','Patioorbalcony','Hottub','Indoorfireplace','Gardenorbackyard','Roomdarkeningshades','Ethernetconnection','Paidparkingoffpremises','Suitableforevents','Widehallwayclearance','Other','Wideclearancetobed','Singlelevelhome','Frontdeskdoorperson','Windowguards','Children'sbooksandtoys','Accessibleheightbed','Flatpathtofrontdoor','Wideentryway','PacknPlaytravelcrib','BBQgrill','Accessibleheighttoilet','Babysitterrecommendations','toilet','Cleaningbeforecheckout','Pocketwifi','Smartlock','Highchair','Childrensdinnerware','Crib','Wideclearancetoshower','Gameconsole','Handheldshowerhead','Paidparkingonpremises','Pool') 
amentieis_freq         = list(4314, 45497, 590, 13919, 16415)                                                         
names(amentieis_freq) = c('Breakfast','Gym','Waterfront', 'Paidparkingonpremises', 'Cats')
#No use Kitchen, Oven, Refrigerator
memory.limit()
memory.limit(size = 120000)
for(n in names(amentieis_freq)){
  combinedData[n] = c()
  print(n)
}


count=1
for (val in combinedData$amenities2) {
  
  if('Breakfast' %in% val){
    combinedData$Breakfast[count] = 1
  }else{
    combinedData$Breakfast[count] = 0
  }
  if('Gym' %in% val){
    combinedData$Gym[count] = 1
  }else{
    combinedData$Gym[count] = 0
  }
  
  if('Waterfront' %in% val){
    combinedData$Waterfront[count] = 1
  }else{
    combinedData$Waterfront[count] = 0
  }
  
  if('Paidparkingonpremises' %in% val){
    combinedData$Paidparkingonpremises[count] = 1
  }else{
    combinedData$Paidparkingonpremises[count] = 0
  }
  if('Cats' %in% val){
    combinedData$Cats[count] = 1
  }else{
    combinedData$Cats[count] = 0
  }
  
  
  count=count+1
  
}

table(combinedData$Breakfast)
table(combinedData$Gym)
table(combinedData$Cats)
table(combinedData$Waterfront)
table(combinedData$Paidparkingonpremises)

#####################################################33
numericVars <- which(sapply(combinedData, is.numeric)) #index vector numeric variables
#then construct a correlation all frame
all_numVar <- combinedData[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
#sorting on decreasing correlations with price
cor_sorted <- as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))
print(cor_sorted)
#selecting only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.1)))
CorHigh

#############################################

### ### ZIP CODES ###
### ZIP CODES ###
## modify zip+4 codes

combinedData$zipcode <- substr(combinedData$zipcode, 1, 5)
## set missing / malformed zipcodes to NA
combinedData$zipcode[nchar(combinedData$zipcode)<5] <- NA_character_
#combinedData$zipcode[is.na(combinedData$zipcode)] = 0;
#combinedData$zipcode <- as.factor(combinedData$zipcode)



### neighbourhood_cleansed ###
unique(combinedData$neighbourhood_cleansed)
combinedData %>%
  mutate(neighbourhood_cleansed = case_when(
    neighbourhood_cleansed %in%  names(which(table(neighbourhood_cleansed) <= 10)) ~ NA_character_,
    TRUE ~ neighbourhood_cleansed
  )) -> combinedData

combinedData$neighbourhood_cleansed[is.na(combinedData$neighbourhood_cleansed)] = 'Other'
sum(combinedData$neighbourhood_cleansed=='Other')
table(combinedData$neighbourhood_cleansed)


### neighbourhood ###
combinedData$neighbourhood =as.character(combinedData$neighbourhood)
combinedData$neighbourhood[is.na(combinedData$neighbourhood)]='Others'
combinedData$neighbourhood=as.factor(combinedData$neighbourhood)

## CITY ##
## remove token separators
combinedData$city <- gsub(",|-|\\n|\\)|\\(|/|\\.", " ", tolower(combinedData$city))
## trim whitespace
combinedData$city <- stringr::str_trim(gsub("\\s+", " ", combinedData$city))

combinedData$city =as.character(combinedData$city)
combinedData$city[is.na(combinedData$city)]='Others'
combinedData$city=as.factor(combinedData$city)
###host_response_rate 

#combinedData$host_response_rate =as.numeric(gsub("([0-9]+).*$", "\\1", combinedData$host_response_rate))

### Fix property types ###
# remove properties with less than 10 examples #
unique(combinedData$property_type)
combinedData %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 10)) ~ NA_character_,
    TRUE ~ property_type
  )) -> combinedData

combinedData$property_type[is.na(combinedData$property_type)] = 'Other'
sum(is.na(combinedData$property_type)==TRUE)
######################

### Set missing character types to NA
char2na <- function(x) {
  case_when(
    x == "" ~ NA_character_,
    x == "N/A" ~ NA_character_,
    TRUE ~ x
  )
}
combinedData %>%
  mutate_if(is.character, char2na) -> combinedData
## Filter & convert character columns
# which columns are character types

char_ind <-  which(sapply(combinedData, class) == "character")
# flag variables for removal if there are too many cats or only 1 cat
char_ind_rm <- c()
for (i in char_ind) {
  ntypes <- sum(table(combinedData[,i])==1)
  ntypes
  if (ntypes/nrow(combinedData) > .3 | ntypes == 1 ) {
    char_ind_rm <- c(char_ind_rm, i)
  }
}
combinedData <- combinedData[,-char_ind_rm]

## collect character types and convert to factors
combinedData <- mutate_if(combinedData, is.character, as.factor)


### Remove outliers from _training_ data


sqft_99 <- quantile(combinedData$square_feet, .99, na.rm=TRUE)
case_when(
  is.na(combinedData$price) ~ TRUE,
  is.na(combinedData$square_feet) ~ TRUE,
  combinedData$square_feet > sqft_99 ~ FALSE,
  combinedData$square_feet==0 ~ FALSE,
  TRUE ~ TRUE) -> keep_index

combinedData <- combinedData[keep_index,]
#combinedData$square_feet[is.na(combinedData$square_feet)]=0
#combinedData$square_feet


## select numeric variables for imputation models
numeric_predictors <- which(colnames(combinedData) != "price" & sapply(combinedData, is.numeric))
imp_model_med <- preProcess(combinedData[,numeric_predictors], method = 'medianImpute')
imp_model_bag <- preProcess(combinedData[,numeric_predictors], method = 'bagImpute')

set.seed(617)
combinedData[,numeric_predictors] <- predict(imp_model_bag, newdata=combinedData[,numeric_predictors])


#Cleaning the data
#combinedData$beds[is.na(combinedData$beds)] = 0;
#combinedData$reviews_per_month[is.na(combinedData$reviews_per_month)]=0


#creating a function to perform this action
removeLevels = function(vect,threshold,alternate){
  #'vect: a vector (or a all frame's column)
  #'threshold: num, values under this value will be grouped into a single value
  #'alternate: str or num, the value which will reaplce levels under the threshold
  l = levels(as.factor(vect))
  t = table(vect)
  for(i in l){
    if (t[i] < threshold){
      vect[vect == i] <- alternate
    }
  }
  return(vect)
}


## this is just an example model
#performing the function on various hand-picked variables and converting some to factor later use. Thresholds and alternate values were chosen based on the results the table() fucntion
combinedData$property_type = as.factor(removeLevels(combinedData$property_type,100,"Other"))
combinedData$bedrooms = removeLevels(combinedData$bedrooms,60,5)
combinedData$bathrooms = removeLevels(combinedData$bathrooms,40,5)
combinedData$beds = removeLevels(combinedData$beds,20,10)
combinedData$cancellation_policy = removeLevels(combinedData$cancellation_policy,20,"strict")
combinedData$guests_included = removeLevels(combinedData$guests_included,50,8)
combinedData$calculated_host_listings_count = removeLevels(combinedData$calculated_host_listings_count,200,9)
combinedData$zipcode = as.factor(removeLevels(combinedData$zipcode,15,"Other"))



## split back into training data
train <- combinedData[!is.na(combinedData$price),]
test <- combinedData[is.na(combinedData$price),]



train <- bind_rows(train, test %>%
                     filter(test$zipcode=='10020') %>%
                     mutate(price=median(train$price)))
train <- bind_rows(train, test %>%
                     filter(test$zipcode=='11581') %>%
                     mutate(price=median(train$price)))



## this is just an example model
densityplot(train$price)

train =train[train$price>30,]
train =train[train$price<750,]

split = sample.split(Y=train$price, SplitRatio =0.80)
train_split=train[split,]
test_split =train[-split,]

nrow(train_split)
##############city#####################
x = levels(test_split$city)
count <- 0
for (val in x) {
  if(sum(train_split$city==val)<=0){
    
    train_split <- bind_rows(train_split, test_split %>%
                               filter(test_split$city==val))
    count=count+1
  }
}
count
nrow(train_split)
############################neighbourhood##############

x = levels(test_split$neighbourhood)
count <- 0
for (val in x) {
  if(sum(train_split$neighbourhood==val)<=0){
    
    train_split <- bind_rows(train_split, test_split %>%
                               filter(test_split$neighbourhood==val))
    count=count+1
  }
}
count
nrow(train_split)
######################zipcode#####################
x = levels(test_split$zipcode)
count <- 0
for (val in x) {
  if(sum(train_split$zipcode==val)<=0){
    
    train_split <- bind_rows(train_split, test_split %>%
                               filter(test_split$zipcode==val))
    count=count+1
  }
}
count
nrow(train_split)
######################neighbourhood_cleansed#####################
x = levels(test_split$neighbourhood_cleansed)
count <- 0
for (val in x) {
  if(sum(train_split$neighbourhood_cleansed==val)<=0){
    
    train_split <- bind_rows(train_split, test_split %>%
                               filter(test_split$neighbourhood_cleansed==val))
    count=count+1
  }
}
count
nrow(train_split)
#############################


##zipcode, property_type
#formula <- as.formula("price~neighbourhood")
# host_response_rate not useful

# gbm not useful:lat+lng+ street , bed_type, number_of_reviews  
#names(amentieis_freq) = c("Breakfast+Kitchen+Pool+Oven+Refrigerator")
table(train_split$Kitchen)
formula1 <- as.formula("price~bathrooms+bedrooms+beds+review_scores_accuracy+accommodates+number_of_reviews+reviews_per_month+cleaning_fee+security_deposit+guests_included+extra_people+minimum_nights+host_response_rate")
formula2 <- as.formula("price~zipcode+accommodates+square_feet+number_of_reviews+reviews_per_month+calculated_host_listings_count_private_rooms+property_type+cleaning_fee+neighbourhood+security_deposit+guests_included+extra_people+minimum_nights+maximum_nights+cancellation_policy+host_response_rate+room_type+city")
formula3 <- as.formula("price~zipcode+bathrooms+bedrooms+beds+review_scores_accuracy+accommodates+square_feet+reviews_per_month+property_type+cleaning_fee+neighbourhood+security_deposit+guests_included+extra_people+minimum_nights+host_response_rate+neighbourhood_cleansed+city+host_response_rate+city+calculated_host_listings_count+weekly_price+review_scores_location+calculated_host_listings_count_entire_homes+Breakfast+Gym+host_is_superhost+availability_30+host_total_listings_count+is_business_travel_ready")
#Linear regression model
model = lm(formula2,data=train_split)
#decision tree model
model = rpart(formula1,data=train_split,cp=0.005, method='anova')
#GBM Model

set.seed(617)
model = gbm(formula3,
            data=train_split,
            distribution="gaussian",
            n.trees = 5000,
            interaction.depth = 1,
            shrinkage = 0.04)

#RSME Calculation
pred_train = predict(model);

rmse_train = sqrt(mean((pred_train-train_split$price)^2)); rmse_train 
pred_test = predict(model, newdata = test_split);rmse_test = sqrt(mean((pred_test-test_split$price)^2)); rmse_test 


pred_boost = predict(model,newdata = test, n.trees =3000,type ='response')

# Construct submission from predictions
pred = predict(model,newdata = test)
submissionFile = data.frame(id = test$id, price = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)


view(submissionFile)
