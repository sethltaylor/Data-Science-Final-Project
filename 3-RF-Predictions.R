### PREDICITIONS: Random Forest ####

"C:/Users/viola/Documents/Studium/MPP GU/Spring 2017/Data Science/Data-Science-Final-Project" # adjust individually

library(randomForest) # for random forest predicitions

# Mean F1 function to test accuracy of predictions
meanf1 <- function(actual, predicted){
  #Mean F1 score function
  #actual = a vector of actual labels
  #predicted = predicted labels
  
  classes <- unique(actual)
  results <- data.frame()
  for(k in classes){
    results <- rbind(results, 
                     data.frame(class.name = k,
                                weight = sum(actual == k)/length(actual),
                                precision = sum(predicted == k & actual == k)/sum(predicted == k), 
                                recall = sum(predicted == k & actual == k)/sum(actual == k)))
  }
  results$score <- results$weight * 2 * (results$precision * results$recall) / (results$precision + results$recall) 
  return(sum(results$score))
}


# Function for k-folds
kfolds.index <- function(n, k, random = TRUE){
  # Returns a vector of labels for each of k-folds. 
  # Useful for setting up k-folds cross validation
  #
  # Args:
  #       n: data size
  #       k: k-folds
  #       random: whether folds should be sequential or randomized (default)
  #
  # Returns:
  #       Vector of numeric labels
  
  #create row index
  row.id <- 1:n
  
  #Decide splits
  break.id <- cut(row.id, breaks = k, labels = FALSE)
  
  #Randomize
  if(random == TRUE){
    row.id <- row.id[sample(row.id, replace = FALSE)]
  }
  
  #Package up
  out <- data.frame(row.id = row.id, folds = break.id)
  out <- out[order(out$row.id), ]
  return(out[,2])
}

### MODELS C) RANDOM FOREST ####

vars <- c("ID", "CH_CRIMHIST_COLLAPSED", "OFFENSE_VIOLENT", "OFFENSE_DRUG", "SES_PHYSABUSED_EVER", "CS_SENTENCEMTH", 
          "SES_PARENTS_INCARCERATED", "SES_HASCHILDREN", "EDUCATION", "AGE_CAT", "SES_INCOMEMTH",
          "black.nh", "hispanic", "asian", "DRUG_COCRKREG", "SES_SEXABUSED_EVER", "SES_FAMILY_INCARCERATED", "DRUG_ANY_ALCDRUGTME", 
          "DRUG_ANYREG", "DRUG_HROPTME", "DRUG_METHATME", "LIFE_SENTENCE")

full.r <- full.numeric[,names(full.numeric) %in% vars]

# factors in DT: dummy would be created for every value of factor, which creates Error that result would be too long a vector

str(full.r) # AGE_CAT, EDUCATION, and SES_INCOMEMTH are factors
# converting them to numeric with the same argument as for KNN predictions: no interpretation, ordinal ranking preserved

full.r$AGE_CAT <- as.numeric(full.r$AGE_CAT)
full.r$EDUCATION <- as.numeric(full.r$EDUCATION)
full.r$SES_INCOMEMTH <- as.numeric(full.r$SES_INCOMEMTH)

# delete rows with missing y (CH_CRIMHIST_COLLAPSED)
full.r <- full.r[is.na(full.r$CH_CRIMHIST_COLLAPSED)==F,]

# RF only handles complete cases --> delete vars with too many missing obs
full.r <- full.r[, !names(full.r) %in% c("DRUG_COCRKREG", "DRUG_ANY_ALCDRUGTME")]

table(complete.cases(full.r)) # now only 5137 are missing
table(complete.cases(full.r[,!names(full.r) %in% c("CS_SENTENCEMTH", "SES_INCOMEMTH")])) 
# this would further reduce missing vars by 3587 obs -> try later

# remove incomplete cases
full.r <- full.r[complete.cases(full.r),] # 12960 obs


## set aside 15% of full.numeric data for validation purposes (in addition to k-fold testing and training)

smp_size <- floor(0.15 * nrow(full.r))

set.seed(123)
val_ind <- sample(seq_len(nrow(full.r)), size = smp_size)

# partiion using the index created
full.val <- full.r[val_ind, ]
full.train <- full.r[-val_ind, ]


## break train dataset into k-folds

n <- nrow(full.train)
k <- 5 # k is arbitrarily chosen, probably a bit high and therefore time intensive

full.train$folds <- kfolds.index(n, k)

#Set placeholder for (mean) errors
error.train <- c() # will calculate meanf1 for train data
error.test <- c() # will calculate meanf1 for test data
error.val <- c() # will calculate meanf1 for validation data set aside previously


## MODEL C.a) Random Forest Default with all 18 Vars ###

#Run kfolds
for(k in 1:5){
  
  #Cut train/test
  train.r <- full.train[full.train$folds != k, ]
  test.r <- full.train[full.train$folds == k, ]
  
  # estimate model
  rf.default.fit <- randomForest(as.factor(CH_CRIMHIST_COLLAPSED) ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + 
                            SES_PARENTS_INCARCERATED + SES_HASCHILDREN + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                            black.nh + hispanic + asian + SES_SEXABUSED_EVER + SES_FAMILY_INCARCERATED + 
                            DRUG_ANYREG + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE + CS_SENTENCEMTH, 
                            data = train.r)
  
  # Decision Tree predictions for train, test, and validate
  pred.train <- predict(rf.default.fit, train.r, type = "class")
  pred.test <- predict(rf.default.fit, test.r, type = "class")
  pred.val <- predict(rf.default.fit, full.val, type = "class")
  
  # calc error, log it
  error.train <- c(error.train, meanf1(train.r$CH_CRIMHIST_COLLAPSED, pred.train))
  error.test <- c(error.test, meanf1(test.r$CH_CRIMHIST_COLLAPSED, pred.test))
  error.val <- c(error.val, meanf1(full.val$CH_CRIMHIST_COLLAPSED, pred.val)) 
}


# add mean error values to model.errors
model.errors[10,c(2:4)] <- c(mean(error.train), mean(error.test), mean(error.val)) # mean F1: ~68%
# assumes that df model.error from KNN-Predictions is still in working environment

# best Mean F1 so far!

# rerun Model on full dataset
rf.full <- randomForest(as.factor(CH_CRIMHIST_COLLAPSED) ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + 
                          SES_PARENTS_INCARCERATED + SES_HASCHILDREN + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                          black.nh + hispanic + asian + SES_SEXABUSED_EVER + SES_FAMILY_INCARCERATED + 
                          DRUG_ANYREG + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE + CS_SENTENCEMTH, 
                          data = full.r)
pred.full <- predict(rf.full, full.r, type = "class")
model.errors[10,5] <- meanf1(full.r$CH_CRIMHIST_COLLAPSED, pred.full)

# confusion matrix
table(full.r$CH_CRIMHIST_COLLAPSED, pred.full) # much better sensitivity and specificity than kNN and DT

#Set empty placeholders again
error.train <- c()
error.test <- c()
error.val <- c()


## MODEL C.b) Random Forest Default with 16 Vars ###

vars.16 <- c("ID", "CH_CRIMHIST_COLLAPSED", "OFFENSE_VIOLENT", "OFFENSE_DRUG", "SES_PHYSABUSED_EVER",
          "SES_PARENTS_INCARCERATED", "SES_HASCHILDREN", "EDUCATION", "AGE_CAT",
          "black.nh", "hispanic", "asian", "SES_SEXABUSED_EVER", "SES_FAMILY_INCARCERATED",
          "DRUG_ANYREG", "DRUG_HROPTME", "DRUG_METHATME", "LIFE_SENTENCE")

full.r <- full.numeric[,names(full.numeric) %in% vars.16]

full.r$AGE_CAT <- as.numeric(full.r$AGE_CAT)
full.r$EDUCATION <- as.numeric(full.r$EDUCATION)

# remove incomplete cases
full.r <- full.r[complete.cases(full.r),] # 16547 obs

#placeholder for tune.param
tune.param <- c() 

#Run kfolds
for(k in 1:5){
  
  #Cut train/test
  train.r <- full.train[full.train$folds != k, ]
  test.r <- full.train[full.train$folds == k, ]
  
  # estimate model
  rf.def16.fit <- randomForest(as.factor(CH_CRIMHIST_COLLAPSED) ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + 
                                   SES_PARENTS_INCARCERATED + SES_HASCHILDREN + EDUCATION + AGE_CAT +
                                   black.nh + hispanic + asian + SES_SEXABUSED_EVER + SES_FAMILY_INCARCERATED + 
                                   DRUG_ANYREG + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE, 
                                   data = train.r)
  
  # Decision Tree predictions for train, test, and validate
  pred.train <- predict(rf.def16.fit, train.r, type = "class")
  pred.test <- predict(rf.def16.fit, test.r, type = "class")
  pred.val <- predict(rf.def16.fit, full.val, type = "class")
  
  # calc error, log it
  error.train <- c(error.train, meanf1(train.r$CH_CRIMHIST_COLLAPSED, pred.train))
  error.test <- c(error.test, meanf1(test.r$CH_CRIMHIST_COLLAPSED, pred.test))
  error.val <- c(error.val, meanf1(full.val$CH_CRIMHIST_COLLAPSED, pred.val)) 
  
  # FINE TUNING
    plot(rf.def16.fit)
    tune.x <- c("OFFENSE_VIOLENT", "OFFENSE_DRUG", "SES_PHYSABUSED_EVER",
              "SES_PARENTS_INCARCERATED", "SES_HASCHILDREN", "EDUCATION", "AGE_CAT",
              "black.nh", "hispanic", "asian", "SES_SEXABUSED_EVER", "SES_FAMILY_INCARCERATED",
              "DRUG_ANYREG", "DRUG_HROPTME", "DRUG_METHATME", "LIFE_SENTENCE")
    fit.tune <- tuneRF(train.r[,(names(train.r) %in% tune.x)], train.r$CH_CRIMHIST_COLLAPSED, ntreeTry = 500,
                     mtryStart = 1, stepFactor = 2, improve = 0.001, trace = T, plot = T)
    # save best k for each iteration in k-fold training and testing
    tune.param <- c(tune.param, fit.tune[fit.tune[, 2] == min(fit.tune[, 2]), 1])
}


# add mean error values to model.errors
model.errors[11,c(2:4)] <- c(mean(error.train), mean(error.test), mean(error.val)) 
# mean F1 higher for validate but lower for test

# rerun Model on full dataset
rf.full <- randomForest(as.factor(CH_CRIMHIST_COLLAPSED) ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + 
                          SES_PARENTS_INCARCERATED + SES_HASCHILDREN + EDUCATION + AGE_CAT +
                          black.nh + hispanic + asian + SES_SEXABUSED_EVER + SES_FAMILY_INCARCERATED + 
                          DRUG_ANYREG + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE,
                          data = full.r)
pred.full <- predict(rf.full, full.r, type = "class")
model.errors[11,5] <- meanf1(full.r$CH_CRIMHIST_COLLAPSED, pred.full) # mean f1 better with RF 18 vars default

# confusion matrix
table(full.r$CH_CRIMHIST_COLLAPSED, pred.full) # number of false positives higher than with default RT with 18 vars

#Set empty placeholders again
error.train <- c()
error.test <- c()
error.val <- c()

tune.param # k=4 is optimal for all 5 folds, OOB error always around 19-20%

## Model C.c) Fine Tuning of RF with k=4 and 18 Vars

full.r <- full.numeric[,names(full.numeric) %in% vars]
full.r <- full.r[, !names(full.r) %in% c("DRUG_COCRKREG", "DRUG_ANY_ALCDRUGTME")]

full.r$AGE_CAT <- as.numeric(full.r$AGE_CAT)
full.r$EDUCATION <- as.numeric(full.r$EDUCATION)
full.r$SES_INCOMEMTH <- as.numeric(full.r$SES_INCOMEMTH)

# remove incomplete cases
full.r <- full.r[complete.cases(full.r),] # 12960 obs

#Run kfolds
for(k in 1:5){
  
  #Cut train/test
  train.r <- full.train[full.train$folds != k, ]
  test.r <- full.train[full.train$folds == k, ]
  
  # estimate model
  rf.opt.fit <- randomForest(as.factor(CH_CRIMHIST_COLLAPSED) ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + 
                                 SES_PARENTS_INCARCERATED + SES_HASCHILDREN + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                                 black.nh + hispanic + asian + SES_SEXABUSED_EVER + SES_FAMILY_INCARCERATED + 
                                 DRUG_ANYREG + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE + CS_SENTENCEMTH, 
                                 data = train.r, mtry=4)
  
  # Decision Tree predictions for train, test, and validate
  pred.train <- predict(rf.opt.fit, train.r, type = "class")
  pred.test <- predict(rf.opt.fit, test.r, type = "class")
  pred.val <- predict(rf.opt.fit, full.val, type = "class")
  
  # calc error, log it
  error.train <- c(error.train, meanf1(train.r$CH_CRIMHIST_COLLAPSED, pred.train))
  error.test <- c(error.test, meanf1(test.r$CH_CRIMHIST_COLLAPSED, pred.test))
  error.val <- c(error.val, meanf1(full.val$CH_CRIMHIST_COLLAPSED, pred.val)) 
  
}

# add mean error values to model.errors
model.errors[12,c(2:4)] <- c(mean(error.train), mean(error.test), mean(error.val))
# only slightly better than RF default for train and test

rf.full <- randomForest(as.factor(CH_CRIMHIST_COLLAPSED) ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + 
                          SES_PARENTS_INCARCERATED + SES_HASCHILDREN + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                          black.nh + hispanic + asian + SES_SEXABUSED_EVER + SES_FAMILY_INCARCERATED + 
                          DRUG_ANYREG + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE + CS_SENTENCEMTH, 
                          data = full.r, mtry=4)
pred.full <- predict(rf.full, full.r, type = "class")
model.errors[12,5] <- meanf1(full.r$CH_CRIMHIST_COLLAPSED, pred.full) #mean f1 for full dataset lower than with RF default

# confusion matrix
table(full.r$CH_CRIMHIST_COLLAPSED, pred.full) # sensitivity and specificity pretty much like RF default

## --> tuned RF probably the best model

#Set empty placeholders again
error.train <- c()
error.test <- c()
error.val <- c()
 
