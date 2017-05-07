### PREDICITIONS: Decsion Trees ####

"C:/Users/viola/Documents/Studium/MPP GU/Spring 2017/Data Science/Data-Science-Final-Project" # adjust individually

library(rpart) # for decision trees

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

### MODELS B) TRAINING DECISION TREES ####

vars <- c("ID", "CH_CRIMHIST_COLLAPSED", "OFFENSE_VIOLENT", "OFFENSE_DRUG", "SES_PHYSABUSED_EVER", "CS_SENTENCEMTH", 
          "SES_PARENTS_INCARCERATED", "SES_HASCHILDREN", "EDUCATION", "AGE_CAT", "SES_INCOMEMTH",
          "black.nh", "hispanic", "asian", "DRUG_COCRKREG", "SES_SEXABUSED_EVER", "SES_FAMILY_INCARCERATED", 
          "DRUG_ANY_ALCDRUGTME", "DRUG_ANYREG", "DRUG_HROPTME", "DRUG_METHATME", "LIFE_SENTENCE")

full.d <- full.numeric[,names(full.numeric) %in% vars]

# factors in DT: dummy would be created for every value of factor, which creates Error that result would be too long a vector

str(full.d) # AGE_CAT, EDUCATION, and SES_INCOMEMTH are factors
# converting them to numeric with the same argument as for KNN predictions: no interpretation, ordinal ranking preserved

full.d$AGE_CAT <- as.numeric(full.d$AGE_CAT)
full.d$EDUCATION <- as.numeric(full.d$EDUCATION)
full.d$SES_INCOMEMTH <- as.numeric(full.d$SES_INCOMEMTH)

# delete rows with missing y (CH_CRIMHIST_COLLAPSED)
full.d <- full.d[is.na(full.d$CH_CRIMHIST_COLLAPSED)==F,]


## set aside 15% of full.numeric data for validation purposes (in addition to k-fold testing and training)

smp_size <- floor(0.15 * nrow(full.d))

set.seed(123)
val_ind <- sample(seq_len(nrow(full.d)), size = smp_size)

# partiion using the index created
full.val <- full.d[val_ind, ]
full.train <- full.d[-val_ind, ]


## break train dataset into k-folds

n <- nrow(full.train)
k <- 5 # k is arbitrarily chosen, probably a bit high and therefore time intensive

full.train$folds <- kfolds.index(n, k)

#Set placeholder for (mean) errors
error.train <- c() # will calculate meanf1 for train data
error.test <- c() # will calculate meanf1 for test data
error.val <- c() # will calculate meanf1 for validation data set aside previously


## MODEL B.a) Decision Tree with Default CP (cp=0.01) ###

#Run kfolds
for(k in 1:5){
  
  #Cut train/test
  train.d <- full.train[full.train$folds != k, ]
  test.d <- full.train[full.train$folds == k, ]
  
  # estimate model
  dt.default.fit <- rpart(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + 
                            SES_PARENTS_INCARCERATED + SES_HASCHILDREN + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                            black.nh + hispanic + asian + DRUG_COCRKREG + SES_SEXABUSED_EVER + SES_FAMILY_INCARCERATED + 
                            DRUG_ANY_ALCDRUGTME + DRUG_ANYREG + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE, 
                            method = "class", data = train.d)
  
  # Decision Tree predictions for train, test, and validate
  pred.train <- predict(dt.default.fit, train.d, type = "class")
  pred.test <- predict(dt.default.fit, test.d, type = "class")
  pred.val <- predict(dt.default.fit, full.val, type = "class")
  
  # calc error, log it
  error.train <- c(error.train, meanf1(train.d$CH_CRIMHIST_COLLAPSED, pred.train))
  error.test <- c(error.test, meanf1(test.d$CH_CRIMHIST_COLLAPSED, pred.test))
  error.val <- c(error.val, meanf1(full.val$CH_CRIMHIST_COLLAPSED, pred.val)) 
}


# add mean error values to model.errors
model.errors[8,c(2:4)] <- c(mean(error.train), mean(error.test), mean(error.val)) # low mean F1: ~65%
# assumes that df model.error from KNN-Predictions is still in working environment

# rerun Model on full dataset
dt.full <- rpart(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + 
                     SES_PARENTS_INCARCERATED + SES_HASCHILDREN + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                     black.nh + hispanic + asian + DRUG_COCRKREG + SES_SEXABUSED_EVER + SES_FAMILY_INCARCERATED + 
                     DRUG_ANY_ALCDRUGTME + DRUG_ANYREG + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE, 
                     method = "class", data = full.d)
pred.full <- predict(dt.full, full.d, type = "class")
model.errors[8,5] <- meanf1(full.d$CH_CRIMHIST_COLLAPSED, pred.full)

# confusion matrix
table(full.d$CH_CRIMHIST_COLLAPSED, pred.full) # number of false positives way too large

#Set empty placeholders again
error.train <- c()
error.test <- c()
error.val <- c()


## MODEL B.b) Decision Tree with CP = 0 ###

#Run kfolds
for(k in 1:5){
  
  #Cut train/test
  train.d <- full.train[full.train$folds != k, ]
  test.d <- full.train[full.train$folds == k, ]
  
  # estimate model
  dt.0.fit <- rpart(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + 
                            SES_PARENTS_INCARCERATED + SES_HASCHILDREN + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                            black.nh + hispanic + asian + DRUG_COCRKREG + SES_SEXABUSED_EVER + SES_FAMILY_INCARCERATED + 
                            DRUG_ANY_ALCDRUGTME + DRUG_ANYREG + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE, 
                            method = "class", data = train.d, cp=0)
  printcp(dt.0.fit)
  
  # Decision Tree predictions for train, test, and validate
  pred.train <- predict(dt.0.fit, train.d, type = "class")
  pred.test <- predict(dt.0.fit, test.d, type = "class")
  pred.val <- predict(dt.0.fit, full.val, type = "class")
  
  # calc error, log it
  error.train <- c(error.train, meanf1(train.d$CH_CRIMHIST_COLLAPSED, pred.train))
  error.test <- c(error.test, meanf1(test.d$CH_CRIMHIST_COLLAPSED, pred.test))
  error.val <- c(error.val, meanf1(full.val$CH_CRIMHIST_COLLAPSED, pred.val)) 
}

# add mean error values to model.errors
model.errors[9,c(2:4)] <- c(mean(error.train), mean(error.test), mean(error.val)) # low mean F1: ~65%

# lowest xerror and lowest acceptable split different for each k, but varies between nsplit = 4 and nsplit = 32
# --> choose nsplit = 6 (arbitrary) --> cp arbitrary as well b/c different for each k: choose 0.003

# rerun Model on full dataset
dt.full <- rpart(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + 
                   SES_PARENTS_INCARCERATED + SES_HASCHILDREN + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                   black.nh + hispanic + asian + DRUG_COCRKREG + SES_SEXABUSED_EVER + SES_FAMILY_INCARCERATED + 
                   DRUG_ANY_ALCDRUGTME + DRUG_ANYREG + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE, 
                 method = "class", data = full.d, cp=0)
pred.full <- predict(dt.full, full.d, type = "class")
model.errors[9,5] <- meanf1(full.d$CH_CRIMHIST_COLLAPSED, pred.full)

# confusion matrix
table(full.d$CH_CRIMHIST_COLLAPSED, pred.full) # number of false positives smaller than with default DT

#Set empty placeholders again
error.train <- c()
error.test <- c()
error.val <- c()


## MODEL B.c) Decision Tree with CP = 0.003 ###

#Run kfolds
for(k in 1:5){
  
  #Cut train/test
  train.d <- full.train[full.train$folds != k, ]
  test.d <- full.train[full.train$folds == k, ]
  
  # estimate model
  dt.opt.fit <- rpart(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + 
                      SES_PARENTS_INCARCERATED + SES_HASCHILDREN + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                      black.nh + hispanic + asian + DRUG_COCRKREG + SES_SEXABUSED_EVER + SES_FAMILY_INCARCERATED + 
                      DRUG_ANY_ALCDRUGTME + DRUG_ANYREG + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE, 
                    method = "class", data = train.d, cp=0.003)
  
  # Decision Tree predictions for train, test, and validate
  pred.train <- predict(dt.opt.fit, train.d, type = "class")
  pred.test <- predict(dt.opt.fit, test.d, type = "class")
  pred.val <- predict(dt.opt.fit, full.val, type = "class")
  
  # calc error, log it
  error.train <- c(error.train, meanf1(train.d$CH_CRIMHIST_COLLAPSED, pred.train))
  error.test <- c(error.test, meanf1(test.d$CH_CRIMHIST_COLLAPSED, pred.test))
  error.val <- c(error.val, meanf1(full.val$CH_CRIMHIST_COLLAPSED, pred.val)) 
}

# add mean error values to model.errors
model.errors[10,c(2:4)] <- c(mean(error.train), mean(error.test), mean(error.val)) # mean F1 hardly better: ~66%

# rerun Model on full dataset
dt.full <- rpart(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + 
                   SES_PARENTS_INCARCERATED + SES_HASCHILDREN + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                   black.nh + hispanic + asian + DRUG_COCRKREG + SES_SEXABUSED_EVER + SES_FAMILY_INCARCERATED + 
                   DRUG_ANY_ALCDRUGTME + DRUG_ANYREG + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE, 
                 method = "class", data = full.d, cp=0.003)
pred.full <- predict(dt.full, full.d, type = "class")
model.errors[10,5] <- meanf1(full.d$CH_CRIMHIST_COLLAPSED, pred.full)

# confusion matrix
table(full.d$CH_CRIMHIST_COLLAPSED, pred.full) # number of false positives nearly as high as with default DT --> 0 better

#Set empty placeholders again
error.train <- c()
error.test <- c()
error.val <- c()


## Model B.d) Decision Tree with variables from best GLM result and DT.opt

## set aside 15% of full.numeric data for validation purposes (in addition to k-fold testing and training)

smp_size <- floor(0.15 * nrow(full.opt))

set.seed(123)
val_ind <- sample(seq_len(nrow(full.opt)), size = smp_size)

# partiion using the index created
full.val <- full.opt[val_ind, ]
full.train <- full.opt[-val_ind, ]


## break train dataset into k-folds

n <- nrow(full.train)
k <- 5 # k is arbitrarily chosen, probably a bit high and therefore time intensive

full.train$folds <- kfolds.index(n, k)


#Run kfolds
for(k in 1:5){
  
  #Cut train/test
  train.d <- full.train[full.train$folds != k, ]
  test.d <- full.train[full.train$folds == k, ]
  
  #vector y for index of columns that are not part of x
  y <- c("ID", "CH_CRIMHIST_COLLAPSED", "folds")
  
  # estimate model
  dt.opt.opt <- rpart(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG +OFFENSE_PROPERTY +SES_PHYSABUSED_EVER +
                        CS_SENTENCEMTH + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + AGE_CAT + 
                        SES_SEXABUSED_EVER + DRUG_ANYREG + DRUG_ANYTME + black.nh + hispanic + asian + state + EDUCATION +
                        SES_FATHER_INCARCERATED + DRUG_COCRKTME + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE + GENDER + 
                        TYPEOFFENSE + DRUG_MARIJTME + CH_PRIORARREST_CAT + SES_LIVE_CHILD_ARREST + DRUG_ABUSE_ONLY + DRUG_TRT, 
                        method = "class", data = train.d, cp=0.003)
  
  # Decision Tree predictions for train, test, and validate
  pred.train <- predict(dt.opt.opt, train.d, type = "class")
  pred.test <- predict(dt.opt.opt, test.d, type = "class")
  pred.val <- predict(dt.opt.opt, full.val, type = "class")
  
  #calc error, log it
  error.train <- c(error.train, meanf1(train.d$CH_CRIMHIST_COLLAPSED, pred.train))
  error.test <- c(error.test, meanf1(test.d$CH_CRIMHIST_COLLAPSED, pred.test))
  error.val <- c(error.val, meanf1(full.val$CH_CRIMHIST_COLLAPSED, pred.val)) 
}

# add mean error values to model.errors
model.errors[11,c(2:4)] <- c(mean(error.train), mean(error.test), mean(error.val))
# best prediction so far

# rerun Model on full dataset
dt.full <- rpart(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG +OFFENSE_PROPERTY +SES_PHYSABUSED_EVER +
                   CS_SENTENCEMTH + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + AGE_CAT + 
                   SES_SEXABUSED_EVER + DRUG_ANYREG + DRUG_ANYTME + black.nh + hispanic + asian + state + EDUCATION +
                   SES_FATHER_INCARCERATED + DRUG_COCRKTME + DRUG_HROPTME + DRUG_METHATME + LIFE_SENTENCE + GENDER + 
                   TYPEOFFENSE + DRUG_MARIJTME + CH_PRIORARREST_CAT + SES_LIVE_CHILD_ARREST + DRUG_ABUSE_ONLY + DRUG_TRT,
                   method = "class", data = full.opt, cp=0.003)
pred.full <- predict(dt.full, full.opt, type = "class")
model.errors[11,5] <- meanf1(full.opt$CH_CRIMHIST_COLLAPSED, pred.full)

# confusion matrix
table(full.opt$CH_CRIMHIST_COLLAPSED, pred.full) # pretty good ROC

#Set empty placeholders again
error.train <- c()
error.test <- c()
error.val <- c()
