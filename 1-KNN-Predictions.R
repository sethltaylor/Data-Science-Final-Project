### PREDICITIONS: k-nearest Neighbor ####

### NB: MAKE SURE TO HAVE RUN "Cleaning and Theoretical Models.R" FIRST!

"C:/Users/viola/Documents/Studium/MPP GU/Spring 2017/Data Science/Data-Science-Final-Project" # adjust individually

library(class) # for kNN

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

# delete rows with missing variables for recidivism
table(is.na(full.numeric$CH_CRIMHIST_COLLAPSED))

full.k <- full.numeric[is.na(full.numeric$CH_CRIMHIST_COLLAPSED) == F,]

# select variables of interest and only keep complete cases
vars <- c("ID", "CH_CRIMHIST_COLLAPSED", "OFFENSE_VIOLENT", "OFFENSE_DRUG", "SES_PHYSABUSED_EVER", "CS_SENTENCEMTH", 
          "SES_PARENTS_INCARCERATED", "SES_HASCHILDREN", "EDUCATION", "AGE_CAT", "SES_INCOMEMTH",
          "black.nh", "hispanic", "asian", "DRUG_COCRKREG", "SES_SEXABUSED_EVER", "SES_FAMILY_INCARCERATED", "DRUG_ANY_ALCDRUGTME", 
          "DRUG_ANYREG", "DRUG_HROPTME", "DRUG_METHATME", "LIFE_SENTENCE")

full.k <- full.k[, names(full.k) %in% vars]

# see how many complete cases are left
table(complete.cases(full.k)) # this is not good: 14102 would be dropped due to at least one var having a missing observation

missing.k <- vector("numeric", length=21)

for (i in 1:ncol(full.k)){
  missing.k[i] <- length(which(is.na(full.k[,i])==T))
}

cbind(names(full.k), missing.k) 
# DRUG_COCRKREG with 11938 missing values and DRUG_ANY_ALCDRUGTME with 7207 missing values
# also: CS_SENTENCEMTH with 3156 missing values and SES_INCOMEMTH with 3156 missing values

# remove these two variables
full.k <- full.k[, !names(full.k) %in% c("DRUG_COCRKREG", "DRUG_ANY_ALCDRUGTME")]

table(complete.cases(full.k)) # now only 5137 are missing
table(complete.cases(full.k[,!names(full.k) %in% c("CS_SENTENCEMTH", "SES_INCOMEMTH")])) 
# this would further reduce missing vars by 3587 obs -> try later

# remove incomplete cases --> only working with 12960 obs
full.k <- full.k[complete.cases(full.k),]

## deal with factors (factors cause knn error: knn not possible with both categorical and continuous variables)
# there is knncat for categorical values (which uses different measurement of distance)
# but knn cannot handle a mix of continuous and categorical variables
# factors will be converted to numeric just for the purpose of knn predicitions
# this is okay, becasue we are not interpreting numerical partial effects. Finding closest neighbors works if observations are
# given the same numerical value for a level of education
# values preserve ordinal ranking of factors (NB numerical distance between values cannot be interpreted however!)

str(full.k) # AGE_CAT, EDUCATION, and SES_INCOMEMTH are factors
full.k$AGE_CAT <- as.numeric(full.k$AGE_CAT)
full.k$EDUCATION <- as.numeric(full.k$EDUCATION)
full.k$SES_INCOMEMTH <- as.numeric(full.k$SES_INCOMEMTH)


## set aside 15% of full.numeric data for validation purposes (in addition to k-fold testing and training)

smp_size <- floor(0.15 * nrow(full.k))

set.seed(123)
val_ind <- sample(seq_len(nrow(full.k)), size = smp_size)

# partiion using the index created
full.val <- full.k[val_ind, ]
full.train <- full.k[-val_ind, ]


## break train dataset into k-folds

n <- nrow(full.train)
k <- 5 # k is arbitrarily chosen, probably a bit high and therefore time intensive

full.train$folds <- kfolds.index(n, k)

#Set placeholder for (mean) errors
error.train <- c() # will calculate meanf1 for train data
error.test <- c() # will calculate meanf1 for test data
error.val <- c() # will calculate meanf1 for validation data set aside previously

#Set placeholder for dataframe that shows train, test, and validate errors for all Models trained
model.errors <- data.frame("models" = character(12), "error.train"= numeric(12), 
                           "error.test" = numeric(12), "error.val" = numeric(12), "error.full" = numeric(12))
model.errors$models <- c("KNN.3.18v", "KNN.k5", "KNN.1", "KNN.10", "KNN.3.16v", "KNN.1.16v", 
                         "DT.default", "DT.0", "DT.opt", "RF.default", "RF.16v", "RF.tuned")


### MODELS A) TRAINING KNN ####

## MODEL A.a) TRAINING KNN WITH K=3 ###

#Run kfolds
for(k in 1:5){
  
  #Cut train/test
  train.k <- full.train[full.train$folds != k, ]
  test.k <- full.train[full.train$folds == k, ]
  
  #vector y for index of columns that are not part of x
  y <- c("ID", "CH_CRIMHIST_COLLAPSED", "folds")
  
  #KNN predictions for train, test, and validate
  pred.train <- knn(train.k[,!names(train.k) %in% y], train.k[,!names(train.k) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=3)
  pred.test <- knn(train.k[,!names(train.k) %in% y], test.k[,!names(test.k) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=3)
  pred.val <- knn(train.k[,!names(train.k) %in% y], full.val[,!names(full.val) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=3)
  
  #calc error, log it
  error.train <- c(error.train, meanf1(train.k$CH_CRIMHIST_COLLAPSED, pred.train))
  error.test <- c(error.test, meanf1(test.k$CH_CRIMHIST_COLLAPSED, pred.test))
  error.val <- c(error.val, meanf1(full.val$CH_CRIMHIST_COLLAPSED, pred.val)) 
}

# add mean error values to model.errors
model.errors[1,c(2:4)] <- c(mean(error.train), mean(error.test), mean(error.val)) # only around 68% (good on train)

#Set empty placeholders again
error.train <- c()
error.test <- c()
error.val <- c()

# rerun Model on full dataset
pred.full <- knn(full.k[,!names(full.k) %in% y], full.k[,!names(full.k) %in% y], cl=full.k$CH_CRIMHIST_COLLAPSED, k=3)
model.errors[1,5] <- meanf1(full.k$CH_CRIMHIST_COLLAPSED, pred.full)

# confusion matrix
table(full.k$CH_CRIMHIST_COLLAPSED, pred.full) 

## MODEL A.b) TRAINING KNN with K=5 ###
#Run kfolds
for(k in 1:5){
  
  #Cut train/test
  train.k <- full.train[full.train$folds != k, ]
  test.k <- full.train[full.train$folds == k, ]
  
  #vector y for index of columns that are not part of x
  y <- c("ID", "CH_CRIMHIST_COLLAPSED", "folds")
  
  #KNN predictions for train, test, and validate
  pred.train <- knn(train.k[,!names(train.k) %in% y], train.k[,!names(train.k) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=5)
  pred.test <- knn(train.k[,!names(train.k) %in% y], test.k[,!names(test.k) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=5)
  pred.val <- knn(train.k[,!names(train.k) %in% y], full.val[,!names(full.val) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=5)
  
  #calc error, log it
  error.train <- c(error.train, meanf1(train.k$CH_CRIMHIST_COLLAPSED, pred.train))
  error.test <- c(error.test, meanf1(test.k$CH_CRIMHIST_COLLAPSED, pred.test))
  error.val <- c(error.val, meanf1(full.val$CH_CRIMHIST_COLLAPSED, pred.val)) 
}

# add mean error values to model.errors
model.errors[2,c(2:4)] <- c(mean(error.train), mean(error.test), mean(error.val)) # better than k3

# rerun Model on full dataset
pred.full <- knn(full.k[,!names(full.k) %in% y], full.k[,!names(full.k) %in% y], cl=full.k$CH_CRIMHIST_COLLAPSED, k=5)
model.errors[2,5] <- meanf1(full.k$CH_CRIMHIST_COLLAPSED, pred.full)

# confusion matrix
table(full.k$CH_CRIMHIST_COLLAPSED, pred.full) #even more false positives

#Set empty placeholders again
error.train <- c()
error.test <- c()
error.val <- c()


## MODEL A.c) TRAINING KNN with K=1 ###
#Run kfolds
for(k in 1:5){
  
  #Cut train/test
  train.k <- full.train[full.train$folds != k, ]
  test.k <- full.train[full.train$folds == k, ]
  
  #vector y for index of columns that are not part of x
  y <- c("ID", "CH_CRIMHIST_COLLAPSED", "folds")
  
  #KNN predictions for train, test, and validate
  pred.train <- knn(train.k[,!names(train.k) %in% y], train.k[,!names(train.k) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=1)
  pred.test <- knn(train.k[,!names(train.k) %in% y], test.k[,!names(test.k) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=1)
  pred.val <- knn(train.k[,!names(train.k) %in% y], full.val[,!names(full.val) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=1)
  
  #calc error, log it
  error.train <- c(error.train, meanf1(train.k$CH_CRIMHIST_COLLAPSED, pred.train))
  error.test <- c(error.test, meanf1(test.k$CH_CRIMHIST_COLLAPSED, pred.test))
  error.val <- c(error.val, meanf1(full.val$CH_CRIMHIST_COLLAPSED, pred.val)) 
}

# add mean error values to model.errors
model.errors[3,c(2:4)] <- c(mean(error.train), mean(error.test), mean(error.val)) # very good for train, but no generalizability

# rerun Model on full dataset
pred.full <- knn(full.k[,!names(full.k) %in% y], full.k[,!names(full.k) %in% y], cl=full.k$CH_CRIMHIST_COLLAPSED, k=1)
model.errors[3,5] <- meanf1(full.k$CH_CRIMHIST_COLLAPSED, pred.full)

# confusion matrix
table(full.k$CH_CRIMHIST_COLLAPSED, pred.full) # very few FP and FN, but no generalizability

#Set empty placeholders again
error.train <- c()
error.test <- c()
error.val <- c()


## MODEL A.d) TRAINING KNN with K=10 ###
#Run kfolds
for(k in 1:5){
  
  #Cut train/test
  train.k <- full.train[full.train$folds != k, ]
  test.k <- full.train[full.train$folds == k, ]
  
  #vector y for index of columns that are not part of x
  y <- c("ID", "CH_CRIMHIST_COLLAPSED", "folds")
  
  #KNN predictions for train, test, and validate
  pred.train <- knn(train.k[,!names(train.k) %in% y], train.k[,!names(train.k) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=10)
  pred.test <- knn(train.k[,!names(train.k) %in% y], test.k[,!names(test.k) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=10)
  pred.val <- knn(train.k[,!names(train.k) %in% y], full.val[,!names(full.val) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=10)
  
  #calc error, log it
  error.train <- c(error.train, meanf1(train.k$CH_CRIMHIST_COLLAPSED, pred.train))
  error.test <- c(error.test, meanf1(test.k$CH_CRIMHIST_COLLAPSED, pred.test))
  error.val <- c(error.val, meanf1(full.val$CH_CRIMHIST_COLLAPSED, pred.val)) 
}

# add mean error values to model.errors
model.errors[4,c(2:4)] <- c(mean(error.train), mean(error.test), mean(error.val)) # worse than k3

# rerun Model on full dataset
pred.full <- knn(full.k[,!names(full.k) %in% y], full.k[,!names(full.k) %in% y], cl=full.k$CH_CRIMHIST_COLLAPSED, k=10)
model.errors[4,5] <- meanf1(full.k$CH_CRIMHIST_COLLAPSED, pred.full)

# confusion matrix
table(full.k$CH_CRIMHIST_COLLAPSED, pred.full) # worse than k=3 and k=5: too many FPs!

#Set empty placeholders again
error.train <- c()
error.test <- c()
error.val <- c()


## MODEL A.e) TRAINING KNN WITH K=3 w/o length of sentence and income to get >3k obs more

full.k <- full.numeric[, names(full.numeric) %in% vars]

full.k <- full.k[,!names(full.k) %in% c("CS_SENTENCEMTH", "SES_INCOMEMTH","DRUG_COCRKREG", "DRUG_ANY_ALCDRUGTME")]

full.k <- full.k[complete.cases(full.k),]
full.k$AGE_CAT <- as.numeric(full.k$AGE_CAT)
full.k$EDUCATION <- as.numeric(full.k$EDUCATION)

## set aside 15% of full.numeric data for validation purposes (in addition to k-fold testing and training)

smp_size <- floor(0.15 * nrow(full.k))

set.seed(123)
val_ind <- sample(seq_len(nrow(full.k)), size = smp_size)

# partiion using the index created
full.val <- full.k[val_ind, ]
full.train <- full.k[-val_ind, ]

## break train dataset into k-folds

n <- nrow(full.train)
k <- 5 # k is arbitrarily chosen, probably a bit high and therefore time intensive

full.train$folds <- kfolds.index(n, k)

#Run kfolds
for(k in 1:5){
  
  #Cut train/test
  train.k <- full.train[full.train$folds != k, ]
  test.k <- full.train[full.train$folds == k, ]
  
  #vector y for index of columns that are not part of x
  y <- c("ID", "CH_CRIMHIST_COLLAPSED", "folds")
  
  #KNN predictions for train, test, and validate
  pred.train <- knn(train.k[,!names(train.k) %in% y], train.k[,!names(train.k) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=3)
  pred.test <- knn(train.k[,!names(train.k) %in% y], test.k[,!names(test.k) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=3)
  pred.val <- knn(train.k[,!names(train.k) %in% y], full.val[,!names(full.val) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=3)
  
  #calc error, log it
  error.train <- c(error.train, meanf1(train.k$CH_CRIMHIST_COLLAPSED, pred.train))
  error.test <- c(error.test, meanf1(test.k$CH_CRIMHIST_COLLAPSED, pred.test))
  error.val <- c(error.val, meanf1(full.val$CH_CRIMHIST_COLLAPSED, pred.val)) 
}

# add mean error values to model.errors
model.errors[5,c(2:4)] <- c(mean(error.train), mean(error.test), mean(error.val))
# best prediction thus far

# rerun Model on full dataset
pred.full <- knn(full.k[,!names(full.k) %in% y], full.k[,!names(full.k) %in% y], cl=full.k$CH_CRIMHIST_COLLAPSED, k=3)
model.errors[5,5] <- meanf1(full.k$CH_CRIMHIST_COLLAPSED, pred.full)

# confusion matrix
table(full.k$CH_CRIMHIST_COLLAPSED, pred.full) 
# --> mean F1s are better than with 18 variables, but higher absolute number of FPs

#Set empty placeholders again
error.train <- c()
error.test <- c()
error.val <- c()


## MODEL A.f) k=1 for 16 Vars

#Run kfolds
for(k in 1:5){
  
  #Cut train/test
  train.k <- full.train[full.train$folds != k, ]
  test.k <- full.train[full.train$folds == k, ]
  
  #vector y for index of columns that are not part of x
  y <- c("ID", "CH_CRIMHIST_COLLAPSED", "folds")
  
  #KNN predictions for train, test, and validate
  pred.train <- knn(train.k[,!names(train.k) %in% y], train.k[,!names(train.k) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=1)
  pred.test <- knn(train.k[,!names(train.k) %in% y], test.k[,!names(test.k) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=1)
  pred.val <- knn(train.k[,!names(train.k) %in% y], full.val[,!names(full.val) %in% y], cl=train.k$CH_CRIMHIST_COLLAPSED, k=1)
  
  #calc error, log it
  error.train <- c(error.train, meanf1(train.k$CH_CRIMHIST_COLLAPSED, pred.train))
  error.test <- c(error.test, meanf1(test.k$CH_CRIMHIST_COLLAPSED, pred.test))
  error.val <- c(error.val, meanf1(full.val$CH_CRIMHIST_COLLAPSED, pred.val)) 
}

# add mean error values to model.errors
model.errors[6,c(2:4)] <- c(mean(error.train), mean(error.test), mean(error.val))
# prediction better with k=3

# rerun Model on full dataset
pred.full <- knn(full.k[,!names(full.k) %in% y], full.k[,!names(full.k) %in% y], cl=full.k$CH_CRIMHIST_COLLAPSED, k=1)
model.errors[6,5] <- meanf1(full.k$CH_CRIMHIST_COLLAPSED, pred.full)

# confusion matrix
table(full.k$CH_CRIMHIST_COLLAPSED, pred.full) # better predicitions on train, but less generalizability than k=3

#Set empty placeholders again
error.train <- c()
error.test <- c()
error.val <- c()

