
setwd("C:/Users/Seth/Documents") # adjust individually

#install.packages("memisc") 
library(memisc)

#### 1) DATA EXTRACTION ####

## Extract .rda files for Federal and State Analysis data from Github #

# This function assigns the loaded data frame the desired name directly

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# Federal Analysis Data
url <- "https://github.com/GeorgetownMcCourt/Predicting-Recidivism/raw/master/Data/FederalAnalysisR.rda"
temp = tempfile() #Create temp file
download.file(url, temp) #download the URL direct to the temp file
fed.an <- loadRData(temp)


# State Analysis Data
url <- "https://github.com/GeorgetownMcCourt/Predicting-Recidivism/raw/master/Data/StateAnalysisR.rda"
temp = tempfile() #Create temp file
download.file(url, temp) ##download the URL direct to the temp file
state.an <- loadRData(temp)

#Creates a dummy for State T/F before combining fed.an and state.an
state.an$state <- TRUE
fed.an$state <- FALSE

full.an <- rbind(fed.an,state.an)

# rename first column (id's) and change to character
names(full.an)[1] <- "ID"
full.an$ID <- as.character(full.an$ID)

#### 2) DATA CLEANING ####

## ultimate goal: dataset that is numeric only (apart from ID Variable)

# replicate dataset in order to check back with orignial dataset if all changes made are true to data
full.numeric <- full.an

# create list with factor levels to get idea of categorical values
levels.list <- vector("list", length = ncol(full.numeric))

for (i in 2:ncol(full.numeric)) {
  if (is.factor(full.numeric[,i]) == T) {
    levels.list[[i]] <- levels(full.numeric[,i]) # extract factor levels
  }
}


## Start with Factors with 3 Levels: Some Form of Yes, No, Missing

# change factor labels for harmonization
for (i in 2:ncol(full.numeric)) {
  if (is.factor(full.numeric[,i]) == T & length(levels(full.numeric[,i])) == 3) {
    
    # remove punctuation, spaces, and alphabetic expressions
    levels(full.numeric[,i]) <- gsub("[[:punct:]]", "", levels(full.numeric[,i]))
    levels(full.numeric[,i]) <- gsub("[[:space:]]", "", levels(full.numeric[,i]))
    levels(full.numeric[,i]) <- gsub("[[:alpha:]]", "", levels(full.numeric[,i]))
    # now the factor label should be numeric only
    
  }
}

# convert into character vector and recode
for (i in 2:ncol(full.numeric)) {
  if (is.factor(full.numeric[,i]) == T & length(levels(full.numeric[,i])) == 3) {
    
    full.numeric[,i] <- as.character(full.numeric[,i])
    
    full.numeric[,i] <- recode(full.numeric[,i],
                               1 <- "0000001",
                               0 <- c("0000000", "0000002"),
                               otherwise = NA)
    
    full.numeric[,i] <- as.numeric(as.character(full.numeric[,i]))
    
  }
}


## Factors with 4 Levels: Some Form of Yes, No, Missing
for (i in 2:ncol(full.numeric)) {
  if (is.factor(full.numeric[,i]) == T & length(levels(full.numeric[,i])) == 4) {
    
    # remove punctuation, spaces, and alphabetic expressions
    levels(full.numeric[,i]) <- gsub("[[:punct:]]", "", levels(full.numeric[,i]))
    levels(full.numeric[,i]) <- gsub("[[:space:]]", "", levels(full.numeric[,i]))
    levels(full.numeric[,i]) <- gsub("[[:alpha:]]", "", levels(full.numeric[,i]))
    # now the factor label should be numeric only
    
  }
}

# convert into character vector and recode
for (i in 2:ncol(full.numeric)) {
  if (is.factor(full.numeric[,i]) == T & length(levels(full.numeric[,i])) == 4) {
    
    full.numeric[,i] <- as.character(full.numeric[,i])
    
    full.numeric[,i] <- recode(full.numeric[,i],
                               1 <- "0000001",
                               0 <- c("0000002", "0000004"),
                               otherwise = NA)
    
    full.numeric[,i] <- as.numeric(as.character(full.numeric[,i]))
    
  }
}


### Generate recidivism variable

# NB: the recoding for this 4-level variable is completely wrong, because 0000001 = NO in this case; 0000002-0000003 = YES

full.numeric$CH_CRIMHIST_COLLAPSED <- full.an$CH_CRIMHIST_COLLAPSED # reassign original variable

full.numeric$CH_CRIMHIST_COLLAPSED <- as.character(full.numeric$CH_CRIMHIST_COLLAPSED) #convert to character

full.numeric$CH_CRIMHIST_COLLAPSED <- recode(full.numeric$CH_CRIMHIST_COLLAPSED, #recode
                                             0 <- "(0000001) First timers",
                                             1 <- c("(0000002) Recidivist, current or past violent offense", 
                                                    "(0000003) Recidivist, no current or prior violent offense"),
                                             otherwise = NA)
full.numeric$CH_CRIMHIST_COLLAPSED <- as.numeric(as.character(full.numeric$CH_CRIMHIST_COLLAPSED))

# table post changes to make sure recoding was right
table(full.numeric$CH_CRIMHIST_COLLAPSED)
table(full.an$CH_CRIMHIST_COLLAPSED)

# Create Race Dummies

full.numeric$hispanic <- NA
full.numeric$hispanic <- ifelse(full.numeric$RACE == "(0000003) Hispanic", full.numeric$hispanic <- 1,
                                ifelse(full.numeric$RACE == "(9999999) Missing", full.numeric$hispanic <- NA, 0))

full.numeric$black.nh <- NA
full.numeric$black.nh <- ifelse(full.numeric$RACE == "(0000002) Black non-hispanic", full.numeric$black.nh <- 1,
                                ifelse(full.numeric$RACE == "(9999999) Missing", full.numeric$black.nh <- NA, 0))

full.numeric$asian <- NA
full.numeric$asian <- ifelse(full.numeric$RACE == "(0000005) Asian, pacific islander, native hawaiian non-hispanic", 
                             full.numeric$asian <- 1,
                             ifelse(full.numeric$RACE == "(9999999) Missing", full.numeric$asian <- NA, 0))


## Recode CS_SENTENCEMTH (Length of Sentence in Month)

full.numeric$CS_SENTENCEMTH[full.numeric$CS_SENTENCEMTH > 10000] <- NA ## convert all Missings

full.numeric$CS_SENTENCEMTH <- as.numeric(full.numeric$CS_SENTENCEMTH) ## NB: Length of 10,000 == Life or Death Sentence

full.numeric$LIFE_SENTENCE <- ifelse(full.numeric$CS_SENTENCEMTH == 10000, 1, 0) ## Creates variable for life sentence

full.numeric$CS_SENTENCEMTH[full.numeric$CS_SENTENCEMTH == 10000] <- NA ## Converts life sentence in months to NA

##Recode SES_PARENTS_INCARCERATED, SES_HASCHILDREN, SES_FAMILY_INCARCERATED

vars <- c("SES_PARENTS_INCARCERATED", "SES_HASCHILDREN", "SES_FAMILY_INCARCERATED")

# Removes punctuation, spaces, and alphabetic expressions

for (i in vars) {
  levels(full.numeric[,i]) <- gsub("[[:punct:]]", "", levels(full.numeric[,i]))
  levels(full.numeric[,i]) <- gsub("[[:space:]]", "", levels(full.numeric[,i]))
  levels(full.numeric[,i]) <- gsub("[[:alpha:]]", "", levels(full.numeric[,i]))
  # now the factor label should be numeric only
}

# Converts into a character vector and recodes

for (i in vars) {
  full.numeric[,i] <- as.character(full.numeric[,i])
  
  full.numeric[,i] <- recode(full.numeric[,i],
                             1 <- "0000001",
                             0 <- c("0000002"),
                             otherwise = NA)
  full.numeric[,i] <- as.numeric(as.character(full.numeric[,i]))
}


###Setting up Train/Test/Validate###
set.seed(42)
rand <- runif(nrow(full.numeric))

trainset <- full.numeric[rand >= 0.3,]
testset <- full.numeric[rand >= 0.15 & rand < 0.3,]
valset <- full.numeric[rand < 0.15,]


#Set up Mean-F1#
meanf1 <- function(actual, predicted){
  
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

###First Predictive Model###

glm.fit <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                 SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + AGE_CAT +
                 SES_SEXABUSED_EVER + DRUG_ANYREG + DRUG_ANYTME + black.nh + hispanic + asian + state, 
               data = trainset,
               family = binomial(),
               na.action = na.exclude)

summary(glm.fit)

#Predict Train and Validate#
predict.glm.train <- predict(glm.fit, trainset, type = "response")
predict.glm.val <- predict(glm.fit, valset, type = "response")

#Making sure casewise deletion applies 1:1 b/w our original data and our predicted data. Otherwise Mean-F1 returns NA
train.history <- NA
for(i in 1:length(predict.glm.train)){
  if(is.na(predict.glm.train[i]) == FALSE){
    train.history[i] <- trainset$CH_CRIMHIST_COLLAPSED[i]
  } 
}

val.history <- NA
for(i in 1:length(predict.glm.val)){
  if(is.na(predict.glm.val[i]) == FALSE){
    val.history[i] <- valset$CH_CRIMHIST_COLLAPSED[i]
  } 
}

# now if you table is.na for val.history and predict.glm.val, they match. Same for train

##Mean F1 Calculations for cutoff of 0.5##

#Applying predicted labels
train.recid <- predict.glm.train > 0.5
train.recid[train.recid == TRUE] <- "Recidivist"
train.recid[train.recid == FALSE] <- "First Timer"

#Applying labels to our original set
train.history[train.history == 1] <- "Recidivist"
train.history[train.history == 0] <- "First Timer"

table(is.na(train.recid[which(is.na(train.history))])) # demonstrates that NAs are at the same place for train.recid and train.history

# thus, we can just exclude NAs for both
train.recid <- train.recid[is.na(train.recid)==F]
train.history <- train.history[is.na(train.history)==F]

meanf1(train.history, train.recid) # now this gives you the correct Mean F1 --> much lower!


#Checking confusion matrix#
table(train.recid, train.history) #High sensitivity, but low specificity. Probably not what we want. Adjusting cutoff

#Applying predicted labels to predicted set
val.recid <- predict.glm.val > 0.5
val.recid[val.recid == TRUE] <- "Recidivist"
val.recid[val.recid == FALSE] <- "First Timer"

#Applying labels to our original set
val.history[val.history == 1] <- "Recidivist"
val.history[val.history == 0] <- "First Timer"

table(is.na(val.recid[which(is.na(val.history))])) # demonstrates that NAs are at the same place for train.recid and train.history

# thus, we can just exclude NAs for both
val.recid <- val.recid[is.na(val.recid)==F]
val.history <- val.history[is.na(val.history)==F]

meanf1(val.history, val.recid) # again, much lower!

#Checking confusion matrix#
table(val.recid, val.history) #High sensitivity, but low specificity. Probably not what we want. Adjusting cutoff


##Mean F1 calculations for cutoff of 0.75##

#Applying predicted labels
train.recid <- predict.glm.train > 0.75
train.recid[train.recid == TRUE] <- "Recidivist"
train.recid[train.recid == FALSE] <- "First Timer"

# exclude NAs again
train.recid <- train.recid[is.na(train.recid)==F]

# Mean F1
meanf1(train.history, train.recid) #too bad

#Checking confusion matrix#
table(train.recid, train.history) #High specificity, but middling sensitivity. Maybe not what we want. Would suggest cuttof between .5 and .75


#Applying predicted labels to predicted set
val.recid <- predict.glm.val > 0.75
val.recid[val.recid == TRUE] <- "Recidivist"
val.recid[val.recid == FALSE] <- "First Timer"

# exclude NAs again
val.recid <- val.recid[is.na(val.recid)==F]

# Mean F1
meanf1(val.history, val.recid) #too bad

#Checking confusion matrix#
table(val.recid, val.history) #Highish specificity, but lowish sensitivity.  Maybe not what we want. Would suggest cuttof between .5 and .75
