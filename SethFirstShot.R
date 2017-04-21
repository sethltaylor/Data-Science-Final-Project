
setwd("/Users/Seth/Documents")

## This is code Jeff gave me to read-in zipped github files, but it's not working for some reason ## 

# url <- "https://github.com/GeorgetownMcCourt/Predicting-Recidivism/raw/master/Data/FedData.zip" #url + file name goes here
# temp = tempfile() #Create temp file
# download.file(url, temp) ##download the URL direct to the temp file
# unzip(temp, exdir=getwd()) ##unzip that file
# out <- read.csv(temp, header = TRUE)

fed <- read.csv("C:/Users/Seth/Documents/GitHub/Predicting-Recidivism/Data/FedData.csv")
state <- read.csv("C:/Users/Seth/Documents/GitHub/Predicting-Recidivism/Data/StateData.csv")

fulldata <- rbind(fed,state)

load("C:/Users/Seth/Documents/GitHub/Predicting-Recidivism/Data/FedAnalysisR.Rdata")
load("C:/Users/Seth/Documents/GitHub/Predicting-Recidivism/Data/StateAnalysisR.Rdata")

fedan <- da04572.0003
statean <- da04572.0004

analysisdata <- rbind(fedan,statean)


#### Cleaning Function ####

clean1 <- function(variable){
  levels(variable)[levels(variable) == "(1) Yes"] <- 1
  levels(variable)[levels(variable) == "(2) No"] <- 0
  levels(variable)[levels(variable) == "(0000001) Has children"] <- 1
  levels(variable)[levels(variable) == "(0000002) Does not have children"] <- 0
  levels(variable)[levels(variable) == "(0000000) No"] <- 0
  levels(variable)[levels(variable) == "(0000002) No"] <- 0
  levels(variable)[levels(variable) == "(0000001) Yes"] <- 1
  levels(variable)[levels(variable) == "(9999998) DK/Refused/Missing"] <- NA
  levels(variable)[levels(variable) == "(9999997) Don't know"] <- NA
  levels(variable)[levels(variable) == "(9999998) Refused"] <- NA
  levels(variable)[levels(variable) == "(9999999) Missing"] <- NA
  levels(variable)[levels(variable) == "(7) Unknown"] <- NA
   return(as.factor(variable))
}


#Generate recidivism variable#
clean1(analysisdata$CH_CRIMHIST_COLLAPSED)
analysisdata$recidivism[analysisdata$CH_CRIMHIST_COLLAPSED == "(0000001) First timers"] <- 0
analysisdata$recidivism[analysisdata$CH_CRIMHIST_COLLAPSED == "(0000002) Recidivist, current or past violent offense"] <- 1
analysisdata$recidivism[analysisdata$CH_CRIMHIST_COLLAPSED == "(0000003) Recidivist, no current or prior violent offense"] <- 1
analysisdata$recidivism <- as.factor(analysisdata$recidivism)

#Clean certain variables#    ###There is probably a better way to do this as a loop, but I couldn't get it to work correctly
analysisdata$VETERAN <- clean1(analysisdata$VETERAN)
analysisdata$AGE_CAT <- clean1(analysisdata$AGE_CAT)
analysisdata$RACE <- clean1(analysisdata$RACE)
analysisdata$OFFENSE_VIOLENT <- clean1(analysisdata$OFFENSE_VIOLENT)
analysisdata$OFFENSE_DRUG <- clean1(analysisdata$OFFENSE_DRUG)
analysisdata$OFFENSE_PROPERTY <- clean1(analysisdata$OFFENSE_PROPERTY)
analysisdata$OFFENSE_PUBLICORDER <- clean1(analysisdata$OFFENSE_PUBLICORDER)
analysisdata$SES_PHYSABUSED_EVER <- clean1(analysisdata$SES_PHYSABUSED_EVER)
analysisdata$SES_SEXABUSED_EVER <- clean1(analysisdata$SES_SEXABUSED_EVER)

## Mean F1 ##
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


###70-15-15 Setup###

set.seed(42)
rand <- runif(nrow(analysisdata))

trainset <- analysisdata[rand >= 0.3,]
testset <- analysisdata[rand >= 0.15 & rand < 0.3,]
valset <- analysisdata[rand < 0.15,]

### GLM predictions ###

#glm.fit <- glm(#formula here#, data = train, family = binomial())

#Predict Train

trainset$pred <- predict(glm.fit, newdata = trainset, type = "response")

meanf1(trainset$recidivism, trainset$pred)

#Predict Validate

valset$pred <- predict(glm.fit, newdata = valset, type = "response")

meanf1(valset$recidvisim, trainset$pred)

#Predict Test if Validate Mean-f1 is satisfactory. Otherwise, iterate on model

testset$pred <- predict(glm.fit, newdata = testset, type = "response")

meanf1(testset$recidivism, trainset$pred)


###K-folds Setup Attempt###   # I don't really get k-folds so I'm sure this will need to be fixed if we want to use k-folds
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

#k = n
k <- nrow(analysisdata)
n <- nrow(analysisdata)

#Set folds
analysisdata$folds <- kfolds.index(n, k)


#Run kfolds 
for(k in unique(analysisdata$folds)){
  
  #Cut train/test
  train <- iris[analysisdata$folds != k, ]
  test <- iris[analysisdata$folds == k, ]

  #estimate model
  
  #fit <- glm(##put formula here##, data = train, family = binomial())
  
  train$pred <- predict(glm.fit, newdata = train, type = "response")
  train$rest <- predict(glm.fit, newdata = test, type = "response")
  
}

meanf1(train$recidivism, train$pred)
meanf1(test$recidivism, test$pred)

























##### Shashank Previous Code. Moved down here #####

summary(mydata)
unique(mydata$CH_CRIMHIST)
unique(mydata$CH_CRIMHIST_COLLAPSED)
unique(mydata$CH_CRIM_HISTORY)

summary(mydata$CH_CRIM_HISTORY) #Variable for recidivism

#Data Structure
str(mydata, list.len=ncol(mydata))
#Most variables are factor variables with well defined labels



################################
######### NUMERIC VARIABLES #################

#I look at Non-Factor Variables first to correct for any missingness
summary(mydata$CS_SENTENCEMTH) #SENTENCE LENGTH IN MONTHS
summary(mydata$CH_EXPECTEDTIMEMTH) #TIME TO BE SERVED TO EXPECTED DATE OF RELEASE IN MONTHS
summary(mydata$CH_SERVEDMTH) #TIME SERVED TO DATE OF INTERVIEW IN MONTHS
summary(mydata$CS_INCDJ) #PRIOR INCARCERATIONS AS JUVENILE
summary(mydata$CS_INCDA) #PRIOR INCARCERATIONS AS ADULT
summary(mydata$CS_INCJ) #PRIOR INCARCERATIONS AS JUVENILE
summary(mydata$CS_INCA) #PRIOR INCARCERATIONS AS ADULT
summary(mydata$CH_NUMCAR) #NUMBER OF INCARCERATIONS
summary(mydata$IC_MANYVICT_NUMOFVICT) #VICTIMS OF VIOLENT CRIME - MORE THAN ONE VICTIM - HOW MANY PERSONS WERE VICTIMS?
summary(mydata$CH_PRIORARREST_CAT) # # OF PRIOR ARRESTS CATEGORIES

#Accounting for missing values
#CS_SENTENCEMTH #SENTENCE LENGTH IN MONTHS
unique(mydata$CS_SENTENCEMTH)
mydata$CS_SENTENCEMTH[mydata$CS_SENTENCEMTH > 10000] <- NA #10000 is a life sentence. Should give it a different numeric value
summary(mydata$CS_SENTENCEMTH)

#CS_SENTENCEMTH #TIME TO BE SERVED TO EXPECTED DATE OF RELEASE IN MONTHS
unique(mydata$CH_EXPECTEDTIMEMTH)
mydata$CH_EXPECTEDTIMEMTH[mydata$CH_EXPECTEDTIMEMTH > 10000] <- NA
summary(mydata$CH_EXPECTEDTIMEMTH)

#CH_SERVEDMTH #TIME SERVED TO DATE OF INTERVIEW IN MONTHS
unique(mydata$CH_SERVEDMTH) #TIME SERVED TO DATE OF INTERVIEW IN MONTHS
mydata$CH_SERVEDMTH[mydata$CH_SERVEDMTH > 10000] <- NA
summary(mydata$CH_SERVEDMTH)

#CS_INCDJ #PRIOR INCARCERATIONS AS JUVENILE
unique(mydata$CS_INCDJ)

#CS_INCDA #PRIOR INCARCERATIONS AS ADULT
unique(mydata$CS_INCDA)

#CS_INCJ #PRIOR INCARCERATIONS AS JUVENILE
unique(mydata$CS_INCJ)

#CS_INCA #PRIOR INCARCERATIONS AS ADULT
unique(mydata$CS_INCA) 

#CH_NUMCAR #NUMBER OF INCARCERATIONS
unique(mydata$CH_NUMCAR)
mydata$CH_NUMCAR[mydata$CH_NUMCAR > 9990] <- NA

#IC_MANYVICT_NUMOFVICT #VICTIMS OF VIOLENT CRIME - MORE THAN ONE VICTIM - HOW MANY PERSONS WERE VICTIMS?
unique(mydata$IC_MANYVICT_NUMOFVICT)
mydata$IC_MANYVICT_NUMOFVICT[mydata$IC_MANYVICT_NUMOFVICT > 1000] <- NA

#CH_PRIORARREST_CAT # # OF PRIOR ARRESTS CATEGORIES
unique(mydata$CH_PRIORARREST_CAT)
mydata$CH_PRIORARREST_CAT[mydata$CH_PRIORARREST_CAT > 1000] <- NA

##########################################

##########################################
################ FACTOR VARIABLES ##########################

#Race: White = 1, Black = 2, Hispanic = 3, Others = 4 (Others include (American indian, alaska native non-hispanic)
# (Asian, pacific islander, native hawaiian non-hispanic), (Multiple races reported, non-hispanic), (Missing))

levels(mydata$RACE)[levels(mydata$RACE) == "(0000001) White non-hispanic"] <- 1
levels(mydata$RACE)[levels(mydata$RACE) == "(0000002) Black non-hispanic"] <- 2
levels(mydata$RACE)[levels(mydata$RACE) == "(0000003) Hispanic"] <- 3
levels(mydata$RACE)[levels(mydata$RACE) == "(0000004) American indian, alaska native non-hispanic"] <- 4
levels(mydata$RACE)[levels(mydata$RACE) == "(0000005) Asian, pacific islander, native hawaiian non-hispanic"] <- 4
levels(mydata$RACE)[levels(mydata$RACE) == "(0000006) Multiple races reported, non-hispanic"] <- 4
levels(mydata$RACE)[levels(mydata$RACE) == "(9999999) Missing"] <- 4
mydata$RACE <- as.numeric(mydata$RACE) 
summary(mydata$RACE)


#Clean Function for Yes/No/Missing in Factor Variables
clean1 <- function(variable){
  as.numeric(levels(variable)[levels(variable) == "(1) Yes"] <- 1)
  as.numeric(levels(variable)[levels(variable) == "(2) No"] <- 0)
  as.numeric(levels(variable)[levels(variable) == "(0000001) Has children"] <- 1)
  as.numeric(levels(variable)[levels(variable) == "(0000002) Does not have children"] <- 0)
  as.numeric(levels(variable)[levels(variable) == "(0000000) No"] <- 0)
  as.numeric(levels(variable)[levels(variable) == "(0000002) No"] <- 0)
  as.numeric(levels(variable)[levels(variable) == "(0000001) Yes"] <- 1)
  as.numeric(levels(variable)[levels(variable) == "(9999998) DK/Refused/Missing"] <- NA)
  as.numeric(levels(variable)[levels(variable) == "(9999997) Don't know"] <- NA)
  as.numeric(levels(variable)[levels(variable) == "(9999998) Refused"] <- NA)
  as.numeric(levels(variable)[levels(variable) == "(9999999) Missing"] <- NA)
  as.numeric(levels(variable)[levels(variable) == "(7) Unknown"] <- NA)
  return(as.numeric(variable))
}

#Veterans
clean1(mydata$VETERAN)
str(mydata$VETERAN)

#Keep Age in factors [Should we convert missing to NAs? Right now it appears as a factor]
#Age Category 1
summary(mydata$AGE_CAT)
#Age Category2
summary(mydata$CAT_AGE2)
#Age Categor3
summary(mydata$CAT_AGE3)

#Converting Yes-No Factor Variables to dummy variables, leaving multiple category variables as factor variables
summary(mydata$OFFENSE_VIOLENT)
clean1(mydata$OFFENSE_PROPERTY)
clean1(mydata$OFFENSE_DRUG)
clean1(mydata$SES_PHYSABUSED_EVER)
clean1(mydata$SES_SEXABUSED_EVER)
clean1(mydata$SES_HASCHILDREN)
clean1(mydata$SES_PARENTS_INCARCERATED)

#Not Working!!!


summary(mydata$SES_PHYSSEXABUSED_EVER) #Multiple Categories
summary(mydata$SES_AGEOFF_PHYSSEXABUSED) #Multiple Categories
summary(mydata$SES_AGEPERP_PHYSSEXABUSED) #Multiple Categories
summary(mydata$SES_OFF_RAPED) #Multiple Categories




##########################################
#Converting Factor Variables to Numeric
mydata <- sapply(mydata, as.numeric)

cortable <- cor(mydata, method = c("pearson", "kendall", "spearman"))

head(cortable, list.len = ncol(cortable))
cor(x = mydata$CH_CRIM_HISTORY, mydata$RACE)

#Variables with highest correlation with recidivism
$CH_PRIORSENTENCE #not useful
$CH_CRIMHIST #not useful
$CH_PRIORSENTENCE_NUM #not useful 
$CH_PROBATION #maybe useful
$CH_INCARCERATION #no
$CH_PROBATION_NUM #maybe
$CH_INCARCERATION_NUM #no
# 30 odd more such variables with correlation higher than 0.1. This is of course a very basic way to get to a model.
#Secondly, might be useful to run the same code for federal and states differently to spot any differences
###