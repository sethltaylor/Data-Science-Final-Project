
setwd("C:/Users/Seth/Documents) # adjust individually

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


# check sample variables to make sure recoding made sense
table(full.numeric$OFFENSE_VIOLENT)
table(full.an$OFFENSE_VIOLENT)

table(full.numeric$SES_SEXABUSED_EVER)
table(full.an$SES_SEXABUSED_EVER)


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


# check sample variables to make sure recoding made sense
table(full.numeric$DRUG_STIMU)
table(full.an$DRUG_STIMU)

table(full.numeric$SES_PERP_SEX_FRIEND)
table(full.an$SES_PERP_SEX_FRIEND)

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

table(full.numeric$RACE) # compare numbers

table(full.numeric$asian)
table(is.na(full.numeric$asian))

table(full.numeric$black.nh)
table(is.na(full.numeric$black.nh))

table(full.numeric$hispanic)
table(is.na(full.numeric$hispanic))


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


#### 3) First Logistic Regressions (NO Predictions yet) ####

# NB: AGE_CAT, EDUCATION, SES_INCOMEMTH still Factor Variables, -> R will find effect for each level

theoretical.regr <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                          SES_PARENTS_INCARCERATED + SES_HASCHILDREN + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                          black.nh + hispanic + asian + DRUG_COCRKREG + SES_SEXABUSED_EVER, 
                        data = full.numeric,
                        family = binomial(),
                        na.action = na.exclude)

summary(theoretical.regr)

theoretical.regr.norace <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                          SES_PARENTS_INCARCERATED + SES_HASCHILDREN + DRUG_COCRKREG + EDUCATION + AGE_CAT + SES_INCOMEMTH + SES_SEXABUSED_EVER + state, 
                        data = full.numeric,
                        family = binomial(),
                        na.action = na.exclude)

summary(theoretical.regr.norace)


# suprises: 1) negative significant effect of length of sentence
#           2) large significant negative effect of hispanic
#           3) having children = higher risk of recidivism
#           4) large negative effect of history of sexual abuse
#           5) Drug offenders less likely (significant negative effect)
#           6) Being in a state prison means you're much more likely to be a recidvist

# NO surprise: 1) positive significant effect of black.nh
#             2) lower inc = positive effect, highest inc = negative effect
#             3) equivalent for education
#             4) family incarcerated = positive effect
#             5) the older the more risk of recidivism
#             6) violent offenders higher risk
#             7) regular use of Cocaine/Crack increases risk of recidivism

#  Other observations:
#             1) Regular crack use variable has significant missingness (>50%) so that likely throws off the model
#             2) Removing race doesn't affect other point estimates very significantly. 

### Further theoretical models ###

theoretical.regr2 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                          + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_COCRK + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                          black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                        data = full.numeric,
                        na.action = na.exclude)

summary(theoretical.regr2)

#Family incarceration highly significant along with parent incarceration. 

## Examining Drug Use ##

#DRUG_ANY_ALCDRUGTME - ANY ALCOHOL OR ILLEGAL DRUG USE AT TIME OF CURRENT OFFENSE

theoretical.regr3 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
+ SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_ANY_ALCDRUGTME + EDUCATION + AGE_CAT + SES_INCOMEMTH +
black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
data = full.numeric,
na.action = na.exclude)

summary(theoretical.regr3)

#Strong significance for any type of drug use at the time of current offense. 

#Regular drug use#

#DRUG_ANYREG EVER USED ANY ILLEGAL DRUG REGULARLY
theoretical.regr4 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                           + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_ANYREG + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                           black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                         data = full.numeric,
                         na.action = na.exclude)

summary(theoretical.regr4)

#Regular use of drugs is also significant. But should be tested with different controls. 
