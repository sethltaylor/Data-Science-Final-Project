
setwd("C:/Users/viola/Documents/Studium/MPP GU/Spring 2017/Data Science/Data-Science-Final-Project") # adjust individually
setwd("/Users/shashankrai/GitHub/Data-Science-Final-Project")
      
      

library(memisc)

#### 1) DATA EXTRACTION ####

## extract .rda files for Federal and State Analysis data from Github #

# function that assigns loaded data frame the desired name directly
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#Download option 1
###
library(RCurl)
temp <- tempfile()
download.file("https://github.com/GeorgetownMcCourt/Predicting-Recidivism/raw/master/Data/FederalAnalysisR.rda", temp, method = "auto", mode="wb")
fed.an <- load(temp)
fed.an <- da04572.0003

temp <- tempfile()
download.file("https://github.com/GeorgetownMcCourt/Predicting-Recidivism/raw/master/Data/StateAnalysisR.rda", temp, method = "auto", mode="wb")
state.an <- load(temp)
state.an <- da04572.0004
###

#Download option 2
###
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
###

# dummy for State T/F before combining fed.an and state.an
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

# NB: if you want to continue using the function below, it would have to look something like that
# the old function only changed factor levels, not the actual values
# this function works if you just convert factors to character vectors
# But I suggest approaching variables differently, because you'd still need to convert every variable even w/ this function

# clean1 <- function(variable){
#   variable[variable == "(1) Yes"] <- 1
#   variable[variable == "(2) No"] <- 0
#   variable[variable == "(0000001) Has children"] <- 1
#   variable[variable == "(0000002) Does not have children"] <- 0
#   variable[variable == "(0000000) No"] <- 0
#   variable[variable == "(0000002) No"] <- 0
#   variable[variable == "(0000001) Yes"] <- 1
#   variable[variable == "(9999998) DK/Refused/Missing"] <- NA
#   variable[variable == "(9999997) Don't know"] <- NA
#   variable[variable == "(9999998) Refused"] <- NA
#   variable[variable == "(9999999) Missing"] <- NA
#   variable[variable == "(7) Unknown"] <- NA
#   return(variable)
# }


### Generate recidivism variable


table(full.numeric$CH_CRIMHIST_COLLAPSED)
table(full.an$CH_CRIMHIST_COLLAPSED)
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


# Recode CS_SENTENCEMTH (Length of Sentence in Month)
str(full.numeric$CS_SENTENCEMTH)

full.numeric$CS_SENTENCEMTH[full.numeric$CS_SENTENCEMTH > 10000] <- NA ## convert all Missings

full.numeric$CS_SENTENCEMTH <- as.numeric(full.numeric$CS_SENTENCEMTH) ## NB: Length of 10,000 == Life or Death Sentence

# Recode SES_PARENTS_INCARCERATED, SES_HASCHILDREN

vars <- c("SES_PARENTS_INCARCERATED", "SES_HASCHILDREN")

for (i in vars) {
    # remove punctuation, spaces, and alphabetic expressions
    levels(full.numeric[,i]) <- gsub("[[:punct:]]", "", levels(full.numeric[,i]))
    levels(full.numeric[,i]) <- gsub("[[:space:]]", "", levels(full.numeric[,i]))
    levels(full.numeric[,i]) <- gsub("[[:alpha:]]", "", levels(full.numeric[,i]))
    # now the factor label should be numeric only
}

# convert into character vector and recode
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
                        SES_PARENTS_INCARCERATED + SES_HASCHILDREN + DRUG_COCRK + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                        black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                        data = full.numeric,
                        na.action = na.exclude)

summary(theoretical.regr)

# suprises: 1) negative significant effect of length of sentence #Shashank - Chances are that prisoners with longer sentences will be released less often in their lifetime and hence less recidivism
#           2) large significant negative effect of hispanic #Shashank - That changes with other specifications. Wouldn't worry about it. 
#           3) having children = higher risk of recidivism #Shashank - See if unemployment's interaction with children is significant. 
                                                          #Hypothesis being that those that are unemployed and with children to feed may use crime as a tool to get by, 
                                                          #but mainly small crimes. After a certain number of offences, they may get a small term. But unable to do any better,
                                                          # once outside the prison, they may continue to engage in illegal activities. 
                                                          #This combined with justice system's tendency to award stricter punishment with increasing number of offences
                                                          #might be one of the causes behind recidivism
#           4) large negative effect of history of sexual abuse 
#           5) Drug offenders less likely (significant negative effect)
#Shashank - Wondering if sexual abuse and drug offences might be positively correlated. Would then be worth figuring out what's going on there. 

#6) Shashank - Also there is small significance for different grades in school, which is not surprising, but large significance for "Graduate school two or more years"

# NO surprise: 1) positive significant effect of black.nh
#             2) lower inc = positive effect, highest inc = negative effect
#             3) equivalent for education
#             4) family incarcerated = positive effect
#             5) the older the more risk of recidivism #Shashank - Not really. It seems significant for all age groups. Significance reduces at 55-64. 
                                                      # And then is insignificant for older ages, which makes sense - average age might be lower. 
                                                      # Also ability to commit crime might go down with age. 
#             6) violent offenders higher risk
#             7) regular use of Cocaine/Crack increases risk of recidivism #Shashank - But drug offences don't? Worth exploring this. 

#############################

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

#looking at family incarceration

full.numeric$SES_FAMILY_INCARCERATED <- clean1(full.numeric$SES_FAMILY_INCARCERATED)
str(full.numeric$SES_FAMILY_INCARCERATED)

full.numeric$SES_FAMILY_INCARCERATED[full.numeric$SES_FAMILY_INCARCERATED == 2] <- 0

summary(full.numeric$SES_FAMILY_INCARCERATED)

#Without Parents' Incarceration

theoretical.regr1 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                         SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_COCRK + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                          black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                        data = full.numeric,
                        na.action = na.exclude)

summary(theoretical.regr1)

#Family Incarceration insignifcant

#With Parents' Incarcerated

theoretical.regr2 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                          + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_COCRK + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                          black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                        data = full.numeric,
                        na.action = na.exclude)

summary(theoretical.regr2)

#Family incarceration highly significant along with parent incarceration. 
#Interesting. 

#Checking for Drugs

#DRUG_ANY_ALCDRUGTME - ANY ALCOHOL OR ILLEGAL DRUG USE AT TIME OF CURRENT OFFENSE

theoretical.regr3 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                           + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_ANY_ALCDRUGTME + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                           black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                         data = full.numeric,
                         na.action = na.exclude)

summary(theoretical.regr3)

#Strong significance for any type of drug use at the time of current offense. 

#Exploring drugs further

############
##Regular drug use

#DRUG_ANYREG EVER USED ANY ILLEGAL DRUG REGULARLY
theoretical.regr4 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                           + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_ANYREG + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                           black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                         data = full.numeric,
                         na.action = na.exclude)

summary(theoretical.regr4)

#However regular usage of drugs is not significant. Important result for policy recommendations. But should be tested with different controls. 

#Breaking up regular drug use 

theoretical.regr5 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                           + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_COCRKREG + DRUG_HROPREG +
                         DRUG_DEPREG + DRUG_STIMREG + DRUG_HALUREG + DRUG_MARIJREG + DRUG_INHALREG + 
                         DRUG_METHAMREG + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                           black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                         data = full.numeric,
                         na.action = na.exclude)

summary(theoretical.regr5)

#No significance for any of the drugs

#Just for crack cocaine
theoretical.regr6 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                           + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_COCRKREG +  EDUCATION + AGE_CAT + SES_INCOMEMTH +
                           black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                         data = full.numeric,
                         na.action = na.exclude)

summary(theoretical.regr6)

#No significance for regular use of crack cocaine either.

############

############
## Exploring use of different drugs at the time of crime

theoretical.regr7 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                           + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_COCRKTME + DRUG_HROPTME
                         + DRUG_DEPTME +  DRUG_STIMTME + DRUG_HALUTME +  DRUG_MARIJTME + DRUG_METHATME
                          +  EDUCATION + AGE_CAT + SES_INCOMEMTH +
                           black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                         data = full.numeric,
                         na.action = na.exclude)

summary(theoretical.regr7)

# COCAINE OR CRACK at the time is highly significant (1 per cent)
# HEROIN OR OTHER OPIATES at the time is highly significant (1 per cent)
# STIMULANTSWITHOUTPRESCRIPTION at the time is significant at 10 per cent

############

# In a separate regression, the coefficient on use of alcohol or any other drug at the time of crime is significant (DRUG_ANY_ALCDRUGTME) - 5 per cent
# In case alcohol abuse has to be explored. 

##############
#Adding other variables
# DRUG_DISPUTES_ALCDRUG - HAD DOMESTIC DISPUTES WHILE DRUNK OR USING ILLEGAL DRUGS IN YEAR BEFORE CURRENT OFFENSE

theoretical.regr8 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                           + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_COCRKTME + DRUG_HROPTME
                         + DRUG_ANY_ALCDRUGTME
                         +  EDUCATION + AGE_CAT + SES_INCOMEMTH +
                           black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                         data = full.numeric,
                         na.action = na.exclude)

summary(theoretical.regr8)

#Not significant

##DRUG_FIRED_ALCDRUG - BEEN FIRED FROM JOB BECAUSE OF ALCOHOL OR DRUG USE IN YEAR BEFORE CURRENT OFFENSE

theoretical.regr9 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                           + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_COCRKTME + DRUG_HROPTME
                         + DRUG_FIRED_ALCDRUG
                         +  EDUCATION + AGE_CAT + SES_INCOMEMTH +
                           black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                         data = full.numeric,
                         na.action = na.exclude)

summary(theoretical.regr9)

#Highly significant

#Another model without the nature of offense to test the mitigating circumstances.

theoretical.regr10 <- glm(CH_CRIMHIST_COLLAPSED ~  SES_PHYSABUSED_EVER + CS_SENTENCEMTH + DRUG_COCRKTME + DRUG_HROPTME
                          + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_ANYREG + EDUCATION + AGE_CAT + SES_INCOMEMTH +
                            black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                          data = full.numeric,
                          na.action = na.exclude)

summary(theoretical.regr10)

#Drug use continues to be highly significant. In fact all major variables, except "CS_SENTENCEMTH" are significant


##DRUG_WORKTROUBLE_ALCDRUG - HAD WORK OR SCHOOL TROUBLE BECAUSE OF ALCOHJOL OR DRUG USE IN YEAR BEFORE CURRENT OFFENSE
theoretical.regr11 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                           + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_COCRKTME + DRUG_HROPTME
                         + DRUG_FIRED_ALCDRUG + DRUG_WORKTROUBLE_ALCDRUG
                         +  EDUCATION + AGE_CAT + SES_INCOMEMTH + 
                           black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                         data = full.numeric,
                         na.action = na.exclude)

summary(theoretical.regr11)

#Not significant

#Treatment - Could this have a negative effect - DRUG_ANYTRT - ANY TREATMENT EVER FOR ALCOHOL OR DRUG USE

theoretical.regr11 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                            + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_COCRKTME + DRUG_HROPTME
                          + DRUG_FIRED_ALCDRUG + DRUG_ANYTRT
                          +  EDUCATION + AGE_CAT + SES_INCOMEMTH + 
                            black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                          data = full.numeric,
                          na.action = na.exclude)

summary(theoretical.regr11)

#Highly significant but a bunch of other variables lose significance. Also coefficient is positive!!! => Drug treatment not helping???

#DRUG_TRTINC - ANY TREATMENT EVER FOR ALCOHOL OR DRUG USE WHILE INCARCERATED

theoretical.regr12 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                            + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_COCRKTME + DRUG_HROPTME
                          + DRUG_FIRED_ALCDRUG + DRUG_TRTINC
                          +  EDUCATION + AGE_CAT + SES_INCOMEMTH + 
                            black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                          data = full.numeric,
                          na.action = na.exclude)

summary(theoretical.regr12)

#Not significant. 

# DRUG_TRTPRB - ANY TREATMENT EVER FOR ALCOHOL OR DRUG USE WHILE ON PROBATION OR PAROLE

theoretical.regr13 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                            + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_COCRKTME + DRUG_HROPTME
                          + DRUG_FIRED_ALCDRUG + DRUG_TRTPRB
                          +  EDUCATION + AGE_CAT + SES_INCOMEMTH + 
                            black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                          data = full.numeric,
                          na.action = na.exclude)

summary(theoretical.regr13)

#Highly significant but still positive. 

#So you abuse drugs, get caught for a criminal offense, serve time, clean up yourself to get parole, 
#make an effort to get back on track by going in for treatment, treatment does not work (for many reasons), 
#And you're back! 

####

#DRUG_ANYPGM - EVER PARTICIPATED IN ANY ALCOHOL OR DRUG PROGRAM

theoretical.regr14 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                            + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_COCRKTME + DRUG_HROPTME
                          + DRUG_FIRED_ALCDRUG + DRUG_TRTPRB + DRUG_ANYPGM
                          +  EDUCATION + AGE_CAT + SES_INCOMEMTH + 
                            black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                          data = full.numeric,
                          na.action = na.exclude)

summary(theoretical.regr14)

#Not signifincant  with or without treatment. 

# DRUG_ANYINF_CB - UNDER INFLUENCE OF ANY DRUGS AT TIME OF CURRENT OFFENSE

full.numeric$DRUG_ANYINF_CB <- clean1(full.numeric$DRUG_ANYINF_CB)
full.numeric$DRUG_ANYINF_CB[full.numeric$DRUG_ANYINF_CB==2] <- 0

theoretical.regr15 <- glm(CH_CRIMHIST_COLLAPSED ~ OFFENSE_VIOLENT + OFFENSE_DRUG + SES_PHYSABUSED_EVER + CS_SENTENCEMTH + 
                            + SES_PARENTS_INCARCERATED + SES_FAMILY_INCARCERATED + SES_HASCHILDREN + DRUG_ANYINF_CB
                          + DRUG_FIRED_ALCDRUG + DRUG_TRTPRB
                          +  EDUCATION + AGE_CAT + SES_INCOMEMTH + 
                            black.nh + hispanic + asian + SES_SEXABUSED_EVER, 
                          data = full.numeric,
                          na.action = na.exclude)

summary(theoretical.regr15)

#Not significant


