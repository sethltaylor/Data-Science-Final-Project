library(foreign)

setwd("/Users/shashankrai")

load("~/GitHub/Data-Science-Final-Project/04572-0004-Data.rda")
load("~/GitHub/Data-Science-Final-Project/04572-0003-Data.rda")

federal <- da04572.0003
states <- da04572.0004

total <- rbind(federal,states)

unique(total$CH_CRIMHIST)
unique(total$CH_CRIMHIST_COLLAPSED)
unique(total$CH_CRIM_HISTORY)

summary(total$CH_CRIMHIST_COLLAPSED)

#New Criminal History Variable with recidivist = 1, and first timers = 0, Factor variable
#drop observations for missing values in criminal history

mydata <- subset.data.frame(x = total, subset = CH_CRIMHIST_COLLAPSED != "(9999999) Missing")
summary(mydata$CH_CRIMHIST_COLLAPSED)
droplevels((mydata)$CH_CRIMHIST_COLLAPSED)

levels(mydata$CH_CRIMHIST_COLLAPSED)[levels(mydata$CH_CRIMHIST_COLLAPSED) == "(0000001) First timers"] <- 0
levels(mydata$CH_CRIMHIST_COLLAPSED)[levels(mydata$CH_CRIMHIST_COLLAPSED) == "(0000002) Recidivist, current or past violent offense"] <- 1
levels(mydata$CH_CRIMHIST_COLLAPSED)[levels(mydata$CH_CRIMHIST_COLLAPSED) == "(0000003) Recidivist, no current or prior violent offense"] <- 1

mydata$CH_CRIMHIST_COLLAPSED <- as.factor(mydata$CH_CRIMHIST_COLLAPSED)
levels(mydata$CH_CRIMHIST_COLLAPSED)

#Veteran: 1 if yes, 0 if no, factor variable
levels(mydata$VETERAN)[levels(mydata$VETERAN) == "(1) Yes"] <- 1
levels(mydata$VETERAN)[levels(mydata$VETERAN) == "(2) No"] <- 0
mydata$VETERAN <- as.numeric(mydata$VETERAN)
mydata$VETERAN[mydata$VETERAN <- 2] <- 0

#Race: White = 1, Black = 2, Hispanic = 3, Others = 4 (Others include (American indian, alaska native non-hispanic)
# (Asian, pacific islander, native hawaiian non-hispanic), (Multiple races reported, non-hispanic), (Missing))


levels(mydata$RACE)[levels(mydata$RACE) == "(0000001) White non-hispanic"] <- 1
levels(mydata$RACE)[levels(mydata$RACE) == "(0000002) Black non-hispanic"] <- 2
levels(mydata$RACE)[levels(mydata$RACE) == "(0000003) Hispanic"] <- 3
levels(mydata$RACE)[levels(mydata$RACE) == "(0000004) American indian, alaska native non-hispanic"] <- 4
levels(mydata$RACE)[levels(mydata$RACE) == "(0000005) Asian, pacific islander, native hawaiian non-hispanic"] <- 4
levels(mydata$RACE)[levels(mydata$RACE) == "(0000006) Multiple races reported, non-hispanic"] <- 4
levels(mydata$RACE)[levels(mydata$RACE) == "(9999999) Missing"] <- 4


#Most variables are factor variables with well defined labels

#Non-Factor Variables 
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


#Converting Factor Variables to Numeric
mydata <- sapply(mydata, as.numeric)

##########################################

cortable <- as.matrix(cor(mydata, method = c("pearson", "kendall", "spearman")))

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