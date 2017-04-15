sum(mydata)

mydata <- as.numeric(mydata)

str(mydata, list.len=ncol(mydata))

variable.names(mydata)

######

#Clean Function for Yes/No/Missing
clean1 <- function(variable){
  variable[variable == "(1) Yes"] <- 1
  variable[variable == "(2) No"] <- 0
  levels(variable)[levels(variable) == "(0000000) No"] <- 0
  levels(variable)[levels(variable) == "(0000002) No"] <- 0
  levels(variable)[levels(variable) == "(0000001) Yes"] <- 1
  levels(variable)[levels(variable) == "(9999998) DK/Refused/Missing"] <- NA
  levels(variable)[levels(variable) == "(9999999) Missing"] <- NA
  as.numeric(variable)
  summary(variable)
}

clean1(mydata$VETERAN)


##########
clean1(mydata$OFFENSE_VIOLENT)
str(mydata$OFFENSE_VIOLENT)
clean1(mydata$OFFENSE_PROPERTY)          
clean1(mydata$OFFENSE_DRUG)                 
clean1(mydata$SES_PHYSABUSED_EVER)             
clean1(mydata$SES_SEXABUSED_EVER)

#SES_PHYSSEXABUSED_EVER
mydata$SES_PHYSSEXABUSED_EVER <- as.character(mydata$SES_PHYSSEXABUSED_EVER)
mydata$SES_PHYSSEXABUSED_EVER[mydata$SES_PHYSSEXABUSED_EVER == "(0000001) Physically only"] <- 1
mydata$SES_PHYSSEXABUSED_EVER[mydata$SES_PHYSSEXABUSED_EVER == "(0000002) Sexually only"] <- 2
mydata$SES_PHYSSEXABUSED_EVER[mydata$SES_PHYSSEXABUSED_EVER == "(0000003) Both physically and sexually abused"] <- 3
mydata$SES_PHYSSEXABUSED_EVER[mydata$SES_PHYSSEXABUSED_EVER == "(0000004) Not abused"] <- 4
mydata$SES_PHYSSEXABUSED_EVER[mydata$SES_PHYSSEXABUSED_EVER == "(9999999) Missing"] <- NA
mydata$SES_PHYSSEXABUSED_EVER <- as.factor(mydata$SES_PHYSSEXABUSED_EVER)
str(mydata$SES_PHYSSEXABUSED_EVER)

#SES_AGEOFF_PHYSSEXABUSED
summary(mydata$SES_AGEOFF_PHYSSEXABUSED)
mydata$SES_AGEOFF_PHYSSEXABUSED <- as.character(mydata$SES_AGEOFF_PHYSSEXABUSED)
mydata$SES_AGEOFF_PHYSSEXABUSED[mydata$SES_AGEOFF_PHYSSEXABUSED == xx] <- 1

############################

tota
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

#############################

#Veteran: 1 if yes, 0 if no, factor variable
levels(mydata$VETERAN)[levels(mydata$VETERAN) == "(1) Yes"] <- 1
levels(mydata$VETERAN)[levels(mydata$VETERAN) == "(2) No"] <- 0
mydata$VETERAN <- as.numeric(mydata$VETERAN)
mydata$VETERAN[mydata$VETERAN <- 2] <- 0

#############################
