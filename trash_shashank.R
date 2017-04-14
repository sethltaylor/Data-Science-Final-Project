sum(mydata)

mydata <- as.numeric(mydata)

str(mydata, list.len=ncol(mydata))

variable.names(mydata)

######

#Clean Function for Yes/No/Missing
clean1 <- function(variable){
  levels(variable)[levels(variable) == "(1) Yes"] <- 1
  levels(variable)[levels(variable) == "(2) No"] <- 0
  levels(variable)[levels(variable) == "(0000000) No"] <- 0
  levels(variable)[levels(variable) == "(0000002) No"] <- 0
  levels(variable)[levels(variable) == "(0000001) Yes"] <- 1
  levels(variable)[levels(variable) == "(9999998) DK/Refused/Missing"] <- NA
  levels(variable)[levels(variable) == "(9999999) Missing"] <- NA
  droplevels(variable)
  as.numeric(variable)
  return(variable)
}

clean1(mydata$VETERAN)


##########
clean1(mydata$OFFENSE_VIOLENT)
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

#############
