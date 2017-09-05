require(tidyverse)
setwd("")

### Read in ###
### Read in ###
### Read in ###
outputRaw <- read.csv("surveyOut94.csv", stringsAsFactors = FALSE, strip.white=TRUE) %>% subset(Status =="Complete")

### Quality Check  - time to complete ###
### Quality Check  - time to complete ###
### Quality Check  - time to complete ###
time <- outputRaw[c(2:3, 114)]
colnames(time)[1:2] <- c("timeIn", "timeOut")

time <- time %>% mutate(timeIn = as.POSIXct(time$timeIn, format = "%b %d, %Y %T %p"),
                        timeOut = as.POSIXct(time$timeOut, format = "%b %d, %Y %T %p"),
                        timeDiff = timeOut - timeIn)

tooShort <- time  %>% subset(timeDiff < 240 & timeDiff > 0) %>% left_join(outputRaw, by="Email.") %>%
                      subset(Does.your.department.use.federal.data. == "Yes") %>%
                      arrange(timeDiff)
#no red flags here
rm(time, tooShort)
                  
### that one person who took the survey twice ###
### that one person who took the survey twice ###
### that one person who took the survey twice ###
dupEmails <- outputRaw %>% group_by(Email.) %>% summarise(n=n()) %>% 
                           subset(n > 1) %>% left_join(outputRaw, by="Email.")
#email this woman
rm(dupEmails)

### Better Var Names ###
### Better Var Names ###
### Better Var Names ###
output <- outputRaw[c(22:112)]

colnames(output) <- c("city", "deptWork", "deptSpeak", "fedUse", "noUseBenefit", "noUseAccess", "noUseCity", "noUsePurchase", "noUseOther", "noUseOtherText", "useDemo", "useEcon", "useEnv", #13
                      "useEdu", "useHealth", "useHouse", "useCrim", "useSci", "useReg", "useTranspo", "useLegal", "useCaFi", "useElec", "useLeg", "dataLabor", "dataDOJ", "dataDOT", #27
                      "dataCensus", "dataDOI", "dataHHS", "dataDOE", "dataEPA", "dataHUD", "dataUSDA", "dataFDIC", "dataFHFA", "dataIRS", "dataCFPB", "dataOther", "otherDataText", "howAccess", #41
                      "geoState", "geoCounty", "geoCountySub", "geoCBSA", "geoTract", "geoBlockGroup", "geoBlock", "geoAll", "geoNA", "doTrack", "doID", "doBudget", "doInform", "doPredict", #55
                      "doReg", "doOther", "otherDoText", "import", "startUse", "pastChg", "lessFreq", "lessQuality", "lessAccess", "lessUse", "lessOther", "lessOtherText", "lessReplace", #68
                      "futureChg", "fLessFreq", "flessQual", "fLessAccess", "fLessUse", "fLessOther", "fLessOtherText", "fLessReplace", "freq", "access", "easeUse", "accuracy", "futureFreq", #81
                      "futureAccess", "futureUse", "futureAccuracy", "mena", "hispanic", "lostAccess", "lostData", "lossImpact", "wishData", "wishListData")


### Clean variables ###
### Clean variables ###
### Clean variables ###
#refactor vars
output <- output %>% mutate(import = factor(import, levels = c("Not important", "Slightly important", "Fairly important",
                                                               "Important", "Very important")),
                            startUse = factor(startUse, levels= c("Less than a year ago", "1 to 3 years ago", "3 to 5 years ago", 
                                                                  "5 to 10 years ago", "More than a decade ago")),
                            pastChg = factor(pastChg, levels = c("A lot less", "Slightly less", "About the same", "Slightly more", "A lot more")),
                            futureChg = factor(futureChg, levels = c("A lot less", "Slightly less", "About the same", "Slightly more", "A lot more")),
                            freq = factor(freq, level = c("Too infrequent", "Just right", "Too frequent")),
                            access = factor(access, levels = c("Very difficult", "Difficult", "Neutral", "Easy", "Very easy")),
                            easeUse = factor(easeUse, levels = c("Very difficult", "Difficult", "Neutral", "Easy", "Very easy")),
                            accuracy = factor(accuracy, levels = c("Very Poor", "Poor", "Acceptable", "Good", "Very Good")),
                            mena = factor(mena, levels = c("Not at all valuable", "Slightly valuable", "Moderately valuable", "Very valuable", "Extremely valuably")),
                            hispanic = factor(hispanic, levels = c("Not at all valuable", "Slightly valuable", "Moderately valuable", "Very valuable", "Extremely valuably")))

refactor <- function(x) {
  x <- factor(x, levels = c("Much worse", "Somewhat worse", "About the same", "Somewhat better", "Much better"))
}
output$futureFreq <- refactor(output$futureFreq)
output$futureAccess <- refactor(output$futureAccess)
output$futureUse <- refactor(output$futureUse)
output$futureAccuracy <- refactor(output$futureAccuracy)

#remove explanatory agency text in response
agency <- output[c(grepl("data|howAccess", colnames(output)))]
output <- output[c(!grepl("data|howAccess", colnames(output)))]

agency <- agency %>% mutate_all(funs(gsub("<strong>|</strong>", "", .))) %>%
  mutate_all(funs(gsub("\\(.*", "", .)))
output <- cbind(output, agency)

rm(agency)

#clean open text fields
output <- output %>% mutate(cityClean = ifelse(grepl("MO", city), "Kansas City MO", city),
                                cityClean = ifelse(grepl(", KS|, Kansas", cityClean), "Kansas City KS", cityClean),
                                  cityClean = gsub("City and County of |County of |City of ", "", cityClean),
                                    cityClean = gsub(" Arizonia", "", cityClean), 
                                      cityClean = gsub("Washington, DC", "District of Columbia", cityClean),
                                        cityClean = tolower(gsub(",.*", "", cityClean))) %>%
                                          subset(cityClean != "n/a") 


output <- output %>% mutate(deptSpeak = tolower(deptSpeak),
                                deptSpeak = ifelse(grepl("all|city wide|countywide|msa wide", deptSpeak) | deptSpeak=="city", "citywide",
                                                   ifelse(grepl("some|several|few|most|//", deptSpeak), "multiple", deptSpeak)),
                                howAccess = gsub("\\(.*", "", howAccess)) %>%
                     subset(!(deptSpeak %in% c("don't know", "n/a", "none"))) %>%
                     mutate(deptSpeakGroup = ifelse(grepl("finance|budget|controller", deptSpeak), "finance", 
                                              ifelse(grepl("planning|development|department and division", deptSpeak), "planning/development", 
                                                ifelse(grepl("police|fire|hsema", deptSpeak), "emergency services", 
                                                  ifelse(grepl("public works|utilities|engineering", deptSpeak), "public works", 
                                                    ifelse(grepl("housing|departmental", deptSpeak), "housing", 
                                                      ifelse(grepl("innovation|datasf", deptSpeak), "innovation/analytics",
                                                        ifelse(grepl("admin|audit|clerk", deptSpeak), "administration",
                                                          ifelse(grepl("governance|performance|omb|project management|hreeo", deptSpeak), "governance/performance",
                                                           ifelse(grepl("children|dfcyr", deptSpeak), "family/children",
                                                                  deptSpeak)))))))))) %>%
                     subset(deptWork != "USDA Food and Nutrition Service") %>%
                     mutate(deptWork = tolower(deptWork),
                              deptWorkGroup = ifelse(grepl("admin|audit|clerk", deptWork), "administration",
                                                ifelse(grepl("budget|finance|controller", deptWork), "finance", 
                                                  ifelse(grepl("legislative|legislature", deptWork), "legislative",
                                                  ifelse(grepl("city manager", deptWork), "city manager office",
                                                    ifelse(grepl("mayor|executive", deptWork), "mayor's office",
                                                      ifelse(grepl("planning|development|devce", deptWork), "planning/development",
                                                        ifelse(grepl("police|fire|hsema", deptWork), "emergency services",
                                                          ifelse(grepl("innovation|stir|knowledge|analytics", deptWork), "innovation/analytics",
                                                              ifelse(grepl("real estate|housing", deptWork), "housing",
                                                                ifelse(grepl("health|safety", deptWork), "health",
                                                                  ifelse(grepl("governance|performance|omb|project management|hreeo", deptWork), "governance/performance",
                                                                    ifelse(grepl("works|engineering|recreation|maintenance|utilities", deptWork), "public works",
                                                                      ifelse(grepl("children", deptWork), "family/children",
                                                                        ifelse(grepl("communications", deptWork), "communications",
                                                                          ifelse(grepl("it|gis|deptWork|geospatial information systems|information services", deptWork), "it/gis",
                                                                        deptWork))))))))))))))))

#mask city field 
cities <- output %>% group_by(cityClean) %>% summarise(n=n())  %>%
          mutate(cityID = sample(100:1000, 62, replace=F)) %>% select(-n)

output <- output %>% inner_join(cities, by="cityClean") %>% select(-city, -cityClean)

### Quality check - neutral throughout ###
### Quality check - neutral throughout ###
### Quality check - neutral throughout ###
allMeh <- output %>% subset(futureFreq=="About the same" & futureUse == "About the same" & 
                              futureAccess == "About the same" & futureAccuracy == "About the same" &
                               access == "Neutral" & easeUse == "Netural" & accuracy == "Acceptable")
#no red flags
rm(allMeh)

### Analytical check - do same cities have same responses ###
### Analytical check - do same cities have same responses ###
### Analytical check - do same cities have same responses ###
city <- output %>% group_by(cityID) %>% summarise(count=n()) %>% subset(count > 3) %>% select(cityID)
manyCityReps <- city %>% left_join(output, by="cityID") %>% arrange(cityID)
eachCity <- split(manyCityReps, manyCityReps$cityID)

# where multiple employees in city answered, no consistent answering. does not support need to weight by city. 
write.csv(output, "fedUseSurveyResponses.csv")
