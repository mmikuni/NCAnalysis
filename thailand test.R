library(xts)

#reads the Thailand incidents and assigns it to "tlcsv"
tlcsv <- read.csv("C:/Users/Matthew.Mikuni/Downloads/thailandincidents.csv", header = TRUE)

#remove unnecessary columns
tlcsv$incidentid <- NULL
tlcsv$city <- NULL
tlcsv$stateprovince <- NULL
tlcsv$country <- NULL
tlcsv$gist <- NULL
tlcsv$description <- NULL
tlcsv$infosource <- NULL
tlcsv$infoquality <- NULL
tlcsv$latitude <- NULL
tlcsv$longitude <- NULL
tlcsv$updatedate <- NULL

#filters all severe incidents
testSevere <- subset(tlcsv, severity == "Severe")

#formats the dates in tlcsv$date occurred to get it to be recognized
tlcsv$dateoccurred <- as.Date(tlcsv$dateoccurred, format = '%m/%d/%Y')

#creates a subset from tlcsv for all incidents that occur after 1/1/2013
subset(tlcsv, tlcsv$dateoccurred >"2013-01-01")

#gets the number of rows for incidents in JUST 2013
getrows <- subset(tlcsv, tlcsv$dateoccurred >= "2013-01-01" & tlcsv$dateoccurred <= "2013-12-31")
nrow(getrows)

#creates a subset and gets the rows from incidents that took place after 2013 and are Security incidents
subset(tlcsv, tlcsv$dateoccurred >"2013-01-01" & tlcsv$incidentcategory == "Security")
getrows2 <- subset(tlcsv, tlcsv$dateoccurred >"2013-01-01" & tlcsv$incidentcategory == "Security")
nrow(getrows2)