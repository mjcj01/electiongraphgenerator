library(dplyr)
library(ggplot2)
library(ggthemes)
library(mgsub)

### Loading original spreadsheets

"2002_2020_data" <- read.csv("C:\\Users\\micha\\Desktop\\1162022\\2002_2020_data.csv")
"1982_2000_data" <- read.csv("C:\\Users\\micha\\Desktop\\1162022\\1982_2000_data.csv")
"1962_1980_data" <- read.csv("C:\\Users\\micha\\Desktop\\1162022\\1962_1980_data.csv")
"1942_1960_data" <- read.csv("C:\\Users\\micha\\Desktop\\1162022\\1942_1960_data.csv")
"1940_data" <- read.csv("C:\\Users\\micha\\Desktop\\1162022\\1940_data.csv")

### Combining data

electionresults_pre <- rbind(`2002_2020_data`, `1982_2000_data`, `1962_1980_data`, `1942_1960_data`, `1940_data`)

### Extracting the number from the Area column to make the function more user friendly

districtnumber <- mgsub(electionresults$Area, c("District 1", "District 2", "District 3", "District 4", "District 5",
                                                "District 6", "District 7", "District 8", "District 9", "At Large"), 
                        c("1", "2", "3", "4", "5", "6", "7", "8", "9", "1"))

### Combining data again

electionresults <- cbind(districtnumber, electionresults_pre)

electionresults$RepVotesMajorPercent <- as.numeric(electionresults$RepVotesMajorPercent)
electionresults$DemVotesMajorPercent <- as.numeric(electionresults$DemVotesMajorPercent)
electionresults$raceYear <- as.numeric(electionresults$raceYear)

### Function time
### This function will create a graph depending on which state, district, and party you put in. 
### You can "Both" in and get the voteshare of both Democrats and Republicans in that district.

districthistory <- function(RepState, RepDistrictNumber, RepParty) {
  if(RepParty == "Democrat") {
    return(district <- filter(electionresults, State == RepState &
                                districtnumber == RepDistrictNumber) %>%
             ggplot(aes(x = raceYear, y = DemVotesMajorPercent)) +
             geom_line(size = 2, color = "#00AEF3")+
             labs() +
             ggtitle(paste("Democrat's Voteshare in", RepState, "District", RepDistrictNumber)) +
             theme_fivethirtyeight() +
             scale_y_continuous(limits = c(0,100))
           )
    district
  } else if(RepParty == "Republican") {
    return(district1 <- filter(electionresults, State == RepState & 
                                districtnumber == RepDistrictNumber) %>%
             ggplot(aes(x = raceYear, y = RepVotesMajorPercent)) +
             geom_line(size = 2, color = "#E81B23") +
             ggtitle(paste("Republican's Voteshare in", RepState, "District", RepDistrictNumber)) +
             theme_fivethirtyeight() +
             scale_y_continuous(limits = c(0,100))
           )
    district
  } else {
    return(district1 <- filter(electionresults, electionresults$State == RepState &
                                electionresults$District == RepDistrictNumber) %>%
             ggplot(aes(x = electionresults$raceYear, y = other_voteshare)) +
             geom_line(size = 2, color = "#508C1B") +
             ggtitle(paste("Additional Parties' Voteshare in", RepState, "District", RepDistrictNumber)) +
             theme_fivethirtyeight() +
             scale_y_continuous(limits = c(0,100))
           )
    district
  }
}
