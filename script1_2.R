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

electionresults_pre$raceYear <- as.numeric(electionresults_pre$raceYear)

### Turning vote counts into numbers from character strings

electionresults_pre$DemVotes <- as.numeric(gsub(",", "", electionresults_pre$DemVotes))
electionresults_pre$DemVotes[is.na(electionresults_pre$DemVotes)] = 0

electionresults_pre$RepVotes <- as.numeric(gsub(",", "", electionresults_pre$RepVotes))
electionresults_pre$RepVotes[is.na(electionresults_pre$RepVotes)] = 0

electionresults_pre$ThirdVotes <- as.numeric(gsub(",", "", electionresults_pre$ThirdVotes))
electionresults_pre$ThirdVotes[is.na(electionresults_pre$ThirdVotes)] = 0

electionresults_pre$OtherVotes <- as.numeric(gsub(",", "", electionresults_pre$OtherVotes))
electionresults_pre$OtherVotes[is.na(electionresults_pre$OtherVotes)] = 0

total_votes <- electionresults_pre$RepVotes + electionresults_pre$DemVotes + electionresults_pre$ThirdVotes +
  electionresults_pre$OtherVotes

### Extracting the number from the Area column to make the function more user friendly

districtnumber <- mgsub(electionresults_pre$Area, c("District 1", "District 2", "District 3", "District 4", "District 5",
                                                "District 6", "District 7", "District 8", "District 9", "At Large"), 
                        c("1", "2", "3", "4", "5", "6", "7", "8", "9", "1"))

demvotepercent <- electionresults_pre$DemVotes / total_votes * 100
repvotepercent <- electionresults_pre$RepVotes / total_votes * 100
othervotepercent <- (electionresults_pre$ThirdVotes + electionresults_pre$OtherVotes) / total_votes * 100

electionresults <- cbind(districtnumber, electionresults_pre, demvotepercent, repvotepercent, othervotepercent)

### Function time 😎

### Actual function

districthistory <- function(RepState, RepDistrictNumber, RepParty) {
  if(RepParty == "Democrat") {
    return(district <- filter(electionresults, State == RepState &
                                districtnumber == RepDistrictNumber) %>%
             ggplot(aes(x = raceYear, y = demvotepercent)) +
             geom_line(size = 2, aes(color = "Democrats"))+
             labs() +
             ggtitle(paste("Democrats' Voteshare in", RepState, "District", RepDistrictNumber)) +
             theme_fivethirtyeight() +
             labs(subtitle = "Election data from 1940-2020") +
             scale_y_continuous(limits = c(0,100)) +
             scale_color_manual(name = "Political Parties",
                                breaks = c("Democrats", "Republicans", "Other Parties"), 
                                values = c("Democrats" = "#00AEF3", "Republicans" = "#E81B23", "Other Parties" = "#508C1B"))
           )
    district
  } else if(RepParty == "Republican") {
    return(district <- filter(electionresults, State == RepState & 
                                districtnumber == RepDistrictNumber) %>%
             ggplot(aes(x = raceYear, y = repvotepercent)) +
             geom_line(size = 2, aes(color = "Republicans")) +
             ggtitle(paste("Republicans' Voteshare in", RepState, "District", RepDistrictNumber)) +
             theme_fivethirtyeight() +
             labs(subtitle = "Election data from 1940-2020") +
             scale_y_continuous(limits = c(0,100)) +
             scale_color_manual(name = "Political Parties",
                                breaks = c("Democrats", "Republicans", "Other Parties"), 
                                values = c("Democrats" = "#00AEF3", "Republicans" = "#E81B23", "Other Parties" = "#508C1B"))
           )
    district
  } else if(RepParty == "Both") {
    return(district <- filter(electionresults, State == RepState &
                                 districtnumber == RepDistrictNumber) %>%
             ggplot(aes(raceYear)) +
             geom_line(aes(y = demvotepercent, color = "Democrat"), size = 2) +
             geom_line(aes(y = repvotepercent, color = "Republican"), size = 2) +
             ggtitle(paste("Both Parties' Voteshare in", RepState, "District", RepDistrictNumber)) +
             theme_fivethirtyeight() +
             labs(subtitle = "Election data from 1940-2020") +
             scale_y_continuous(limits = c(0,100)) +
             scale_color_manual(name = "Political Parties",
                                breaks = c("Democrat", "Republican", "Other Parties"), 
                                values = c("Democrat" = "#00AEF3", "Republican" = "#E81B23", "Other Parties" = "#508C1B"))
           )
    district
  } else if(RepParty == "All") {
    return(district <- filter(electionresults, State == RepState &
                                 districtnumber == RepDistrictNumber) %>%
             ggplot(aes(raceYear)) +
             geom_line(aes(y = demvotepercent, color = "Democrat"), size = 1.5) +
             geom_line(aes(y = repvotepercent, color = "Republican"), size = 1.5) +
             geom_line(aes(y = othervotepercent, color = "Other Parties"), size = 1.5) +
             ggtitle(paste("All Parties' Voteshare in", RepState, "District", RepDistrictNumber)) +
             theme(legend.position = "bottom") +
             theme_fivethirtyeight() +
             labs(subtitle = "Election data from 1940-2020") +
             scale_y_continuous(limits = c(0,100)) +
             labs(color = "Political Parties") +
             scale_color_manual(name = "Political Parties",
                                breaks = c("Democrat", "Republican", "Other Parties"), 
                                values = c("Democrat" = "#00AEF3", "Republican" = "#E81B23", "Other Parties" = "#508C1B"))
          )
    district
  } else {
    return(district <- filter(electionresults, electionresults$State == RepState &
                                electionresults$District == RepDistrictNumber) %>%
             ggplot(aes(x = electionresults$raceYear, y = othervotepercent)) +
             geom_line(size = 2, color = "Other Parties") +
             ggtitle(paste("Additional Parties' Voteshare in", RepState, "District", RepDistrictNumber)) +
             theme_fivethirtyeight() +
             labs(subtitle = "Election data from 1940-2020") +
             scale_y_continuous(limits = c(0,100)) +
             scale_color_manual(name = "Political Parties",
                                breaks = c("Democrat", "Republican", "Other Parties"), 
                                values = c("Democrat" = "#00AEF3", "Republican" = "#E81B23", "Other Parties" = "#508C1B"))
           )
    district
  }
}


