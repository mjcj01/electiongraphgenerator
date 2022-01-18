library(dplyr)
library(ggplot2)
library(ggthemes)

### Loading original spreadsheets
### Data Sourced from CQPress through Pennsylvania State University

"2002_2020_data" <- read.csv("C:\\Users\\micha\\Desktop\\1162022\\2002_2020_data.csv")
"1982_2000_data" <- read.csv("C:\\Users\\micha\\Desktop\\1162022\\1982_2000_data.csv")
"1962_1980_data" <- read.csv("C:\\Users\\micha\\Desktop\\1162022\\1962_1980_data.csv")
"1942_1960_data" <- read.csv("C:\\Users\\micha\\Desktop\\1162022\\1942_1960_data.csv")
"1940_data" <- read.csv("C:\\Users\\micha\\Desktop\\1162022\\1940_data.csv")

### Combining data

electionresults <- rbind(`2002_2020_data`, `1982_2000_data`, `1962_1980_data`, `1942_1960_data`, `1940_data`)



### Function time 😎

### Actual function

districthistory <- function(RepState, RepDistrictNumber, RepParty) {
  if(RepParty == "Democrat") {
    return(district <- filter(electionresults, electionresults$State == RepState &
                                electionresults$District == RepDistrictNumber) %>%
             ggplot(aes(x = Year, y = dem_voteshare)) +
             geom_line(size = 2, color = "#00AEF3")+
             labs() +
             ggtitle(paste("Democrat's Voteshare in", RepState, "District", RepDistrictNumber)) +
             theme_fivethirtyeight() +
             scale_y_continuous(limits = c(0,1))
           )
    district
  } else if(RepParty == "Republican") {
    return(district1 <- filter(electionresults, electionresults$State == RepState & 
                                electionresults$District == RepDistrictNumber) %>%
             ggplot(aes(x = Year, y = rep_voteshare)) +
             geom_line(size = 2, color = "#E81B23") +
             ggtitle(paste("Republican's Voteshare in", RepState, "District", RepDistrictNumber)) +
             theme_fivethirtyeight() +
             scale_y_continuous(limits = c(0,1))
           )
    district
  } else {
    return(district1 <- filter(electionresults, electionresults$State == RepState &
                                electionresults$District == RepDistrictNumber) %>%
             ggplot(aes(x = Year, y = other_voteshare)) +
             geom_line(size = 2, color = "#508C1B") +
             ggtitle(paste("Additional Parties' Voteshare in", RepState, "District", RepDistrictNumber)) +
             theme_fivethirtyeight() +
             scale_y_continuous(limits = c(0,1))
           )
    district
  }
}