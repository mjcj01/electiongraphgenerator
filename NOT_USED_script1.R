library(dplyr)
library(ggplot2)
library(ggthemes)

### Loading original spreadsheet
### Data sourced from https://github.com/PrincetonUniversity/gerrymandertests/tree/master/election_data
### and https://www.cookpolitical.com/2020-house-vote-tracker 

electionresults_original <- rbind(results_2020, congressional_election_results_post1948)

### Changing variables to factors

electionresults_original$District <- as.factor(electionresults_original$District)
electionresults_original$Party <- as.factor(electionresults_original$Party)

### Changing variables to integers

electionresults_original$`Dem Votes` <- as.integer(electionresults_original$`Dem Votes`)
electionresults_original$`GOP Votes` <- as.integer(electionresults_original$`GOP Votes`)

### Manipulating data

total_votes <- electionresults_original$`Dem Votes` / electionresults_original$`D Voteshare`
rep_voteshare <- electionresults_original$`GOP Votes` / total_votes
dem_voteshare <- electionresults_original$`Dem Votes` / total_votes
other_voteshare <- (total_votes - (electionresults_original$`Dem Votes` + electionresults_original$`GOP Votes`)) / total_votes

### Combining data

electionresults <- cbind(total_votes, electionresults_original, rep_voteshare, dem_voteshare, other_voteshare)
electionresults$dem_voteshare[is.nan(electionresults$dem_voteshare)] = 0
electionresults$rep_voteshare[is.nan(electionresults$rep_voteshare)] = 0
electionresults$other_voteshare[is.nan(electionresults$other_voteshare)] = 0

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
