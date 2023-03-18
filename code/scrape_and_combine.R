#------------------------------------------------------------------------

# Project: Paywalls
# 
# Purpose: Scrape state legislation party majorities from Ballotpedia 
#          and combine into one dataset
# 

#------------------------------------------------------------------------

### Clean workspace
rm(list=ls())


### Set wd
if (!dir.exists("../data/")) dir.create("../data/")
setwd("../data/")


### Load libraries
if (!require('datasets')) install.packages('datasets');   library('datasets')   # list of US state names
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')  # data manipulation
if (!require('rvest')) install.packages('rvest');         library('rvest')      # web scraping
if (!require('polite')) install.packages('polite');       library('polite')     # polite web scraping
if (!require('xlsx')) install.packages('xlsx');           library('xlsx')       # handle Excel files



#------------------------------------------------------------------------
# Preparations
#------------------------------------------------------------------------

# List of US states
states <- state.name
states <- lapply(states, function(s) str_replace(s, " ", "_")) # replace whitespaces


# List of URLs
urls_senate <- lapply(states, function(s) paste0("https://ballotpedia.org/", s, "_State_Senate"))
urls_house  <- lapply(states, function(s) paste0("https://ballotpedia.org/", s, "_House_of_Representatives"))



#------------------------------------------------------------------------
# Scrape Ballotpedia
#------------------------------------------------------------------------

# Scrape using 'polite' package
results_senate <- list()
results_house <- list()
for (i in seq(length(states))) { 
  
  # Progress
  if (i==1) {cat("Scraping 'ballotpedia.org' for 50 states.\nProgress: ")}
  cat(i, "")
  
  #Senate
  url <- urls_senate[[i]]
  session <- bow(url)
  results_senate[[states[[i]]]] <- try(scrape(session))

  # House
  url <- urls_house[[i]]
  session <- bow(url)
  results_house[[states[[i]]]] <- try(scrape(session))
  
}
rm(i, url, session)


# Check success (should be 50)
length(results_senate)
length(results_house)


# Get all tables + save
tables_senate <- lapply(results_senate, function(l) l %>% html_nodes("table") %>% html_table())
tables_house  <- lapply(results_house,  function(l) l %>% html_nodes("table") %>% html_table())
saveRDS(tables_senate, "./ballotpedia_senates.RDS")
saveRDS(tables_house, "./ballotpedia_houses.RDS")
# tables_senate <- readRDS("./ballotpedia_senates.RDS")
# tables_house  <- readRDS("./ballotpedia_houses.RDS")


# Clean up
rm(urls_senate, urls_house, results_senate, results_house)



#------------------------------------------------------------------------
# Collect tables describing historical party composition 
#------------------------------------------------------------------------

# Identify position of partisan composition table in list by table format (MAY CHANGE IN FUTURE)
# Function
is_hist_table <- function(tble) {
  # Checks elements in first column
  row1_ok <- colnames(tble)[1] %in% c("Year", "Party")
  row2_ok <- tble[1,1] %in% c("Democrats", "Republicans")
  row3_ok <- tble[2,1] %in% c("Democrats", "Republicans")
  if (row1_ok & row2_ok & row3_ok) {res <- TRUE}
  else {res <- FALSE}
  return(res)
}
# For each state, get indices of history tables (1 per state)
histtab_indices_senate <- lapply(states, function(s) which(sapply(tables_senate[[s]], function(l) is_hist_table(l))))
histtab_indices_house  <- lapply(states, function(s) which(sapply(tables_house[[s]],  function(l) is_hist_table(l))))
names(histtab_indices_senate) <- states
names(histtab_indices_house)  <- states
rm(is_hist_table)


# Extract history table for each state
histtabs_senate <- lapply(states, function(s) tables_senate[[s]][[histtab_indices_senate[[s]]]])
histtabs_house  <- lapply(states, function(s) tables_house[[s]][[histtab_indices_house[[s]]]])
names(histtabs_senate) <- states
names(histtabs_house) <- states
rm(tables_senate, tables_house, histtab_indices_senate, histtab_indices_house)




#------------------------------------------------------------------------
# Create yearly tables from parsed history tables
#------------------------------------------------------------------------


### Helper objects
# Function: Transforms year format YY to YYYY (quick and dirty)
yy_to_yyyy <- function(yy) {
  yy <- as.numeric(yy)
  return((yy+1900)*(yy>=30) + (yy+2000)*(yy<30))
}  
# List of years
years <- tibble("year"=1990:2023)



### Main function
format_histtab <- function(tble) {
  
  # Prepare
  colnames(tble)[1] <- "Names"
  t1 <- tble %>% column_to_rownames("Names")
  
  # Transpose
  t2 <- tibble("year"         = colnames(t1), 
               "democrats"    = unlist(t1["Democrats",]), 
               "republicans"  = unlist(t1["Republicans",]), 
               "independents" = unlist(t1["Independents",]),
               "other"        = unlist(t1["Other",])) 
  
  # Format
  t3 <- t2 %>% 
    # Replace NA's for other and independents with 0
    mutate(independents = replace(independents, is.na(independents), 0),
           other        = replace(other,        is.na(other),        0)) %>% 
    # Extract notes 
    mutate(note = str_sub(year, start=str_locate(year, "\\[")[,1]),
           note = replace(note, is.na(note), "0")) %>% 
    # Fix year entries
    mutate(year = str_replace(year, "'", ""),
           year = str_sub(year, 1, 2),
           year = yy_to_yyyy(year) + 1) %>% 
    # Extract asterisks (if any)
    mutate(asterisk    = str_detect(democrats, "\\*") | str_detect(republicans, "\\*"),
           democrats   = str_extract(democrats, "[:digit:]*"),
           republicans = str_extract(republicans, "[:digit:]*"),
           across(democrats:other, ~ as.numeric(.x))) %>% 
    # Add rows for intermittent years
    full_join(years, by="year") %>% arrange(year) %>% 
    # Fill rows with latest value
    fill(democrats, .direction="down") %>% 
    fill(republicans, .direction="down") %>% 
    fill(independents, .direction="down") %>% 
    fill(other, .direction="down") %>% 
    fill(note, .direction="down") %>% 
    fill(asterisk, .direction="down")
  
  # Return
  return(t3)
} 


### Format all tables
histtabs_senate <- lapply(histtabs_senate, function(t) format_histtab(t))
histtabs_house  <- lapply(histtabs_house,  function(t) format_histtab(t))


### Consistency checks
# Look at tails
# lapply(histtabs_senate, function(t) tail(t))
# lapply(histtabs_house,  function(t) tail(t))
# Check column types
checks_senate <- lapply(states, function(s) {
  tble  <- histtabs_senate[[s]]
  types_ok <- is.numeric(tble$year) & is.numeric(tble$democrats) & is.numeric(tble$republicans) & is.numeric(tble$independents) & is.numeric(tble$other)
  if (types_ok) {return(TRUE)}
  else {return(FALSE)}
})
names(checks_senate) <- states
which(checks_senate!=TRUE)     # Should be empty
checks_house <- lapply(states, function(s) {
  tble  <- histtabs_house[[s]]
  types_ok <- is.numeric(tble$year) & is.numeric(tble$democrats) & is.numeric(tble$republicans) & is.numeric(tble$independents) & is.numeric(tble$other)
  if (types_ok) {return(TRUE)}
  else {return(FALSE)}
})
names(checks_house) <- states
which(checks_house!=TRUE)     # Should be empty


### Clean up
rm(years, yy_to_yyyy, format_histtab, checks_senate, checks_house)



#------------------------------------------------------------------------
# Combine to one dataframe
#------------------------------------------------------------------------

### Combine to one dataframe
partycomp_state_senate <- bind_rows(histtabs_senate, .id="state") %>% 
  filter(!is.na(democrats)) 
partycomp_state_house  <- bind_rows(histtabs_house, .id="state") %>% 
  filter(!is.na(democrats)) 


# Clean up
rm(histtabs_senate, histtabs_house)


#------------------------------------------------------------------------
# Finalize variables
#------------------------------------------------------------------------

### Merge + format new variables: Senate
partycomp_state_senate <- partycomp_state_senate %>% 
  # Create majority variables
  mutate(majority = case_when(
    democrats > republicans  ~ "democrats",
    democrats == republicans ~ "neither",
    democrats < republicans  ~ "republicans",
    TRUE                     ~ "unknown"
  )) %>% 
  # Fix state names
  mutate(state = str_replace(state, "_", " ")) %>% 
  # Mutate data format
  mutate(across(c(state, majority), ~ as.factor(.x))) %>% 
  # Change column order
  relocate(majority, .after=other)



### Merge + format new variables: House
partycomp_state_house <- partycomp_state_house %>% 
  # Create majority variables
  mutate(majority = case_when(
    democrats > republicans  ~ "democrats",
    democrats == republicans ~ "neither",
    democrats < republicans  ~ "republicans",
    TRUE                     ~ "unknown"
  )) %>% 
  # Fix state names
  mutate(state = str_replace(state, "_", " ")) %>% 
  # Mutate data format
  mutate(across(c(state, majority), ~ as.factor(.x))) %>% 
  # Change column order
  relocate(majority, .after=other) 




### Save: Full dataset
# R Dataset
saveRDS(partycomp_state_senate, "./stateYear_SenatePartisanComposition_BP.RDS")
saveRDS(partycomp_state_house,  "./stateYear_HousePartisanComposition_BP.RDS")
# CSV
write.csv(partycomp_state_senate , "./stateYear_SenatePartisanComposition_BP.csv", row.names=TRUE)
write.csv(partycomp_state_house,  "./stateYear_HousePartisanComposition_BP.csv", row.names=TRUE)



#------------------------------------------------------------------------
# CONSISTENCY CHECKS + SUMMARY STATISTICS: INTERNAL
#------------------------------------------------------------------------

### Senate -------------------------------------------------

# Summary
summary(partycomp_state_senate)


### Explore how many present
# Number of states
length(unique(partycomp_state_senate$state))
# Number of years by state
partycomp_state_senate %>% group_by(state) %>% count() %>% arrange(n) # Nebraska only included for 9 years
partycomp_state_senate %>% group_by(state) %>% count() %>% ungroup() %>% count(n) %>% arrange(n)


# Neither party has majority
partycomp_state_senate %>% filter(majority=="neither") %>% print(n=50) 


# Explore cite notes + asterisks
partycomp_state_senate %>% filter(note!="0")
partycomp_state_senate %>% filter(asterisk==TRUE)



### House -------------------------------------------------

# Summary
summary(partycomp_state_house)


### Explore how many present
# Number of states
length(unique(partycomp_state_house$state))
# Number of years by state
partycomp_state_house %>% group_by(state) %>% count() %>% arrange(n) # Nebraska included although unicameral
partycomp_state_house %>% group_by(state) %>% count() %>% ungroup() %>% count(n) %>% arrange(n)


# Neither party has majority
partycomp_state_house %>% filter(majority=="neither") %>% print(n=50) 


# Explore cite notes + asterisks
partycomp_state_house %>% filter(note!="0")
partycomp_state_house %>% filter(asterisk==TRUE)







