library(tidyverse)
library(lubridate)

# folder <- choose.dir(caption = "Select main analysis folder")
folder <- "analysis/data/"

# load mastertable. Change for analysis of different data set
mastertable <- read.csv2(file = paste0(folder, "metadata/mastertable.csv"), header = TRUE, sep = ";",
                         na.strings = "NA")
# load master list of column labels
mastercolnames <- read.csv2(file = paste0(folder, "metadata/mastercolnames.csv"),
                            header = FALSE, sep = ";") %>%
  unlist()

# load Table of conditions. Change for analysis of different data set
conditions <- read.csv2(file = paste0(folder, "metadata/conditions.csv"), header = TRUE, sep = ";")

# helper function to read csv file with raw data and preprocess them
load_raw_csv <- function(path) {
  nthday <- read.csv2(file = path,  sep=";", dec = ",", header = TRUE,
                      fileEncoding = "UTF-16LE", as.is = TRUE, row.names = NULL)
  
  colnames(nthday) <- mastercolnames  # rename column labels with correct order

  nthday <- nthday %>%
    arrange(DateTime) %>% 
    arrange(DateTime) %>% #sort chronologically
    mutate(DateTime = as.numeric(str_replace(DateTime, ",", ".")),
           DateTime = as.POSIXct(as.numeric(DateTime) * (60 * 60 * 24),
                                 origin = "1899-12-30", tz = "UTC"))

  #find out first line of useful data
  firstrow <- nthday %>%
    mutate(rown = row_number(DateTime)) %>%
    filter(SystemMsg == "start") %>%
    summarise(firstrow = max(rown) + 1) %>%
    pull()
  if (is.na(firstrow) | is.infinite(firstrow)) { # in rare cases the start line is missing
    # then take the first rewarded visit of an animal as the firstrow
    firstrow <- nthday %>%
      mutate(rown = row_number(DateTime)) %>%
      filter(IdLabel != "test", reinforce1value > 0) %>%
      summarise(firstrow = min(rown)) %>%
      pull()
  }

  # find the last row number, larger than firstrow and preceding a row with "exp" in the
  # "unitLabel" column
  lastrow <- c()
  lastrow <- nthday %>%
    mutate(rown = row_number(DateTime)) %>%
    filter(SystemMsg == "end", rown > firstrow, str_detect(IdLabel, "I")) %>%
    summarise(lastrow = min(rown) - 1) %>%
    pull()
  if (is.na(lastrow) | is.infinite(lastrow)) {lastrow <- nrow(nthday)}

  #only select relevant data and discard the rest
  nthday <- nthday %>%
    slice(firstrow:lastrow)
}

days <- as.list(mastertable$day)
paths <- as.list(paste0(folder, mastertable$path))
# path <- paths %>%  pluck(2)

# function for aggregating data from all days and adding correct day column
aggregate_days <- function(paths, days) {
  map(paths, load_raw_csv) %>%
    set_names(days) %>%
    enframe("day", "day_data") %>%
    unnest(day_data) %>%
    mutate(day = as.numeric(day))
}

alldays <- aggregate_days(paths, days) %>% 
  arrange(day, DateTime) #sort by day and chronologically

# remove columns with irrelevant data
alldays <- alldays %>%
  # filter(!str_detect(IdLabel, "T"), IdLabel != "") %>%
  select(day, DateTime, IdLabel, IdRFID, unitLabel, eventDuration, reinforce1value, SystemMsg)

# mark when pump was in busy state
alldays <- alldays %>% 
  mutate(pumping = case_when(
    DateTime == min(DateTime) ~ 0,
    SystemMsg == "" ~ NA_real_,
    str_detect(SystemMsg, "start p") ~ 1,
    str_detect(SystemMsg, "end p") ~ 0)) %>%
  fill(pumping)

# repair missing identities
id_table <- conditions %>% 
  select(day, IdRFID, Id_actual = IdLabel) %>% 
  distinct()

alldays <- alldays %>% 
  left_join(id_table, by = c("day", "IdRFID")) %>%
  mutate(IdLabel = Id_actual) %>%
  select(-Id_actual) %>% 
  filter(str_detect(IdLabel, "I") | str_detect(unitLabel, "pump"))

# make new columns
alldays <- alldays %>%
  group_by(day, IdLabel) %>% 
  mutate(
    # for flexibility test, phase 1 = first pair of flowers rewarding, and phase 2 = second pair of flowers rewarding
    phase = ifelse(SystemMsg == "switch", 1, 0),
    phase = cumsum(phase) + 1,
    # numeric column for reward volume delivered
    vol = replace_na(reinforce1value, 0),
    # numeric column for reward status 1=rewarded, 0=unrewarded, helps calculate reward rates
    rewarded = ifelse(vol > 0, 1, vol),
    # create location column from the unitLabels
    loc = as.numeric(str_extract(unitLabel, "[:digit:].*$"))
    ) %>%
  select(-reinforce1value) %>% 
  ungroup()

# merge the conditions table and the current data table to make new columns for dispenser properties
# such as maximal volume, sugar concentration, probability, etc., as well as experimental conditions
# days to discard, etc.
conditions_table <- conditions %>% 
  select(-loc, -rewarding, -IdRFID, prob_cond = prob) %>% 
  distinct() %>% 
  mutate(discard = as.character(discard))

flower_table <- conditions %>%
  select(day, IdLabel, loc, prob, rewarding)
  
add_conditions_discard_data <- function(tbl, conditions_table) {
  #discard data labeled for discarding in conditions table
  date_filters <- any(!conditions_table$discard %in% c(1, 0))
  
  tbl_w_conds <- tbl %>%
    left_join(conditions_table, by = c("day", "IdLabel")) %>%
    select(day, IdLabel, loc, everything()) %>% 
    mutate(discard = replace_na(discard, 0))
  
  if (!date_filters) {
    tbl_filtered <- tbl_w_conds %>% 
             filter(discard == 0) %>%
             select(-discard)
  } else {
    tbl_filtered <- tbl_w_conds %>% 
      group_by(day, IdLabel) %>% 
      mutate(start_date = as.POSIXct(as.Date(min(DateTime))),
             time_after = str_extract(discard, "(?<=>)[:digit:]{2}:[:digit:]{2}"),
             time_before = str_extract(discard, "(?<=<)[:digit:]{2}:[:digit:]{2}"),
             discard = as.numeric(discard == 1),
             time_after = case_when(
               is.na(time_after) & is.na(time_before) ~ NA_POSIXct_,
               is.na(time_after) & !is.na(time_before) ~ start_date,
               hm(time_after) < hm("17:00") ~ start_date + days(1) + hm(time_after),
               TRUE ~ start_date + hm(time_after)
             ),
             time_before = case_when(
               is.na(time_before) & is.na(time_after) ~ NA_POSIXct_,
               is.na(time_before) ~ start_date + days(2),
               hm(time_before) < hm("17:00") ~ start_date + days(1) + hm(time_before),
               TRUE ~ start_date + hm(time_before)
             ),
             discard_times = ifelse(DateTime %within% interval(time_after, time_before), 1, 0),
             discard_times = replace_na(discard_times, 0),
             discard = discard_times == 1 | discard == 1)  %>% 
      filter(!discard) %>%
      ungroup() %>% 
      select(-time_before, -time_after, -start_date, -discard_times, -discard)
  }
tbl_filtered
}

alldays <- alldays %>%
  add_conditions_discard_data(conditions_table) %>% 
  left_join(flower_table, by = c("day", "IdLabel", "loc")) %>% 
  # sort chronologically again
  arrange(DateTime) %>% 
  mutate(loc = factor(loc),
         weight = as.numeric(as.character(weight)))

alldays <- alldays %>%
  #boolean column for whether an event was a choice at a flower (RFID signal and IR beam interrupted):
  mutate(choice = !is.na(loc) & str_detect(unitLabel, "Cond"),
         prob = as.numeric(as.character(prob)),
         prob = replace_na(prob, 0),
         rewarding = replace_na(rewarding, 0))

write.table(alldays, file = paste0(folder, "EventData.csv"), sep = ";", row.names = FALSE)
