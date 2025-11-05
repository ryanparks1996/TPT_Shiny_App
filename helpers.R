# Load libraries
library('dplyr')
library('tidyr')
library('janitor')
library('janitor')
library('tibble')
library('lubridate')

# Function to take in .xlsx file, convert to .csv file and make 'tidy'
file_convert_to_tidy <- function(file, sheetSelected) {
  # Only supports sheets is Week to Week
  # ADD Period to Period IN THE FUTURE
  if (sheetSelected != "Week to Week") {
    stop("sheetSelected must be 'Week to Week'")
  }
  
  new_file <- read_excel(file, sheet = "Week to Week ", range = "A4:BR464")
  
  # Clean data by setting all names to uppercase for proper filtering later
  new_file$Region <- toupper(new_file$Region)
  new_file$DM <- toupper(new_file$DM)
  new_file$DT <- toupper(new_file$DT)
  new_file$`Site name` <- toupper(new_file$`Site name`)
  
  # Note: some times stores will not have DT/DM 
  # This will fill in empty data with "NA"
  new_file$RT[is.na(new_file$RT)] <- "NA"
  new_file$DT[is.na(new_file$DT)] <- "NA"
  new_file$DM[is.na(new_file$DM)] <- "NA"
  
  # Average_TPT
  # Remove all the monthly averages
  if (sheetSelected == "Week to Week") {
    new_file <- new_file %>%
      select(-c("Pd 1", "Pd 2", "Pd 3", "Pd 4", "Pd 5", "Pd 6",
                "Pd 7", "Pd 8", "Pd 9", "Pd 10", "Pd 11", "Pd 12"))
    
    # Note: week 11 is missing from all stores
    `Week 11` <- rep(NULL, nrow(new_file))
    
    # Remove empty columns. Each week a new column is filled in
    new_file <- new_file %>%
      remove_empty(which = "cols") 
    
    # Correcting col names
    for (i in c(7:16)) {
      names(new_file)[i] <- paste("Week", i-6)
    }
    for (i in c(17:ncol(new_file))) {
      names(new_file)[i] <- paste("Week", i-5)
    }
    
    new_file_cleaned <- new_file 
    
    # Create tidy version of data
    new_file_Tidy <- new_file_cleaned %>%
      gather(-c('Region', 'RT', 'DT', 'DM', `Site name`, `Site Code`), key='Week', value='TPT') %>%
      separate(Week, c(NA, 'Week'), sep=' ') %>%
      mutate(Week = as.numeric(Week))
    
  }
  # sheetSelected = 'Period to Period' (not yet supported)
  else {
    new_file <- new_file %>%
      select(c("Pd 1", "Pd 2", "Pd 3", "Pd 4", "Pd 5", "Pd 6",
               "Pd 7", "Pd 8", "Pd 9", "Pd 10", "Pd 11", "Pd 12"))
    
    new_file_cleaned <- new_file #%>% na.omit()
    
    # Create tidy version of data
    new_file_Tidy <- new_file_cleaned %>%
      gather(-c('Region', 'RT', 'DT', 'DM', `Site name`, `Site Code`), key='Pd', value='TPT') %>%
      separate(Pd, c(NA, 'Pd'), sep=' ') %>%
      mutate(Pd = as.numeric(Pd))
  }
  
  
  return(new_file_Tidy)
}

####------------------------------------------------------------------------####

# Function to convert excel date values to r Date
get_dates <- function(file) {
  # Read in file
  new_file <- read_excel(file, sheet = "Week to Week ", range = "A4:BR464")
  
  # Remove all monthly averages
  new_file <- new_file %>%
    select(-c("Pd 1", "Pd 2", "Pd 3", "Pd 4", "Pd 5", "Pd 6", 
              "Pd 7", "Pd 8", "Pd 9", "Pd 10", "Pd 11", "Pd 12"))
  
  # Save column names to a variable and convert from string to numeric
  dates <- as.numeric(names(new_file)[7:ncol(new_file)])
  
  # Correct column names
  dates_formated <- as.Date(dates, origin = "1899-12-30")
  
  return(dates_formated)
}


