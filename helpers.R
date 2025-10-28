# Load libraries
library('dplyr')
library('tidyr')
library('janitor')
library('janitor')

# Function to take in .xlsx file, convert to .csv file and make 'tidy'
file_convert_to_tidy <- function(file, sheetSelected) {
  # Only supports sheets is Week to Week
  #ADD Period to Period IN THE FUTURE
  if (sheetSelected != "Week to Week" ||
      sheetSelected != "Period to Period") {
    stop("sheetSelected must be 'Week to Week' or 'Period to Period")
  }
  
  new_file <- read_excel(file, sheet = "Week to Week ", range = "A4:BR464")
  
  #Average_TPT
  #remove all the monthly averages
  if (sheetSelected == "Week to Week") {
    new_file <- new_file %>%
      select(-c("Pd 1", "Pd 2", "Pd 3", "Pd 4", "Pd 5", "Pd 6", "Pd 7", "Pd 8", "Pd 9", "Pd 10", "Pd 11", "Pd 12"))
    
    #remove empty columns. Each week a new column is filled in
    new_file <- new_file %>%
      remove_empty(which = "cols")
    
    #correcting col names
    for (i in 7:ncol(new_file)) {
      names(new_file)[i] <- paste("Week", i-6)
    }
    
    new_file_cleaned <- new_file %>% na.omit()
    
    # Create tidy version of data
    new_file_Tidy <- new_file_cleaned %>%
      gather(-c('Region', 'RT', 'DT', 'DM', `Site name`, `Site Code`), key='Week', value='TPT') %>%
      separate(Week, c(NA, 'Week'), sep=' ') %>%
      mutate(Week = as.numeric(Week))
  } 
  else {
    new_file <- new_file %>%
      select(c("Pd 1", "Pd 2", "Pd 3", "Pd 4", "Pd 5", "Pd 6", "Pd 7", "Pd 8", "Pd 9", "Pd 10", "Pd 11", "Pd 12"))
    
    new_file_cleaned <- new_file %>% na.omit()
    
    # Create tidy version of data
    new_file_Tidy <- new_file_cleaned %>%
      gather(-c('Region', 'RT', 'DT', 'DM', `Site name`, `Site Code`), key='Pd', value='TPT') %>%
      separate(Pd, c(NA, 'Pd'), sep=' ') %>%
      mutate(Pd = as.numeric(Pd))
  }
  
  
  
  
  
  
  
  
  return(new_file_Tidy)
}

#head(file_convert_to_tidy('C:\\Users\\ryans\\Documents\\TPT_ShinyApp\\Average TPT Report October 20 2025.xlsx',
 #                    "Week to Week"))
