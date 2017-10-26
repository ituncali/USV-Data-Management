# This function is meant to be used to clean data coming from Avisoft
# It gets rid of NAs, any lables with just a number in it
# It will give you "just the calls"


just_calls  <- function(dataframe){
  
  # Labels get contaminated with NA or numbers
  # Be aware numbers will be read as characters
  # Rationale of the filter built in just_calls function
  
  # get rid of NAs
  
  dataframe <- dataframe[!is.na(dataframe$label),]
  
  # We coerce to numbers because labels get contaminated with numbers
  
  flag <- as.numeric(dataframe$label)
  
  # now we want to keep all the things that are NAs in this flag
  
  dataframe <- dataframe[is.na(flag),]
  
  return(dataframe)
  
}

