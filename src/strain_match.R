#function that take input of what files are what strain and add a strain column to the dataframe

strain_match <- function(dataframe, which_are_WK) {
  
  #which_are_WK is a string vector containing the file names that are WK files
  
  frame <- dataframe %>% mutate(strain = 
                                 ifelse(file.name %in% which_are_WK,"WK","SD"))
  
}