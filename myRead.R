library(readxl)

myRead <- function(list.element){
  
  rawraw <- read_xlsx(list.element, sheet=1)
  
  #need to put something in here that adds a "file" column to each data set!!
  
  rawraw <- mutate(rawraw, 
                   file = stringr::str_extract(string = list.element, 
                                               pattern = "T[0-9]+"))
  
  return(rawraw)
  
}
