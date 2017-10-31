# This function corrects for labels that are in the shape x flat y short
# it needs a helper function called rep_my_char, which is a mapper
# it will produce a list that needs to be bound


correct_labels <- function(dataframe){
  
  keep.running <- TRUE
  
  # keep all the fixed data.frames inside the list
  li <- list()
  
  # Very first time we do it from dataframe
  
  qq <- dataframe %>%
    mutate( 
      raw.extr = str_extract(string = label, pattern = "[0-9]+ [a-z]+"),
      number =   as.numeric(str_extract(string = raw.extr,
                                        pattern = "[0-9]+")),
      call.type = str_extract(string = raw.extr, pattern = "[a-z]+"),
      replica =  rep_my_char(call.type, number, collapse = TRUE),
      label = str_replace(string = label, pattern = raw.extr, 
                          replacement = replica)
    )
  
  # Apped to list
  
  li[[1]] <- qq
  
  # Very first time running
  li[[1]]$while.counter <- 1
  
  # start the while counter
  
  while.counter <- 2
  
  # Replicate data out so we can overwrite within the loop
  TOY <- qq
  
  while (keep.running) {
    
    # look for the rows that still have a number
    TOY <- TOY[grep("[0-9]", TOY$label), ]
    
    # Break if eveything there is na (label would be a character(0))
    
    if(identical(TOY$label, character(0))) break()
    
    
    TOY <- TOY %>%
      mutate( 
        raw.extr = str_extract(string = label, pattern = "[0-9]+ [a-z]+"),
        number =   as.numeric(str_extract(string = raw.extr,
                                          pattern = "[0-9]+")),
        call.type = str_extract(string = raw.extr, pattern = "[a-z]+"),
        replica = rep_my_char(call.type, number, collapse = TRUE),
        label = str_replace(string = label, pattern = raw.extr, 
                            replacement = replica)
      ) 
    
    
    print(paste("finished cleaning round number", while.counter))
    
    
    TOY$while.counter <- while.counter
    
    # save it on the counter
    li[[while.counter]] <- TOY
    
    
    while.counter <- while.counter + 1
    
    # Just in case...we don't want to run forever  
    if(while.counter > 10) break
    
  }
  
  
  
  return(li)
  
}
