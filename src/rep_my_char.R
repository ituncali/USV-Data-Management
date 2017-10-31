# This function is a row-wise wrapper of rep
# The idea is that it maps the elements row wise
# so that rep understands that the repetition should be vectorized

rep_my_char <- function(char,times, collapse=F){
  
  # collapse = False gives you a list of character vectors
  # collapse = True gives you an unnamed character vector
  
  map_rep <- Map(rep, char, times)
  
  if(collapse==T){
    
    # We meed to collapse the length>1 vectors with space
    mylist <- lapply(map_rep, function(y,t) paste(y[t], collapse=" "))  
    
    # first we unlist it
    char_vec <- unlist(mylist)
    
    # later we remove the names
    
    names(char_vec) <- NULL
    
    return(char_vec)
    
  } else {
    
    # We meed to collapse the length>1 vectors with space
    mylist <- lapply(map_rep, function(y,t) paste(y[t]))
    
    return(mylist)
    
  }
  
  
}
