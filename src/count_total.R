count_total <-  function(dataframe, categories.allowed){
  
  
  y <- dataframe$label
  
  #need to add (?!-) so that flat-z, etc. don't get counted as flat, etc.
  categories.allowed.search <- paste0(categories.allowed, "(?!-)")
  
  li <- lapply(categories.allowed.search, function(q) sum(str_count(y,regex(q, ignore_case = T))))
  
  
  out <- unlist(li)
  
  #took this out so that function returns a vector!! easier to manipulate into data.frames that I want...
  out <- data.frame(categories.allowed=categories.allowed,
                    total.counts = out)
  
  
  
  
  return(out)
}

