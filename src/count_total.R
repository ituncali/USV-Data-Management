count_total <-  function(dataframe, categories.allowed){
  
  
  y <- dataframe$label
  
  
  li <- lapply(categories.allowed, function(q) sum(str_count(y,q)))
  
  
  out <- unlist(li)
  
  #took this out so that function returns a vector!! easier to manipulate into data.frames that I want...
  out <- data.frame(categories.allowed=categories.allowed,
                    total.counts = out)
  
  
  return(out)
}

