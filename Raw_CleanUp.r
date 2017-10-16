
  
  #  Choose directory
  
  mypath <- choose.dir(getwd(),"Choose a Folder")
  
  # List of files, fix here if pattern needed
  
  myfiles <- paste0(mypath,'\\',list.files(path = mypath))
  
  # Takes a while if coming from excel, be patient!
  #readxl
  
  myRead <- function(list.element){
    
    rawraw <- read_xlsx(list.element, sheet=1)
    
    #need to put something in here that adds a "file" column to each data set!!
    
    rawraw <- mutate(rawraw, 
                     file = stringr::str_extract(string = list.element, 
                                                 pattern = "T[0-9]+"))
    
    return(rawraw)
    
  }
  
  #takes a list of files from a folder and reads them one by one....
  
  read.files <- lapply(myfiles, myRead)
  
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
  
  lista <- lapply(read.files, just_calls)

  BIG <- bind_rows(lista)

  ##### Select only necesary columns
  #the necessary columns depend on what we're doing with the data!!!
  #if we're only counting: labels
  #if we're analyzing parameters: labels, duration, f1:f50
  #if we're analyzing behavior: labels, start time, duration, f1:f50
  #i'll just do the most possible for now...
  #having start time will 
  #ACTUALLY... why not just keep most of the columns for now... 10-16-17
  
  #onlycolumnsnecessary <- c(2:4,7:56)
  #BIG <- BIG[,onlycolumnsnecessary]
  
  byecolumns <- c(1,5,6)
  BIG <- BIG[,-byecolumns]
  
  ##### Correctly name the columns
  
  names(BIG) <- c("label", "duration", "start.time", paste0("f",1:50), "file")
  
  #Then put the file column at the front
  
  BIG <- select(.data = BIG, file, label:f50)
  
  
  ##### before counting, first, manage FAINT and noise, - harm, - harmx, 
  
  #tidyr::separate(BIG, col = label,
  #                into = c("label", "extra_tag"),
  #                sep = " - Matias - ")
  
  # Regular expressions in R
  
  
  #BIG$label <- gsub(pattern = " NOISE", replacement = "", x = BIG$label)
  #do this to everything or put things.to.erase in pattern
  
  thing.to.erase <- c(" FAINT| NOISE| - harm| - harmx| - fragment")
  
  BIG$label <- gsub(pattern = thing.to.erase, replacement = "", x = BIG$label)
  
  ######
  #pru <- c("hola", "me", 'mea', "meaw")
  ## Examples to play with
  #> gsub( c("me|a|h"), "", pru)
  #[1] "ol" ""   ""   "w" 
  #> gsub( c("me|a| h"), "", pru)
  #[1] "hol" ""    ""    "w"  
  #######
  
  
  
  ### Counting ALL CALLS ####
  #apply(expand.grid(c(1,2,3), c("flat", "short")), 1, paste, collapse=" ")
  #str_count(example, "flat")
  
  #before count_total, want to change "3 flat" to "flat flat flat", etc...
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
      replica = rep_my_char(call.type, number, collapse = TRUE),
      label = str_replace(string = label, pattern = raw.extr, 
                          replacement = replica)
      )
  
  # Apped to list
  
  li[[1]] <- qq
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
    
    # save it on the counter
    li[[while.counter]] <- TOY
    
    
    while.counter <- while.counter + 1
    
    # Just in case...we don't want to run forever  
    if(while.counter > 10) break
      
  }
  
  
  
  return(li)
  
}
  
  
  
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
  
  
  
  
  
  #now count!
count_total <-  function(dataframe, categories.allowed){
    
    
    y <- dataframe$label
    
    
    li <- lapply(categories.allowed, function(q) sum(str_count(y,q)))
    
    
    out <- unlist(li)
    
    out <- data.frame(categories.allowed=categories.allowed,
                      total.counts = out)
    

    return(out)
  }
  
  
  
  
  
  ### to filter only the categories ####
  
    
  ##### Get rid of all the overlap
  # Figure a way to count
  ##make a list of the categories
  allowed.categories <- c("flat", "flat-z", "flat-mz", "short", "short-su", "short-sd",
                          "short-ur", "short-dr", "short-c", "complex", "upward ramp",
                          "downward ramp", "step up", "step down", "multi-step", "multi-step-s",
                          "trill", "trill-c", "trill-f", "inverted-U", "unclear") 
  #need to make count.flag
  
  #so this is designating which labels are single calls and which labels are overlapping calls
  #it does NOT count the types of call in any way
  
  BIG %>% mutate(count.flag = ifelse(label %in% allowed.categories,
                                     "allowed call",
                                     "overlapping call")) %>% select(count.flag) %>% tally()
  
  filter(BIG, label %in% allowed.categories)
  
  
  ### Parameter Shit ####
  # Get rid of all the overlap
  
  
  
  
  
  