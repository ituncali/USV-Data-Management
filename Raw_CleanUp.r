
# Loading packages
source("src/my_loader.R")
my_loader(c("dplyr","ggplot2","stringr","tidyr", "xlsx"))


#####READ IN THE DATA#####
  
  #  Choose directory
  
  mypath <- choose.dir(getwd(),"Choose a Folder")
  
  # DATA SHOULD GO INTO A FOLDER WITHIN THIS PROJECT "/data"
  
  
  # List of files, fix here if pattern needed
  
  myfiles <- paste0(mypath,'\\',list.files(path = mypath))
  
  # Read data into list
  # takes a list of files from a folder and reads them one by one....
  
  source("src/myRead.R")
  read.files <- lapply(myfiles, myRead)
  
  # Get Rid of spurious things in the file, get "just the calls" :)
  
  source("src/just_calls.R")
  
  # Warning about NAs introduced by coercion is normal
  lista <- lapply(read.files, just_calls)
  
  # Bind the things together into a BIG BIG list :)
  BIG <- bind_rows(lista)
  
  #Add unique.id column
  BIG <- mutate(BIG, unique.id = c(1:nrow(BIG)))

  # Select only necesary columns
  # the necessary columns depend on what we're doing with the data!!!
  # if we're only counting: labels
  # if we're analyzing parameters: labels, duration, f1:f50
  # if we're analyzing behavior: labels, start time, duration, f1:f50
  # i'll just do the most possible for now...
  # having start time will 
  # ACTUALLY... why not just keep most of the columns for now... 10-16-17
  
  
  # cleanup for some weird columns that we don't care about
  BIG <- select(BIG, -`#`, -`end time`, -`peak ampl(mean)`)
  
  ##### Correctly name the columns
  
  names(BIG) <- c("label", "duration", "start.time", paste0("f",1:50), "file.name", "unique.id")
  
  #Then put the file column at the front
  
  BIG <- select(.data = BIG, unique.id, file.name, label:f50)
  
  
  ##### before counting, first, manage FAINT and noise, - harm, - harmx, 
  
  thing.to.erase <- c(" FAINT| NOISE| - harm| - harmx| - fragment")
  
  BIG$label <- gsub(pattern = thing.to.erase, replacement = "", x = BIG$label)
  

  #### Counting ALL CALLS ####
  #apply(expand.grid(c(1,2,3), c("flat", "short")), 1, paste, collapse=" ")
  #str_count(example, "flat")
  
  #before count_total, want to change "3 flat" to "flat flat flat", etc...

 
 #### Overlapping calls ####
  
  # We will separate overlapping of the type x flat y short
  # from all other calls
  
 # we make a flag to subset the ones that need to be fixed  
 BIG$flag <- ifelse(grepl(pattern = "[0-9]+ [a-z]+", x = BIG$label), "subset.me", "leave.alone")
  
  needs_fixing <- filter(BIG, flag=="subset.me")

# -----
 # Here is where solutions for correct_labels and bind that list and doing something with the multiple counts will come
# ----
    source("src/rep_my_char.R")
    source("src/correct_labels.R")
  
  #Run the function
  new_labels <- correct_labels(needs_fixing)
  #so this will return a list with indices of the rounds... we want a dataframe
  
  new_labels <- bind_rows(new_labels)
  
  #now we want to group by unique.id and filter by the largest while.counter
  
  new_labels <- new_labels %>%
    group_by(unique.id) %>%
    filter(while.counter==max(while.counter))
  
  #then get rid of extra columns
  
  new_labels <- select(new_labels, -`raw.extr`, -`number`, -`call.type`, -`replica`, -`while.counter`)
  
  #then, replace those rows that were originally subsetted with new stuff
  
  BIG <- bind_rows(filter(BIG, flag=="leave.alone"),new_labels)
  #then remove flag column
  BIG <- select(BIG, -`flag`)
  
  
  #now count!

  source("src/count_total.R")
  
  ##make a list of the categories
  allowed.categories <- c("flat", "flat-z", "flat-mz", "short", "short-su", "short-sd",
                          "short-ur", "short-dr", "short-c", "complex", "upward ramp",
                          "downward ramp", "step up", "step down", "multi-step", "multi-step-s",
                          "trill", "trill-c", "trill-f", "inverted-U", "unclear") 
  
# by factor
  
count_list <- by(data = BIG, INDICES = BIG$file.name, FUN = function(x) count_total(x, allowed.categories))

count_frame <- do.call(rbind, count_list)

# Get the rows to be a formal column and erase them
count_frame$file.name <- row.names(count_frame)
row.names(count_frame) <- NULL

# Clean the files (we remove the ending in point and numbers, .[0-9]+$ pattern) 

count_frame$file.name <- str_replace(count_frame$file.name, pattern = ".[0-9]+$", replacement =  "" )

# we update with counts by file and counts by 
count_frame <- count_frame %>% group_by(file.name) %>%
  mutate(total.filecounts=sum(total.counts), rel.filecount=total.counts/total.filecounts)

# stacked barplot for totals
count_frame %>% ggplot(aes(file.name, rel.filecount, fill=categories.allowed)) + geom_bar(stat='identity') 

# by file, total counts 

count_frame %>% group_by(file.name) %>%
  summarise(total.filecounts = unique(total.filecounts)) %>%
  ggplot(aes(file.name, total.filecounts)) + geom_bar(stat='identity', fill="white", color="black")


# by file, total counts, colored by call type
count_frame %>% ggplot(aes(file.name, total.counts, fill=categories.allowed)) + geom_bar(stat='identity')


# correlation between number of calls and call type

count_frame %>% ggplot(aes(total.filecounts, rel.filecount, group=categories.allowed, color=categories.allowed)) +
  geom_point() + geom_line()

#stacked bar plots faceted by strain
plot_frame_1 <- strain_match(count_frame,which_are_WK)
plot_frame_1 %>% ggplot(aes(file.name, total.counts, fill=categories.allowed)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~strain, scale = "free")

#to simply put sd to one side and wk to the other
plot_frame %>% ggplot(aes(interaction(file.name, strain), total.counts, fill=categories.allowed)) + 
  geom_bar(stat='identity')

# write.csv(BIG, "AASDJSAIDjSDJDJJDJDJDJD.csv", row.names = FALSE)

#histogram of time bins
#first add strain column
#for mom_anesth...... need to figure out a way to figure this out....
source("src/strain_match.R")
  ## for MOTHER-LITTER INTERACTION RECORDINGS
which_are_WK <- c("T0000091", "T0000096", "T0000124", "T0000185", "T0000190", "T0000228", "T0000303")
  ## for MOM ANESTHETIZED RECORDINGS
which_are_WK <- c("T0000102","T0000109","T0000138","T0000199","T0000203","T0000233","T0000308")
  ## for MSX-3 RECORDINGS
which_are_WK <- c("T0000291","T0000292","T0000297","T0000058","T0000060","T0000061")
  ## for WKY behavioral baseline controls
which_are_WK <- c("T0000001", "T0000020", "T0000025", "T0000027", "T0000029")
plot_frame <- strain_match(BIG,which_are_WK)

myplot <- ggplot(plot_frame, aes(start.time, fill=strain))

myplot + geom_histogram(bins = 60, position='dodge', color='white') + geom_vline(xintercept = 300)

myplot + geom_histogram(bins = 60, color='black') +
  
  facet_wrap(~strain, nrow=2) +
  
  geom_vline(xintercept = c(150,300,450,600)) + theme_classic()

myplot + geom_density(alpha=0.5) + facet_wrap(~strain, nrow=2)


#strain before ~file.name so that SDs are all on top and WKs are all on bottom
myplot + geom_histogram(bins = 60, color='black') +
  
  facet_wrap(strain~file.name, nrow=2) +
  
  geom_vline(xintercept = c(150,300,450,600)) + theme_classic()







  ### to filter only the categories ####
  
    
  ##### Get rid of all the overlap
  # Figure a way to count
 
  #need to make count.flag
  
  #so this is designating which labels are single calls and which labels are overlapping calls
  #it does NOT count the types of call in any way
  
  BIG %>% mutate(count.flag = ifelse(label %in% allowed.categories,
                                     "allowed call",
                                     "overlapping call")) %>% select(count.flag) %>% tally()
  
  filter(BIG, label %in% allowed.categories)
  
  
  ### Parameter Shit ####
  # Get rid of all the overlap
  
  
  
  
  
  