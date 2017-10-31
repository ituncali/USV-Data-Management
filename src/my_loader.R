
###### Load Packages #####
# This function loads packages without any warning/message
# It produces an error if the packages cannot be loaded

# Example of use
# .packages <- c('xlsx','dplyr','lubridate','tidyr','reshape2','stringr','janitor')
# my_loader(.packages)

my_loader <- function(.packages){
  pack_list <- lapply(.packages,
                      function(t) suppressMessages(suppressWarnings(require(t, character.only = T))))
  
  # we DO want to find if a package could not be loaded
  if(any(pack_list==F)){
    pack_error <- which(pack_list==F)
    
    message('Following package(s) could not be loaded')
    print(.packages[pack_error])
    
  }
} 