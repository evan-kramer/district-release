library(tidyverse)
library(janitor)

setwd(str_c("N:/ORP_accountability/projects/", year(now()), "_district_release/Determination Letters/Letters"))

# List of files
file_list = tibble(
  filename = list.files(), 
  system = str_sub(filename, 1, 3) %>% 
    str_replace_all("_", "") %>% 
    as.numeric()
) %>% 
  filter(!is.na(system))

# Loop
for(d in sort(unique(file_list$system))) {
  file_vector = file_list$filename[file_list$system == d]
  
  if(length(file_vector > 0)) {
    zip(
      zipfile = str_c(d, "_school_letters.zip"),
      files = file_vector,
      flags = " a -tzip", 
      zip = "C:/Program Files/7-Zip/7Z") # Had to download the 7-Zip program and point the function to it
  }
}

# Current status
round(
  100 * length(list.files("N:/ORP_accountability/projects/2019_district_release/Determination Letters/Letters")) / 
  read_csv("N:/ORP_accountability/projects/2019_district_release/Code/school_universe.csv") %>% nrow(), 1
)
