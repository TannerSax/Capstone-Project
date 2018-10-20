library(tidyverse)  
library(rvest)    

#     Company Links - Quick Reference
# https://www.glassdoor.com/Jobs/Uber-Jobs-E575263.htm
# https://www.glassdoor.com/Jobs/Robinhood-Jobs-E1167765.htm
# https://www.glassdoor.com/Jobs/Lyft-Jobs-E700614.htm
# https://www.glassdoor.com/Jobs/Rubrik-Jobs-E955861.htm

      # Load Company Data
url <- 'https://www.glassdoor.com/Jobs/Lyft-Jobs-E700614.htm'
html <- read_html(url)

      # Filter through pages **INCOMPLETE**

# get_last_page <- function(html){
# 
#   pages_data <- html %>%
#     # The '.' indicates the class
#     html_nodes('.page') %>%
#     # Extract the raw text as a list
#     html_text()
# 
#   # The second to last of the buttons is the one
#   pages_data[(length(pages_data)-1)] %>%
#     # Take the raw string
#     unname() %>%
#     # Convert to number
#     as.numeric()
# }
# 
# first_page <- read_html(url)
# (latest_page_number <- get_last_page(first_page))



     # Create columns for position, location, Data_ID

# load job attributes as dataframe, remove unnecessary lines
job_attr <- html %>%  
  html_nodes("#EmployerJobs") %>% 
  html_nodes("li") %>% 
  html_attrs() %>% 
  unlist() %>% 
  as.data.frame() %>% 
  filter(. != "span-1-2 inlineBlock blockMob") 


# create columns for position, location, Data_ID
position <- job_attr[,1][seq(from = 9, to = length(job_attr[,1])-7, by = 13)] %>% as.data.frame()
location <- job_attr[,1][seq(from = 10, to = length(job_attr[,1])-7, by = 13)] %>% as.data.frame()
Data_ID <- job_attr[,1][seq(from = 2, to = length(job_attr[,1])-7, by = 13)] %>% as.data.frame()

# bind columns and name them
df <- bind_cols(position, location, Data_ID) 
colnames(df) <- c("Position", "Location", "Data_ID")


View(df)
