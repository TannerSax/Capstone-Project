library(tidyverse)  
library(rvest)    
library(stringr)

#     Company Links - Quick Reference
# https://www.glassdoor.com/Jobs/Uber-Jobs-E575263.htm
# https://www.glassdoor.com/Jobs/Robinhood-Jobs-E1167765.htm
# https://www.glassdoor.com/Jobs/Lyft-Jobs-E700614.htm
# https://www.glassdoor.com/Jobs/Rubrik-Jobs-E955861.htm

      # Load Company Data
url <- 'https://www.glassdoor.com/Jobs/Rubrik-Jobs-E955861.htm'
html <- read_html(url)

      # Filter through pages - Obtain List

get_last_page <- function(html){

  pages_data <- html %>%
    # The '.' indicates the class
    html_nodes(".page.last") %>%
    # Extract the raw text as a list
    html_text()
  
  pages_data[(length(pages_data))] %>%
    # Take the raw string
    unname() %>%
    # Convert to number
    as.numeric()
}

first_page <- read_html(url)
(latest_page_number <- get_last_page(first_page))

url_base <- str_remove(url, ".htm")

list_of_pages <- str_c(url_base, "_P", grep("[0-9]", 1:latest_page_number, value = TRUE), ".htm")

list_of_pages


  
     # Create columns for position, location, Data_ID

# load job attributes as dataframe, remove unnecessary lines
job_attr <- function(html){
  html %>%  
  html_nodes("#EmployerJobs") %>% 
  html_nodes("li") %>% 
  html_attrs() %>% 
  unlist() %>% 
  as.data.frame() %>% 
  filter(. != "span-1-2 inlineBlock blockMob") 
}

  job_df <- job_attr(html)

# create columns for position, location, Data_ID
  position <- job_df[,1][seq(from = 9, to = (length((job_df)[,1])-7), by = 13)] %>% as.data.frame()
  location <- job_df[,1][seq(from = 10, to = (length((job_df)[,1])-7), by = 13)] %>% as.data.frame()
  Data_ID <- job_df[,1][seq(from = 2, to = (length((job_df)[,1])-7), by = 13)] %>% as.data.frame()

  View(position)
# bind columns and name them
df <- bind_cols(position, location, Data_ID) 
colnames(df) <- c("Position", "Location", "Data_ID")

View(df)
