library(tidyverse)  
library(rvest)    
library(stringr)   
library(lubridate)

url <- 'https://www.glassdoor.com/Jobs/Databricks-Jobs-E954734.htm'
html <- read_html(url)


get_last_page <- function(html){
  
  pages_data <- html %>% 
    # The '.' indicates the class
    html_nodes('.page') %>% 
    # Extract the raw text as a list
    html_text()                   
  
  # The second to last of the buttons is the one
  pages_data[(length(pages_data))] %>%            
    # Take the raw string
    unname() %>%                                     
    # Convert to number
    as.numeric()                                     
}

first_page <- read_html(url)
(latest_page_number <- get_last_page(first_page))

