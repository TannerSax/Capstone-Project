# Packages
library(tidyverse) 
library(dplyr)
library(rvest)    
library(stringr)

# Open page in html
url = 'https://www.glassdoor.com/Job/new-york-data-analyst-jobs-SRCH_IL.0,8_IC1132348_KO9,21.htm'
pg = read_html(url)

# Get number of pages
get_last_page <- function(html){
  
  pages_data <- pg %>%
    html_nodes(".padVertSm") %>%
    html_text()
  
  pages_data %>% 
    regmatches(regexpr('[0-9][0-9][0-9]$', pages_data))
}
maxresults <- get_last_page()
url_base <- str_remove(url, ".htm")

# Create dataframe
df <- map_df(1:30, function(i) {
  
  Sys.sleep(sample(seq(1, 5, by=0.01), 1))   
  
  cat("boom! ")   #progress indicator
  
  list_of_pages <- read_html(paste0(url_base, "_IP", i, ".htm"))

  data.frame(Job = html_text(html_nodes(list_of_pages, '.jobTitle')) %>% 
               subset(. != ''), 
             Location = html_text(html_nodes(list_of_pages, '.loc')) %>% 
               subset(. != ''),
             Summary = html_text(html_nodes(list_of_pages, '.jl')),
             stringsAsFactors=F)
})

df <- df %>% 
  mutate(python = ifelse(grepl("python", Summary, ignore.case = TRUE), 1, 0)) %>% 
  mutate(sql = ifelse(grepl("sql", Summary, ignore.case = TRUE), 1, 0)) %>% 
  mutate(java = ifelse(grepl("java", Summary, ignore.case = TRUE), 1, 0))


