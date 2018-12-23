# Packages
library(tidyverse) 
library(dplyr)
library(rvest)    
library(stringr)

#https://www.glassdoor.com/Job/robinhood-jobs-SRCH_KE0,9.htm
#https://www.glassdoor.com/Job/ripple-jobs-SRCH_KE0,6.htm
#https://www.glassdoor.com/Job/uber-jobs-SRCH_KE0,4.htm
#https://www.glassdoor.com/Job/flexport-jobs-SRCH_KE0,8.htm

# Open page in html
url = 'https://www.glassdoor.com/Job/ripple-jobs-SRCH_KE0,6.htm'
pg = read_html(url)

# Get number of pages
get_last_page <- function(html){
  
  pages_data <- pg %>%
    html_nodes(".padVertSm") %>%
    html_text()
  
  pages_data %>% 
    regmatches(regexpr('[0-9]$|[0-9][0-9]$', pages_data))
}
maxresults <- get_last_page()
url_base <- str_remove(url, ".htm")

# Create dataframe
df <- map_df(1:maxresults, function(i) {
  
  Sys.sleep(sample(seq(1, 5, by=0.01), 1))   
  
  cat("boom! ")   #progress indicator
  
  list_of_pages <- read_html(paste0(url_base, "_IP", i, ".htm"))
  
  data.frame(rev.job = html_text(html_nodes(list_of_pages, '.jobLink')) %>% 
               subset(. != ' no.logo.alt') %>% 
               subset(. != ''), 
             rev.loc = html_text(html_nodes(list_of_pages, '.loc')) %>% 
             subset(. != ''),
             rev.sum = html_text(html_nodes(list_of_pages, '.jl')),
             stringsAsFactors=F)
})



