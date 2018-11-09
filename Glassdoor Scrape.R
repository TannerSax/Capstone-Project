library(tidyverse) 
library(dplyr)
library(rvest)    
library(stringr)

#https://www.glassdoor.com/Job/robinhood-jobs-SRCH_KE0,9.htm
#https://www.glassdoor.com/Job/ripple-jobs-SRCH_KE0,6.htm
#https://www.glassdoor.com/Job/uber-jobs-SRCH_KE0,4.htm
#https://www.glassdoor.com/Job/flexport-jobs-SRCH_KE0,8.htm

# Open page in html
url = 'https://www.glassdoor.com/Job/uber-jobs-SRCH_KE0,4.htm'
pg = read_html(url)

# Company
comp_end = regexpr("[-]", url)
company = substr(url, 31, comp_end-1)

# Filter Pages
get_last_page <- function(html){
  
  pages_data <- pg %>%
    # The '.' indicates the class
    html_nodes(".padVertSm") %>%
    # Extract the raw text as a list
    html_text()
  
    pages_data %>% 
      regmatches(regexpr('[0-9]$|[0-9][0-9]$', pages_data))
}

get_data_table <- function(html, company){

  # Job Title
  job_html = html_nodes(pg, '.jobLink')
  job = html_text(job_html) %>% 
    subset(. != ' no.logo.alt') %>% 
    subset(. != '') 
  job = job[1:30]

  # Location
  loc_html = html_nodes(pg, '.loc')
  loc = html_text(loc_html) 
  loc = loc[-1]
  loc

  # Summary
  sum_html = html_nodes(pg, '.jl')
  sum = html_text(sum_html) 
  sum = sum[1:30]

  # Combine into a tibble
  df <- tibble(Company = company, Position = job, Location = loc, Summary = sum)
}

get_data_from_url <- function(url, company){
  html <- read_html(url)
  get_data_table(html, company)
}

scrape_write_table <- function(url, company){
  
  first_page <- read_html(url)
  latest_page_number <- get_last_page()
  url_base <- str_remove(url, ".htm")
  list_of_pages <- paste0(url_base, "_IP", grep("[0-999]", 1:latest_page_number, value = TRUE), ".htm")

  # Apply the extraction and bind the individual results back into one table, 
  # which is then written as a tsv file into the working directory
  list_of_pages %>% 
    # Apply to all URLs
    map_dfr(get_data_from_url, company) 
    # Write a tab-separated file
    write_tsv(str_c(company,'.tsv'))  
}

scrape_write_table(url, company)

tbl <- read_tsv('uber.tsv')
View(tbl)


