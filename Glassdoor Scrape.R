library(tidyverse) 
library(dplyr)
library(rvest)    
library(stringr)


url = 'https://www.glassdoor.com/Job/rubrik-jobs-SRCH_KE0,6.htm'
comp_end = regexpr("[-]", url)
company = substr(url, 31, comp_end-1)
pg = read_html(url)
jl_html = html_nodes(pg, '.jl')
jl_text = html_text(jl_html)
jl_text


# FILTER PAGES
get_last_page <- function(html){
  
  pages_data <- pg %>%
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
list_of_page <- str_c(url_base, "_IP", grep("[0-9]", 1:latest_page_number, value = TRUE), ".htm")
list_of_pages


# Job Title
rm_num <- gsub("^ [0-9].[0-9]", "", jl_text)
comp_ind <- regexpr(company, rm_num, ignore.case = TRUE) 
position <- substr(rm_num, 1, comp_ind-2)
position

