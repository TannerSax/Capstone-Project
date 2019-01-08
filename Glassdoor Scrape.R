# Packages
library(tidyverse) 
library(dplyr)
library(rvest)
library(stringr)
library(purrr)
library(stringi)

# Set as url_base to create data frame for desired city
New_York_url <- "https://glassdoor.com/Job/new-york-data-analyst-jobs-SRCH_IL.0,8_IC1132348_KO9,21"
Boston_url <- "https://www.glassdoor.com/Job/boston-data-analyst-jobs-SRCH_IL.0,6_IC1154532_KO7,19"
Washington_url <- "https://www.glassdoor.com/Job/washington-data-analyst-jobs-SRCH_IL.0,10_IC1138213_KO11,23"
Philadelphia_url <- "https://www.glassdoor.com/Job/philadelphia-data-analyst-jobs-SRCH_IL.0,12_IC1152672_KO13,25"

page_result_start <- 1 # starting page 
page_result_end <- 30  # last page results

full_df <- data.frame()
for(i in page_result_start:page_result_end) {
  
  url_base <- New_York_url
  url <- paste0(url_base, "_IP", i, ".htm")
  page <- xml2::read_html(url)
  
  # Sys.sleep pauses R for two seconds before it resumes
  # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
  Sys.sleep(2)
  
  # get the job title
  job_title <- page %>% 
    html_nodes(".jobTitle") %>% 
    html_text() %>% 
    subset(. != '') 

  # get the company name
  company_name <- page %>%
    html_nodes("span")  %>%
    html_nodes(xpath = '//*[@id="MainCol"]/div/ul/li/div/div/div/text()')  %>%
    html_text() %>% 
    subset(. != 'New') %>% 
    subset(. != 'Hot') %>% 
    subset(. != "We're Hiring") %>% 
    subset(. != "Top Company") %>%
    stri_trim_both()
  
  # remove hyphens at end of company name
  company_name <- gsub('[^[:alnum:][:blank:]?&/\\-]', '', company_name)
  
  # get job location
  job_location <- page %>% 
    html_nodes(".loc") %>% 
    html_text() %>% 
    subset(. != '')
  
  # get links
  links <- page %>%
    html_nodes("div") %>%
    html_nodes(xpath = '//*[@id="MainCol"]/div/ul/li/div[2]/div[1]/div[1]/a') %>%
    html_attr("href")
  
  # get job description
  job_description <- c()
  for(i in seq_along(links)) {
    
    url <- paste0("https://www.glassdoor.com", links[i])
    
    result <- try({  
      
    page <- xml2::read_html(url)
    job_description[[i]] <- page %>%
      html_nodes("div")  %>%
      html_nodes('.jobDesc') %>%
      html_text() %>% 
      stri_trim_both()
    } ,silent = TRUE)
    
if(!inherits(result, "try-error")) result
    }
   
    
  df <- data.frame(job_title, company_name, job_location, job_description)
  full_df <- rbind(full_df, df)
}


