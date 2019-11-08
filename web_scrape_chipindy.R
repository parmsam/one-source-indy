library(rvest)
library(tidyverse)
library(stringr)
library(rio)
#retreive all resource category pages
res_links <- read_html("http://www.handbookofhelp.org/clothing-pantries/") %>%  html_nodes("li") %>% str_extract('http://[\\w:[[:punct:]]]*/')
res_links <- res_links[4:19]

#build function to pull from each page
scrape_hfh <- function(link){
  scraping_hfh <- read_html(link)
  rs_raw <- scraping_hfh %>%
    html_nodes("h1") %>%
    html_text() 
  
  rs_category <- rs_raw[2]
  
  rs_body <- scraping_hfh %>%
    html_nodes("tbody") %>%
    html_text() 
  rs_names<- rs_body %>% str_extract("\n.*[\n\n|\n\n\n]") %>% str_remove("Phone:\n") %>% str_remove_all("\n") %>% str_remove("\\(.*\\)")
  rs_phones<- rs_body %>% str_extract("Phone:\n.*\n") %>% str_remove("Phone:\n") %>% str_remove("\n")
  rs_address<- rs_body %>% str_extract("Address:\n.*\n") %>% str_remove("Address:\n") %>% str_remove("\n")
  rs_hours <- rs_body %>% str_extract("Hours:\n.*\n") %>% str_remove("Hours:\n") %>% str_remove("\n")
  rs_required<- rs_body %>% str_extract("Required:\n.*\n") %>% str_remove("Required:\n") %>% str_remove("\n")
  rs_services<- rs_body %>% str_extract("Services:\n.*\n") %>% str_remove("Services:\n") %>% str_remove("\n")
  rs_fees<- rs_body %>% str_extract("Fees:\n.*\n") %>% str_remove("Fees:\n") %>% str_remove("\n")
  rs_general<- rs_body %>% str_extract("General:\n.*\n") %>% str_remove("General:\n") %>% str_remove("\n")
  rs_eligibility<- rs_body %>% str_extract("Eligibility:\n.*\n") %>% str_remove("Eligibility:\n") %>% str_remove("\n")
  rs_website<- rs_body %>% str_extract("(More info|Website):\n.*\n") %>% str_remove("(More info|Website):\n") %>% str_remove("\n")
  rs_email<- rs_body %>% str_extract("Email:\n.*\n") %>% str_remove("Email:\n") %>% str_remove("\n")
  
  rs_dataframe <- data.frame(rs_category, rs_names, rs_phones, rs_address, rs_hours, rs_required, 
                             rs_services, rs_fees, rs_general, rs_eligibility, rs_website, rs_email) 
  return(rs_dataframe)
}

scrape_hfh("http://www.handbookofhelp.org/neighborhood-community-centers/") %>% View()
full_dataset <- map(res_links, scrape_hfh)

for (i in 1:length(full_dataset)) {
  assign(paste0("hfh_", i), as.data.frame(full_dataset[[i]]))
}

single_df <- data.frame()
for (i in 1:length(full_dataset)){
  single_df <- single_df %>% rbind(full_dataset[[i]])
}
export(single_df,"D:/Programming/resource_data_chipindy.csv")
