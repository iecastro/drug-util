library(tidyverse)
library(healthdatacsv)
library(rvest)

#--- drug utilization dataset from healthdata.gov
catalog <- fetch_catalog("Centers for Medicare & Medicaid Services") %>% 
  mutate(drug_util = str_detect(product, 
                                "Drug Utilization")) %>% 
  filter(drug_util == TRUE)

product <- catalog %>% 
  filter(product == 
           "State Drug Utilization Data 2018") %>% 
  fetch_csv()

product %>% 
  unnest() %>% 
  select(-(1:5)) %>% 
  write_csv("data-raw/drug-util-2018.csv")

#--- web scrape labeler codes
table <- read_html("https://ndclist.com/labeler/index-view-all") %>% 
  html_node(".col-sm-12 , #labeler-table-index-view-all") %>% 
  html_table()

table %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  write_csv("data-raw/labeler-codes.csv")

