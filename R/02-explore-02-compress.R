library(tidyverse)


data <- read_csv("data-raw/drug-util-2018.csv")

mfr <- read_csv("data-raw/labeler-codes.csv")

# clean col names: reformat to snake_case
data <- data %>% 
  janitor::clean_names()

#----- join labeler name
DrugUtil <- data %>% 
  mutate(labeler_code = 
           as.numeric(labeler_code)) %>% 
  left_join(mfr %>% 
              select(-products), 
            by = "labeler_code")

#--- compress and save data
save(DrugUtil,
     file = "data/DrugUtil.RData")

