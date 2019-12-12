library(tidyverse)


#-- ingest atc classification codes and names
# source https://github.com/fabkury/ndc_map

ndc_atc <- read_csv("https://raw.githubusercontent.com/fabkury/ndc_map/master/Old%20version/2019-03-19/2019_03_19%2013_49%20year_month_ndc_atc4.csv")

atc4_names <- read_csv("https://raw.githubusercontent.com/fabkury/ndc_map/master/Old%20version/2019-03-19/2019_03_19%2013_49%20fda%20package%20atc4_name.csv")

#-- filter codes for 2018
codes_df <- ndc_atc %>% 
  filter(YEAR == 2018) %>% 
  separate(NDC,
           into = c("vendor", "product", "size"),
           sep = "-",
           remove = FALSE)

#-- convert 10-digit to 11-digit NDC
# 11-digit code uses 5-4-2 format and
# requires zero-padding byndc section 
ndc_11 <- codes_df %>% 
  mutate(vendor_5 = ifelse(str_count(.$vendor) == 4,
                           paste0("0",.$vendor),
                           vendor),
         product_4 = ifelse(str_count(.$product) == 3,
                            paste0("0",.$product),
                            product),
         size_2 = ifelse(str_count(.$size) == 1,
                         paste0("0", .$size),
                         size)) %>% 
  unite(NDC_11, vendor_5, product_4, size_2,
        sep = "")

range(str_count(ndc_11$NDC_11))
range(str_count(ndc_11$NDC)) # count 12 including dashes

#--- merge to ATC labels

atc4_names

#--- wrangle names and main groups
ndc_11 %>% 
  left_join(atc4_names,
            by = "ATC4") %>% 
  separate(ATC4, 
           into = c("ATC1", "rest"),
           sep = 1,
           remove = FALSE) %>% 
  mutate(ATC1_name = case_when(
    ATC1 == "A" ~ "Alimentary tract and metabolism",
    ATC1 == "B" ~ "Blood and blood forming organs",
    ATC1 == "C" ~ "Cardiovascular system",
    ATC1 == "D" ~ "Dermatologicals",
    ATC1 == "G" ~ "Genito-urinary system and sex hormones",
    ATC1 == "H" ~ "Systemic hormonal preparations, excluding sex hormones and insulins",
    ATC1 == "J" ~ "Antiinfectives for systemic use",
    ATC1 == "L" ~ "Antineoplastic and immunomodulating agents",
    ATC1 == "M" ~ "Musculo-skeletal system",
    ATC1 == "N" ~ "Nervous system",
    ATC1 == "P" ~ "Antiparasitic products, insecticides and repellents",
    ATC1 == "R" ~ "Respiratory system",
    ATC1 == "S" ~ "Sensory organs",
    ATC1 == "V" ~ "Various"
  ),
  MONTH = as.numeric(MONTH),
  quarter = case_when(
    MONTH %in% c(1:3) ~ 1,
    MONTH %in% c(4:6) ~ 2,
    MONTH %in% c(7:9) ~ 3,
    MONTH %in% c(10:12) ~ 4
  )) %>% 
  # select vars needed for merging
  select(NDC_11,
         YEAR,
         quarter,
         ATC4,
         ATC4_NAME,
         ATC1_name) %>% 
  # write to data file for future use
  write_csv("data-raw/drug-classification.csv")



