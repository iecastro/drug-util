library(tidyverse)


data <- read_csv("data-raw/drug-util-2018.csv")

mfr <- read_csv("data-raw/labeler-codes.csv")

#----- check data 
glimpse(data)

data %>% filter(State == "XX") # aggregated NCDs
data %>% filter(`Suppression Used`) # ~2.5M obs suppressed

# clean col names
data <- data %>% 
  janitor::clean_names()

#----- summaries
data %>% 
  group_by(state) %>% 
  count() %>% 
  filter(state != "XX") %>% 
  arrange(desc(n))

# most common product name
data %>% 
  filter(!is.na(product_name)) %>% 
  group_by(product_name) %>% 
  count() %>% # ~14K unique product names
  arrange(desc(n)) # top 2 are blood pressure meds

# product names / product codes / ndc 

data %>% 
  #filter(!is.na(product_name)) %>% 
  group_by(labeler_code) %>% 
  count() %>% # ~774 unique labelers
  arrange(desc(n))

# drug with most units reimbursed
data %>% 
  filter(!is.na(units_reimbursed)) %>% 
  group_by(product_name) %>% 
  summarise(tot_units = 
              sum(units_reimbursed)) %>% 
  arrange(desc(tot_units))

# most amount reimbursed
data %>% 
  filter(!is.na(units_reimbursed)) %>% 
  group_by(product_name) %>% 
  summarise(tot_paid = 
              sum(total_amount_reimbursed),
            medicaid_paid = 
              sum(medicaid_amount_reimbursed)) %>% 
  arrange(desc(medicaid_paid))

#----- join labeler name
data <- data %>% 
  mutate(labeler_code = 
           as.numeric(labeler_code)) %>% 
  left_join(mfr %>% 
              select(-products), 
            by = "labeler_code")
#--- check data
data %>% 
  group_by(is.na(labeler_name)) %>% 
  count() # ~2K labeler codes did not match


data %>% 
  filter(is.na(labeler_name)) %>% 
  select(product_name, ndc, 
         labeler_name, labeler_code)

data %>% 
  filter(product_name == "DILTIAZEM") %>% 
  select(product_name, ndc, 
         labeler_name, labeler_code)

data %>% 
  filter(!is.na(labeler_name)) %>% 
  distinct(labeler_name) # n=634

data %>% 
  filter(!is.na(labeler_name)) %>% 
  distinct(labeler_name, labeler_code)  %>% # n=732
  get_dupes(labeler_name) %>% # 50 unique names have multiple code
  arrange(desc(dupe_count)) %>% 
  nest(labeler_code,
       .key = "codes")

#--- check geographic extent
data %>% 
  filter(!is.na(state)) %>% 
  group_by(state) %>% 
  count(location)

data %>% 
  filter(state == "NY") %>% 
  distinct(longitude)

#--- explore reimbursements
glimpse(data)

medicaid <- data %>% 
  filter(!state == "XX") %>% 
  group_by(state, utilization_type) %>% 
  summarise(units_tot =
              sum(units_reimbursed,
                  na.rm = TRUE),
            medicaid_tot = 
              sum(medicaid_amount_reimbursed,
                  na.rm = TRUE),
            cost_per_unit = 
              medicaid_tot / units_tot) %>% 
  ungroup()

ggplot(medicaid,
       aes(utilization_type,
           medicaid_tot)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar)

ggplot(medicaid,
       aes(state, medicaid_tot)) +
  geom_col(aes(fill = utilization_type),
           position = "dodge") + 
  coord_flip()
  
 
data %>% 
  filter(product_name == "LISINOPRIL" &
           state != "XX") %>% 
  group_by(state, labeler_name) %>% 
  filter(!is.na(units_reimbursed)) %>% 
  summarise(units_tot =
              sum(units_reimbursed),
            medicaid_tot = 
              sum(medicaid_amount_reimbursed),
            cost_per_unit = 
              medicaid_tot / units_tot)
  

humira <- data %>% 
  mutate(humira = 
           str_detect(product_name,
                      "HUMIRA")) %>% 
  filter(humira == TRUE,
         state != "XX") %>% 
  group_by(state, product_name) %>% 
  filter(!is.na(units_reimbursed)) %>% 
  summarise(units_tot =
              sum(units_reimbursed),
            medicaid_tot = 
              sum(medicaid_amount_reimbursed),
            cost_per_unit = 
              medicaid_tot / units_tot) %>% 
  ungroup()

humira %>% 
  group_by(product_name) %>% 
  count()

humira %>% 
  filter(product_name == "HUMIRA PEN") %>% 
  ggplot(aes(reorder(state, -cost_per_unit),
             cost_per_unit)) +
  geom_point() +
  geom_segment(aes(x = state, xend = state,
                   y = 0, yend = cost_per_unit)) +
  coord_flip() +
  labs(x = "Medicaid cost-per-unit of Humira Pen")


#--- top mfr by state
state_mfr <- data %>% 
  filter(!is.na(medicaid_amount_reimbursed)) %>% 
  group_by(state, labeler_code, labeler_name) %>% 
  summarise(total = sum(medicaid_amount_reimbursed)) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  slice(which.max(total)) %>% 
  ungroup()

#--- add drug classification 

drug_class <- read_csv("data-raw/drug-classification.csv")

drug_class %>% 
  select(ndc = NDC_11,
         drug_group = ATC1_name) %>% 
  right_join(DrugUtil %>% 
               group_by(quarter),
             by = "ndc")  # only 38% of ndcs matched
# may be a bug in the string convertion
# from 10 to 11 digits ndc





