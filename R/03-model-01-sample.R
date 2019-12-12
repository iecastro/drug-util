library(tidyverse)
library(parsnip)
library(broom)


#data("DrugUtil")

#---- features
model_data <- DrugUtil %>% 
  filter(state != "XX" &
           !is.na(total_amount_reimbursed)) %>% 
  mutate(cost_per_unit =  # medicaid cost/unit
           medicaid_amount_reimbursed / units_reimbursed,
         price_rx = 
           total_amount_reimbursed / number_of_prescriptions,
         labeler_name = 
           ifelse(is.na(labeler_name),
                  labeler_code,
                  labeler_name),
         product_name = 
           ifelse(is.na(product_name),
                  product_code,
                  product_name),
         quarter = as.factor(quarter),
         state = as.factor(state),
         utilization_type = as.factor(utilization_type)
  ) %>% 
  mutate_if(is.numeric, jtools::center) %>% 
  # dummy code utilization
  mutate(FFS = ifelse(utilization_type == "FFSU", 1, 0), 
         MCO = ifelse(utilization_type == "MCOU", 1, 0)) %>% 
  mutate_at(vars(FFS, MCO),
            as.factor) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-labeler_code,
         -product_code,
         -year,
         -suppression_used,
         -quarter_begin,
         -quarter_begin_date, 
         -location,
         -latitude,
         -longitude,
         -total_amount_reimbursed,
         -non_medicaid_amount_reimbursed,
         #-number_of_prescriptions
         )

#---- sample n=5000 obs
set.seed(5672)

model_data <- model_data %>% 
  sample_n(15000)


#----- data split
set.seed(7564)

data_split <- 
  rsample::initial_split(model_data %>% 
                           select(-utilization_type), 
                         strata = "medicaid_amount_reimbursed", 
                         p = 0.75)

expense_train <- rsample::training(data_split)
expense_test  <- rsample::testing(data_split)


#---- model 
# random forest
rf_mod <- rand_forest(mode = "regression",
            mtry = 5,
            trees = 200) %>% 
  set_engine("ranger",
             seed = 5462,
             importance = "impurity",
             respect.unordered.factors = TRUE) %>% 
  fit(medicaid_amount_reimbursed ~ .,
      data = expense_train)

#---- predictions
results <- expense_test %>%
  select(medicaid_amount_reimbursed) %>%
  bind_cols(
    predict.model_fit(rf_mod,
                      new_data = expense_test)
  )

