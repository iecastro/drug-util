library(tidyverse)
library(parsnip)

data("DrugUtil")

glimpse(DrugUtil)

#---- wrangle 
model_data <- DrugUtil %>% 
  filter(state != "XX" &
           !is.na(total_amount_reimbursed)) %>% 
  mutate(cost_per_unit = 
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
         utilization_type = as.factor(utilization_type)) %>% 
  select(-labeler_code,
         -product_code,
         -year,
         -suppression_used,
         -quarter_begin,
         -quarter_begin_date, 
         -location,
         -latitude,
         -longitude)

glimpse(model_data)

#----- data split
set.seed(7564)

data_split <- 
  rsample::initial_split(model_data, 
                         strata = "medicaid_amount_reimbursed", 
                         p = 0.75)

expense_train <- rsample::training(data_split)
expense_test  <- rsample::testing(data_split)


#---- model 
# random forest

rf_mod <- rand_forest(mode = "regression",
            mtry = 5,
            trees = 150) %>% 
  set_engine("ranger",
             seed = 5462,
             importance = 'impurity') %>% 
  fit(medicaid_amount_reimbursed ~ .,
      data = expense_train)


rf_mod

#--- compress & save 
# traning data
save(expense_train,
     file = "data/expense_train.RData")

# test data
save(expense_test,
     file = "data/expense_test.RData")


# model object
save(rf_mod,
     file = "data/rf_mod.RData")



