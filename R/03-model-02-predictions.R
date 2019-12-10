library(breakDown)
library(tidyverse)

load("data/rf_mod.RData")


preds <- predict(rf_mod,
                 new_data = expense_test,
                 verbose = TRUE)



sub_expense_test <- expense_test %>% 
  sample_n(100)

preds_sub <- predict(rf_mod,
                     new_data = sub_expense_test,
                     verbose = TRUE)
  
  

save(preds_sub,
     file = "data/preds.RData")
