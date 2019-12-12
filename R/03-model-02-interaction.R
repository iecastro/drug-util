library(interactions)

data("DrugUtil")

#--- interaction
#---- sample n=5000 obs
set.seed(5672)

model_data2 <- DrugUtil %>% 
  filter(state != "XX" &
           !is.na(medicaid_amount_reimbursed)) %>% 
  sample_n(5000) %>% 
  mutate(util = as.factor(utilization_type),
         units = jtools::center(units_reimbursed),
         medicaid_reimbursement = 
           jtools::center(medicaid_amount_reimbursed),
         cost_per_unit =
           medicaid_amount_reimbursed / units_reimbursed
  )


mod <- lm(medicaid_reimbursement ~ cost_per_unit + units + util,
          data = model_data2)

mod2 <- lm(medicaid_reimbursement ~ cost_per_unit + units*util,
           data = model_data2)

sjPlot::tab_model(mod, mod2,
                  show.aic = TRUE)

interactions::interact_plot(mod2, pred = units, 
                            modx = util,
                            interval = TRUE)
