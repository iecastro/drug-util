library(tidyverse) # loads dplyr and ggplot

data("DrugUtil")

#--- top mfr by state
state_mfr <- DrugUtil %>% 
  filter(!is.na(medicaid_amount_reimbursed)) %>% 
  group_by(state, labeler_code, labeler_name) %>% 
  summarise(total = sum(medicaid_amount_reimbursed)) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  slice(which.max(total)) %>% 
  ungroup()


label <- state_mfr %>% 
  filter(state != "XX") %>% 
  group_by(labeler_name) %>% 
  slice(which.max(total)) %>% 
  ungroup()

state_mfr %>% 
  arrange(desc(total)) %>% 
  filter(state != "XX") %>% # remove national total 
  ggplot(aes(fct_inorder(state), total)) + 
  geom_col(aes(fill = labeler_name),
           width = .75) +
  coord_flip() + # flip axis position
  scico::scale_fill_scico_d() +
  theme_minimal() +
  ggrepel::geom_text_repel(aes(state, total,
                               label = labeler_name),
                           data = label,
                           hjust = -.75,
                           nudge_x = 1.2,
                           nudge_y = .25,
                           direction = "y") +
  # scaling y axis because they're flipped
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"))


#-- highlight gilead
state_mfr %>% 
  arrange(desc(total)) %>% 
  filter(state != "XX") %>% # remove national total 
  mutate(gilead = 
           ifelse(labeler_name == "Gilead Sciences, Inc.",
                  "yes", "no")) %>% 
  ggplot(aes(fct_inorder(state), total)) + 
  geom_point(aes(color = gilead), size= 1.5) +
  geom_segment(aes(x = state, xend = state,
                   y = 0, yend = total,
                   color = gilead)) +
  coord_flip() + # flip axis position
  scale_color_manual(values = 
                      c("yes" = "#225B60",
                        "no" = "Grey90")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"))



glimpse(DrugUtil)

drug_label <- DrugUtil %>% 
  group_by(state) %>% 
  slice(which.max(medicaid_amount_reimbursed)) %>% 
  ungroup() %>% 
  select(state, product_name, 
         labeler_name, 
         medicaid_amount_reimbursed)


DrugUtil %>% 
  filter(state != "XX" &
           !is.na(medicaid_amount_reimbursed)) %>% 
  ggplot(aes(state, medicaid_amount_reimbursed)) +
  geom_boxplot() +
  scale_y_log10() +
  ggrepel::geom_text_repel(
    data = drug_label,
    aes(label = product_name),
    nudge_y      = 0.5,
    direction    = "x",
    angle        = 90,
    vjust        = -1,
    segment.size = 0.7
  )







