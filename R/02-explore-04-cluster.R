library(tidyverse)
library(cluster)
library(factoextra)

#-- ingest
data("DrugUtil")


#-- wrangle
util_counts <- DrugUtil %>% 
  filter(state != "XX" &
           !is.na(total_amount_reimbursed)) %>% 
  mutate(utilization_type = as.factor(utilization_type)) %>% 
  group_by(state, utilization_type) %>% 
  count() %>% 
  spread(key = utilization_type,
         value = n,
         fill = 0) %>% 
  ungroup()


total_sums <- DrugUtil %>% 
  filter(state != "XX" &
           !is.na(total_amount_reimbursed)) %>% 
  group_by(state) %>% 
  summarise(medicaid_amount_reimbursed = 
              sum(medicaid_amount_reimbursed),
            number_of_prescriptions = 
              sum(number_of_prescriptions),
            units_reimbursed = 
              sum(units_reimbursed),
            total_amount_reimbursed = 
              sum(total_amount_reimbursed)) %>% 
  ungroup()


merged <- total_sums %>% 
  inner_join(util_counts,
             by = "state") %>% 
  column_to_rownames(var = "state") 

#---- check clustering 
# cluster tendency
get_clust_tendency(merged, n = 25)

# scale dataframe
clust_df <- scale(merged)

# distance matrix using
# correlation-based distance
distance <- get_dist(clust_df,
                     method =  "pearson") 

fviz_dist(distance, 
          gradient = list(low = "#B8AE87", 
                          mid = "white", 
                          high = "#4C0000"))

#---- cluster analysis 

set.seed(3456)

fviz_nbclust(clust_df, 
             kmeans, 
             method = "wss") # within-clust sum of squares

fviz_nbclust(clust_df, kmeans, 
             method = "silhouette")

kclust <- kmeans(clust_df, 
                 centers = 4, 
                 nstart = 25)

kclust

fviz_cluster(kclust, data = merged,
             main = "Clusters") +
  theme_minimal() +
  scico::scale_color_scico_d(palette = "roma") +
  scico::scale_fill_scico_d(palette = "roma")

merged %>% 
  mutate(cluster = kclust$cluster) %>%
  group_by(cluster) %>%
  summarise_all("mean")

#========  create state x type observations

combined_sums <- DrugUtil %>% 
  filter(state != "XX" &
           !is.na(total_amount_reimbursed)) %>% 
  unite(state_type,
        state, utilization_type) %>% 
  group_by(state_type) %>% 
  summarise(count = n(),
            medicaid_amount_reimbursed = 
              sum(medicaid_amount_reimbursed),
            number_of_prescriptions = 
              sum(number_of_prescriptions),
            units_reimbursed = 
              sum(units_reimbursed),
            total_amount_reimbursed = 
              sum(total_amount_reimbursed)) %>% 
  ungroup() %>% 
  column_to_rownames(var = "state_type")

clust_df2 <- scale(combined_sums)


#---- cluster analysis 

set.seed(3456)

fviz_nbclust(clust_df2, 
             kmeans, 
             method = "wss") # within-clust sum of squares

fviz_nbclust(clust_df2, kmeans, 
             method = "silhouette")

kclust2 <- kmeans(clust_df2, 
                 centers = 4, 
                 nstart = 25)

kclust2

fviz_cluster(kclust2, 
             data = combined_sums,
             main = "Clusters") +
  theme_minimal() +
  scico::scale_color_scico_d(palette = "roma") +
  scico::scale_fill_scico_d(palette = "roma")



