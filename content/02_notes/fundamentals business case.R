library(readxl)
library(tidyverse)
library(broom)
library(umap)
library(ggrepel)

stores_tbl <- read_excel(path = "breakfast_at_the_frat.xlsx",
                         sheet = "dh Store Lookup",
                         skip = 1)

products_tbl <- read_excel(path = "breakfast_at_the_frat.xlsx",
                         sheet = "dh Products Lookup",
                         skip = 1)

transaction_tbl <- read_excel(path  = "breakfast_at_the_frat.xlsx",
                              sheet = "dh Transaction Data", 
                              skip  = 1)

orderlines_tbl <- transaction_tbl %>% 
  left_join(products_tbl) %>% 
  left_join(stores_tbl, by = c("STORE_NUM" = "STORE_ID"))

#get customer trends
customer_trends_tbl <- orderlines_tbl %>%
  mutate(BRANDED = ifelse(MANUFACTURER == "PRIVATE LABEL", "no", "yes")) %>%
  select(STORE_NAME, PRICE, UPC, DESCRIPTION, CATEGORY, SUB_CATEGORY, BRANDED, UNITS) %>%
  group_by(STORE_NAME, PRICE, UPC, DESCRIPTION, CATEGORY, SUB_CATEGORY, BRANDED) %>%
  summarise(QUANTITY_PURCHASED = sum(UNITS)) %>%
  ungroup() %>%
  group_by(STORE_NAME) %>%
  mutate(PROP_OF_TOTAL = QUANTITY_PURCHASED / sum(QUANTITY_PURCHASED)) %>%
  ungroup()

customer_product_tbl <- customer_trends_tbl %>% 
  select(STORE_NAME, UPC, PROP_OF_TOTAL) %>%
  pivot_wider(names_from = UPC, values_from = PROP_OF_TOTAL, values_fill = 0) %>%
  ungroup()

#k-means
?kmeans

kmeans_obj <- customer_product_tbl %>% 
  select(-STORE_NAME) %>%
  kmeans(centers = 3, nstart = 100)

kmeans_obj$cluster


broom::tidy(kmeans_obj) %>% glimpse()
broom::glance(kmeans_obj)
broom::augment(kmeans_obj, customer_product_tbl) %>%
  select(STORE_NAME, .cluster)

kmeans_mapper <- function(centers = 3) {
  
  customer_product_tbl %>%
    select(-STORE_NAME) %>%
    kmeans(centers = centers, nstart = 100)
}

3 %>% kmeans_mapper() %>% glance()

kmeans_mapped_tbl <- tibble(centers = 1:15) %>%
  mutate(k_means = centers %>% map(kmeans_mapper)) %>%
  mutate(glance  = k_means %>% map(glance))

kmeans_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss)

kmeans_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  
  # Visualization
  ggplot(aes(centers, tot.withinss)) +
  geom_point(color = "#2DC6D6", size = 4) +
  geom_line(color = "#2DC6D6", size = 1) +
  # Add labels (which are repelled a little)
  ggrepel::geom_label_repel(aes(label = centers), color = "#2DC6D6") + 
  
  # Formatting
  labs(title = "Skree Plot",
       subtitle = "Measures the distance each of the customer are from the closes K-Means center",
       caption = "Conclusion: Based on the Scree Plot, we select 3 clusters to segment the customer base.")