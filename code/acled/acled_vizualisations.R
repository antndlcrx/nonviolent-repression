##### set up ####

pacman::p_load(tidyverse, rio, ggplot2, lubridate)

acled <- import("data/acled_processed_data/acled_deberta_preds_05_02_2024.csv") 


## unique observations 

length(unique(acled$notes))
# Finding duplicate observations in 'variable'
duplicates <- duplicated(acled$notes)

# Getting the row IDs of these duplicates
row_ids <- which(duplicates)

# Print events
acled$notes[row_ids]
acled_clean <- acled[!duplicated(acled$notes), ]

# Convert date column to Date object and create month-year object 
acled <- acled %>% 
  mutate(date = dmy(event_date),
         month_year = format(date, "%Y-%m"))

# # create a subset from Jul 2021 to end Dec 2022
# acled_subset <- acled %>% 
#   # select(c(notes, date, protest_type, proportion,
#   #          pro_kremlin_indicator, event_id_cnty)) %>% 
#   filter(date >= as.Date("2021-07-01") & date <= as.Date("2022-12-31"))
# 
# # Save as CSV
# write.csv(acled_subset, "data/processed_data/acled_preprocessed_jul21_dec22.csv", row.names = FALSE)

# Save as Excel
# write.xlsx(acled_subset, "acled_preprocessed_jul21_dec22.xlsx", rowNames = FALSE)


#### 1. Time-series line plot of the monthly number of unique protests in Russia ####

# Count unique observations of 'notes' for each month
unique_counts <- acled_clean %>%
  filter(pro_kremlin_indicator != 1) %>% # remove pro-krml
  group_by(month_year) %>%
  summarise(unique_notes = n_distinct(notes))%>% # counting unique events (notes)
  mutate(month_year = ym(month_year))

monthly_n_plot <- ggplot(unique_counts, aes(x = month_year, y = unique_notes)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", minor_breaks = "1 month") +
  labs(title = "Monthly Number of Unique Protests in Russia",
       x = "",
       y = "Number of Unique Protests") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dashed", color = "red")

monthly_n_plot

ggsave("outputs/daily_unique_protests_plot.png", plot = monthly_n_plot, width = 10, height = 6, dpi = 300)


#### 2. subsetting political and non-political protests; ####

unique_counts_polit <- acled_clean %>%
  filter(pro_kremlin_indicator != 1) %>%
  mutate(polit_indicator = case_when(
    pred_labels == "political" ~ "political",
    TRUE ~ "other"),
    month_year = as.Date(paste0(month_year, "-01"))) %>%
  group_by(month_year, polit_indicator) %>%
  summarise(unique_notes = n_distinct(notes))


# Plotting
political_protests_plot <- ggplot(unique_counts_polit, aes(x = month_year, y = unique_notes, linetype = polit_indicator)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", minor_breaks = "1 month") +
  labs(title = "Monthly Number of Unique Protests in Russia",
       x = "",
       y = "Number of Unique Protests",
       linetype = "") +  # Set legend title to no title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dashed", color = "red") +
  scale_linetype_manual(values = c("political" = "solid", "other" = "dotdash")) 

ggsave("outputs/political_protests_plot.png", plot = political_protests_plot, width = 10, height = 6, dpi = 300)

#### 2.5 Protests by Type ####

unique_counts_by_type <- acled_clean %>%
  filter(pro_kremlin_indicator != 1) %>%
  mutate(
    month_year = as.Date(paste0(month_year, "-01"))) %>%
  group_by(month_year, pred_labels) %>%
  summarise(unique_notes = n_distinct(notes))


# Plotting
protests_by_type_plot <- ggplot(unique_counts_by_type, aes(x = month_year, y = unique_notes, color = pred_labels)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", minor_breaks = "1 month") +
  labs(title = "Monthly Number of Unique Protests in Russia by Protest Type",
       x = "",
       y = "Number of Unique Protests",
       linetype = "",
       color = "Protest Type") +  # Set legend title to no title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dashed", color = "red") 
  # scale_linetype_manual(values = c("political" = "solid", "other" = "dotdash")) 

ggsave("outputs/protests_by_type_plot.png", plot = political_protests_plot, width = 10, height = 6, dpi = 300)



#### 3. Time-series line plot of the monthly share of unauthorised ####

# Prepare the data
unique_counts_share <- acled_clean %>%
  filter(pro_kremlin_indicator != 1) %>%
  mutate(authorized_status = case_when(
    authorized >= 1 & unauthorized == 0 ~ "authorized" ,
    unauthorized >= 1 & authorized == 0 ~ "unauthorized",
    TRUE ~ "na"
  ),
         month_year = as.Date(paste0(month_year, "-01"))) %>%
  group_by(month_year, authorized_status) %>%
  summarise(count = n_distinct(notes)) %>%
  spread(authorized_status, count, fill = 0) %>% # Spread data for calculation
  mutate(share_unauthorized = unauthorized / (unauthorized + authorized + na)) %>%
  select(month_year, share_unauthorized) # Select only necessary columns

# Plot the data
unauthorised_protests_plot <- ggplot(unique_counts_share, aes(x = month_year, y = share_unauthorized)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", minor_breaks = "1 month") +
  labs(title = "Monthly Share of Unauthorized Protests in Russia",
       x = "",
       y = "Share of Unauthorized Protests") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dashed", color = "red")

ggsave("outputs/unauthorised_protests_plot.png", plot = unauthorised_protests_plot, width = 10, height = 6, dpi = 300)


#### 4 share of unauthorized political and unauthorized non-political ####

# Prepare the data
unique_counts_share <- acled_clean %>%
  filter(pro_kremlin_indicator != 1) %>%
  mutate(polit_indicator = case_when(pred_labels == "political" ~ "political",
    TRUE ~ "other"),
    authorized_status = case_when(
      authorized == 1 ~ "authorized",
      unauthorized ==1 & authorized == 0 ~ "unauthorized",
      TRUE ~ "na"
    ),
    month_year = as.Date(paste0(month_year, "-01"))) %>%
  group_by(month_year, polit_indicator, authorized_status) %>%
  summarise(count = n_distinct(notes)) %>%
  spread(authorized_status, count, fill = 0) %>% # Spread data for calculation
  mutate(share_unauthorized = unauthorized / (unauthorized + authorized + na)) %>%
  select(month_year, share_unauthorized) # Select only necessary columns


# Plot the data
unauthorised_political_protests_plot <- ggplot(unique_counts_share, aes(x = month_year, y = share_unauthorized, color = polit_indicator)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", minor_breaks = "1 month") +
  labs(title = "Monthly Share of Unauthorized Protests in Russia by Protest Type",
       x = "",
       y = "Share of Unauthorized Protests",
       color = "Protest Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dashed", color = "red")

ggsave("outputs/unauthorised_political_protests_plot.png", plot = unauthorised_political_protests_plot, width = 10, height = 6, dpi = 300)

#### 5.1 Share of protests by type facing Arrests #### 

# Prepare the data
unique_counts_arrests_share <- acled_clean %>%
  filter(pro_kremlin_indicator != 1) %>%
  mutate(polit_indicator = case_when(pred_labels == "political" ~ "political",
                                     TRUE ~ "other"),
         arrests = ifelse(sub_event_type %in% c("Protest with intervention", 
                                                "Excessive force against protesters"),
                          "arrests", "no_arrests"),
         authorized_status = case_when(
           authorized == 1 ~ "authorized",
           unauthorized ==1 & authorized == 0 ~ "unauthorized",
           TRUE ~ NA
         ),
         month_year = as.Date(paste0(month_year, "-01"))) %>%
  group_by(month_year, arrests) %>%
  summarise(count = n_distinct(notes)) %>%
  spread(arrests, count, fill = 0) %>% # Spread data for calculation
  mutate(share_arrests = arrests / (arrests + no_arrests)) %>%
  select(month_year, share_arrests) # Select only necessary columns


# Plot the data
arrests_protests_plot <- ggplot(unique_counts_arrests_share, aes(x = month_year, y = share_arrests)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", minor_breaks = "1 month") +
  labs(title = "Monthly Share of Protests with Arrests in Russia",
       x = "",
       y = "",
       color = "Protest Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dashed", color = "red")

ggsave("outputs/share_arrests_plot.png", plot = arrests_protests_plot, width = 10, height = 6, dpi = 300)


#### 5.2. Share of protests by type facing Arrests #### 

# Prepare the data
unique_counts_arrests_share <- acled_clean %>%
  filter(pro_kremlin_indicator != 1) %>%
  mutate(polit_indicator = case_when(pred_labels == "political" ~ "political",
                                     TRUE ~ "other"),
         arrests = ifelse(sub_event_type %in% c("Protest with intervention", 
                                                       "Excessive force against protesters"),
                                 "arrests", "no_arrests"),
         authorized_status = case_when(
           authorized == 1 ~ "authorized",
           unauthorized ==1 & authorized == 0 ~ "unauthorized",
           TRUE ~ NA
         ),
         month_year = as.Date(paste0(month_year, "-01"))) %>%
  group_by(month_year, polit_indicator, arrests) %>%
  summarise(count = n_distinct(notes)) %>%
  spread(arrests, count, fill = 0) %>% # Spread data for calculation
  mutate(share_arrests = arrests / (arrests + no_arrests)) %>%
  select(month_year, share_arrests) # Select only necessary columns


# Plot the data
arrests_type_protests_plot <- ggplot(unique_counts_arrests_share, aes(x = month_year, y = share_arrests, color = polit_indicator)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", minor_breaks = "1 month") +
  labs(title = "Monthly Share of Protests with Arrests in Russia by Protest Type",
       x = "",
       y = "",
       color = "Protest Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dashed", color = "red")

ggsave("outputs/share_arrests_by_type_plot.png", plot = arrests_type_protests_plot, width = 10, height = 6, dpi = 300)



#### checking events #####

# Filter data for the last three months
# end_date <- Sys.Date()
# start_date <- end_date %m-% months(6)


end_date <- as.Date("2020-06-01")
start_date <- as.Date("2020-03-01")

filtered_df <- acled %>% 
  filter(unauthorized != 0) %>% 
  mutate(month_year = as.Date(paste0(year(date), "-", month(date), "-01"))) %>%
  filter(month_year >= start_date & month_year <= end_date)

set.seed(123) 
sampled_events <- filtered_df %>% 
  sample_n(size = 10)

sampled_events[c("notes", "unauthorized")]


#### session info ####

# R version 4.3.1 (2023-06-16 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 22621)
# 
# Matrix products: default
# 
# 
# locale:
# LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
# LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
# LC_TIME=English_United States.utf8    
# 
# time zone: Europe/London
# tzcode source: internal
# 
# attached base packages:
# stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# caret_6.0-94    lattice_0.21-8  readxl_1.4.3    keyATM_0.5.0    quanteda_3.3.1 
# rio_1.0.1       lubridate_1.9.3 forcats_1.0.0   stringr_1.5.0   dplyr_1.1.3    
# purrr_1.0.2     readr_2.1.4     tidyr_1.3.0     tibble_3.2.1    ggplot2_3.4.4  
# tidyverse_2.0.0
# 
# loaded via a namespace (and not attached):
# tidyselect_1.2.0     timeDate_4022.108    farver_2.1.1        
# R.utils_2.12.2       pacman_0.5.1         pROC_1.18.5         
# digest_0.6.33        rpart_4.1.19         timechange_0.2.0    
# lifecycle_1.0.3      survival_3.5-5       magrittr_2.0.3      
# compiler_4.3.1       rlang_1.1.1          tools_4.3.1         
# utf8_1.2.3           data.table_1.14.8    labeling_0.4.3      
# plyr_1.8.9           withr_2.5.1          R.oo_1.25.0         
# nnet_7.3-19          grid_4.3.1           stats4_4.3.1        
# fansi_1.0.5          colorspace_2.1-0     future_1.33.0       
# globals_0.16.2       scales_1.2.1         iterators_1.0.14    
# MASS_7.3-60          cli_3.6.1            generics_0.1.3      
# RcppParallel_5.1.7   rstudioapi_0.15.0    future.apply_1.11.0 
# reshape2_1.4.4       tzdb_0.4.0           splines_4.3.1       
# parallel_4.3.1       cellranger_1.1.0     vctrs_0.6.3         
# hardhat_1.3.0        Matrix_1.6-2         hms_1.1.3           
# listenv_0.9.0        foreach_1.5.2        gower_1.0.1         
# recipes_1.0.8        glue_1.6.2           parallelly_1.36.0   
# codetools_0.2-19     stringi_1.7.12       gtable_0.3.4        
# munsell_0.5.0        pillar_1.9.0         ipred_0.9-14        
# lava_1.7.3           R6_2.5.1             R.methodsS3_1.8.2   
# class_7.3-22         Rcpp_1.0.11          fastmatch_1.1-4     
# nlme_3.1-162         prodlim_2023.08.28   pkgconfig_2.0.3     
# ModelMetrics_1.2.2.2