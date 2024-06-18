##### set up ####

pacman::p_load(tidyverse, rio, ggplot2, lubridate)

acled <- import("C:/Users/murrn/GitHub/nonviolent-repression/data/acled_processed_data/acled_with_dvs_and_controls_18_06_2024.csv") 


# # Convert date column to Date object and create month-year object 
# acled_clean <- acled_clean %>% 
#   mutate(date = ymd(event_date),
#          month_year = format(date, "%Y-%m"))

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

acled_subset_for_plot <- acled %>% 
  filter(date >= '2021-01-01')%>%
  filter(pro_kremlin_indicator != 1) %>%
  mutate(week = ceiling_date(date, unit = "week")) 

#### 1. Time-series line plot of the monthly number of unique protests in Russia ####

# Count unique observations of 'notes' for each month
unique_counts <- acled_subset_for_plot %>%
  filter(pro_kremlin_indicator != 1) %>%
  mutate(week = ceiling_date(date, unit = "week")) %>%
  group_by(week) %>%
  summarise(unique_notes = n_distinct(notes))

# Create the plot
weekly_n_plot <- ggplot(unique_counts, aes(x = week, y = unique_notes)) +
  geom_line() +
  geom_point() +
  scale_x_date(
    date_minor_breaks = "1 week",
    date_breaks = "1 month",
    labels = function(x) format(x, "%B")
  ) +
  labs(title = "Weekly Number of Unique Protests in Russia",
       x = "",
       y = "Number of Unique Protests") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")), linetype = "dashed", color = "blue") +  # Add blue vertical lines at the start of 2021, 2022, and 2023
  annotate(geom = "text", x = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")),
           y = 160, label = c("2021", "2022", "2023"), vjust = 1, hjust = 0,
           color = "blue", angle = 90, size = 3) + 
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "red") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = 160, label = "Invasion", vjust = 1, hjust = 0, color = "red", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "darkgreen") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = 160, label = "Mobilisation", vjust = 1, hjust = 0, color = "darkgreen", angle = 90, size = 3)


weekly_n_plot

ggsave("outputs/weekly_unique_protests_plot.png", plot = weekly_n_plot, width = 10, height = 6, dpi = 300)

# daily_counts <- acled_subset_for_plot %>%
#   filter(pro_kremlin_indicator != 1) %>%
#   group_by(date) %>%
#   summarise(unique_notes = n_distinct(notes))
# 
# # Create the plot
# daily_n_plot <- ggplot(daily_counts, aes(x = date, y = unique_notes)) +
#   geom_line() +
#   geom_point() +
#   scale_x_date(date_breaks = "1 month", labels = date_format("%B")) +
#   labs(title = "Daily Number of Unique Protests in Russia",
#        x = "",
#        y = "Number of Unique Protests") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   geom_vline(xintercept = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")), linetype = "dashed", color = "blue") +  # Add blue vertical lines at the start of 2021, 2022, and 2023
#   annotate(geom = "text", x = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")),
#            y = 160, label = c("2021", "2022", "2023"), vjust = 1, hjust = 0,
#            color = "blue", angle = 90, size = 3) + 
#   geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "red") 
# 
# daily_n_plot


#### 2. subsetting political and non-political protests; ####

unique_counts_polit <- acled_subset_for_plot %>%
  filter(pro_kremlin_indicator != 1) %>%
  mutate(polit_indicator = case_when(
    pred_labels == "political" ~ "political",
    TRUE ~ "other")) %>% 
  group_by(week, polit_indicator) %>%
  summarise(unique_notes = n_distinct(notes))


# Plotting
political_protests_plot <- ggplot(unique_counts_polit, aes(x = week, y = unique_notes,
                                                           linetype = polit_indicator)) +
  geom_line() +
  geom_point() +
  scale_x_date(
    date_minor_breaks = "1 week",
    date_breaks = "1 month",
    labels = function(x) format(x, "%B")
  ) +
  labs(title = "Weekly Number of Political Protests in Russia",
       x = "",
       y = "Number of Unique Protests") +
  theme_minimal() +
  scale_linetype_manual(values = c(political = "solid", other = "dashed")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")), linetype = "dashed", color = "blue") +  # Add blue vertical lines at the start of 2021, 2022, and 2023
  annotate(geom = "text", x = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")),
           y = 160, label = c("2021", "2022", "2023"), vjust = 1, hjust = 0,
           color = "blue", angle = 90, size = 3) + 
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "red") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = 160, label = "Invasion", vjust = 1, hjust = 0, color = "red", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "darkgreen") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = 160, label = "Mobilisation", vjust = 1, hjust = 0, color = "darkgreen", angle = 90, size = 3)


political_protests_plot


ggsave("outputs/political_protests_plot.png", plot = political_protests_plot, width = 10, height = 6, dpi = 300)

#### 2.5 Protests by Type ####

unique_counts_by_type <- acled_subset_for_plot %>%
  filter(pro_kremlin_indicator != 1) %>%
  group_by(week, pred_labels) %>%
  summarise(unique_notes = n_distinct(notes))


# Plotting
protests_by_type_plot <- ggplot(unique_counts_by_type, aes(x = week, y = unique_notes,
                                                         color = pred_labels)) +
  geom_line() +
  geom_point() +
  scale_x_date(
    date_minor_breaks = "1 week",
    date_breaks = "1 month",
    labels = function(x) format(x, "%B")
  ) +
  labs(title = "Weekly Number of Protests by Type",
       x = "",
       y = "Number of Unique Protests") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")), linetype = "dashed", color = "blue") +  # Add blue vertical lines at the start of 2021, 2022, and 2023
  annotate(geom = "text", x = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")),
           y = 160, label = c("2021", "2022", "2023"), vjust = 1, hjust = 0,
           color = "blue", angle = 90, size = 3) + 
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "red") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = 160, label = "Invasion", vjust = 1, hjust = 0, color = "red", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "darkgreen") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = 160, label = "Mobilisation", vjust = 1, hjust = 0, color = "darkgreen", angle = 90, size = 3)

protests_by_type_plot

ggsave("outputs/protests_by_type_plot.png", plot = protests_by_type_plot, width = 10, height = 6, dpi = 300)



#### 3. Time-series line plot of the monthly share of unauthorised ####

# Prepare the data
unique_counts_share <- acled_subset_for_plot %>%
  mutate(authorized_status = case_when(
    authorized >= 1 & unauthorized == 0 ~ "authorized" ,
    unauthorized >= 1 & authorized == 0 ~ "unauthorized",
    TRUE ~ "na"
  )) %>%
  group_by(week, authorized_status) %>%
  summarise(count = n_distinct(notes)) %>%
  spread(authorized_status, count, fill = 0) %>% # Spread data for calculation
  mutate(share_unauthorized = unauthorized / (unauthorized + authorized + na)) %>%
  select(week, share_unauthorized) # Select only necessary columns

# Plot the data
unauthorised_protests_plot <- ggplot(unique_counts_share, aes(x = week, y = share_unauthorized
                                                                )) +
  geom_line() +
  geom_point() +
  scale_x_date(
    date_minor_breaks = "1 week",
    date_breaks = "1 month",
    labels = function(x) format(x, "%B")
  ) +
  labs(title = "Weekly Share of Unauthorised Protests in Russia",
       x = "",
       y = "Share of Unauthorised Protests") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")), linetype = "dashed", color = "blue") +  # Add blue vertical lines at the start of 2021, 2022, and 2023
  annotate(geom = "text", x = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")),
           y = 0.7, label = c("2021", "2022", "2023"), vjust = 1, hjust = 0,
           color = "blue", angle = 90, size = 3) + 
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "red") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = 0.7, label = "Invasion", vjust = 1, hjust = 0, color = "red", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "darkgreen") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = 0.7, label = "Mobilisation", vjust = 1, hjust = 0, color = "darkgreen", angle = 90, size = 3)

unauthorised_protests_plot

ggsave("outputs/unauthorised_protests_plot.png", plot = unauthorised_protests_plot, width = 10, height = 6, dpi = 300)


#### 4 share of unauthorized political and unauthorized non-political ####

# Prepare the data
unique_counts_share <- acled_subset_for_plot %>%
  mutate(polit_indicator = case_when(pred_labels == "political" ~ "political",
    TRUE ~ "other"),
    authorized_status = case_when(
      authorized == 1 ~ "authorized",
      unauthorized ==1 & authorized == 0 ~ "unauthorized",
      TRUE ~ "na"
    )) %>%
  group_by(week, polit_indicator, authorized_status) %>%
  summarise(count = n_distinct(notes)) %>%
  spread(authorized_status, count, fill = 0) %>% # Spread data for calculation
  mutate(share_unauthorized = unauthorized / (unauthorized + authorized + na)) %>%
  select(week, share_unauthorized) # Select only necessary columns


# Plot the data
unauthorised_political_protests_plot <- ggplot(unique_counts_share,
                                               aes(x = week,
                                                   y = share_unauthorized,
                                                   linetype = polit_indicator)
                                               ) +
  geom_line() +
  geom_point() +
  scale_x_date(
    date_minor_breaks = "1 week",
    date_breaks = "1 month",
    labels = function(x) format(x, "%B")
  ) +
  labs(title = "Weekly Share of Unauthorised Political Protests in Russia",
       x = "",
       y = "Share of Unauthorised Protests") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_linetype_manual(values = c(political = "solid", other = "dashed")) + 
  geom_vline(xintercept = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")), linetype = "dashed", color = "blue") +  # Add blue vertical lines at the start of 2021, 2022, and 2023
  annotate(geom = "text", x = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")),
           y = 0.7, label = c("2021", "2022", "2023"), vjust = 1, hjust = 0,
           color = "blue", angle = 90, size = 3) + 
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "red") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = 0.7, label = "Invasion", vjust = 1, hjust = 0, color = "red", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "darkgreen") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = 0.7, label = "Mobilisation", vjust = 1, hjust = 0, color = "darkgreen", angle = 90, size = 3)

unauthorised_political_protests_plot

ggsave("outputs/unauthorised_political_protests_plot.png", plot = unauthorised_political_protests_plot, width = 10, height = 6, dpi = 300)

#### 5.1 Share of protests by type facing Arrests #### 

# Prepare the data
unique_counts_arrests_share <- acled_subset_for_plot %>%
  mutate(polit_indicator = case_when(pred_labels == "political" ~ "political",
                                     TRUE ~ "other"),
         arrests = ifelse(sub_event_type %in% c("Protest with intervention", 
                                                "Excessive force against protesters"),
                          "arrests", "no_arrests"),
         authorized_status = case_when(
           authorized == 1 ~ "authorized",
           unauthorized ==1 & authorized == 0 ~ "unauthorized",
           TRUE ~ NA
         )) %>%
  group_by(week, arrests) %>%
  summarise(count = n_distinct(notes)) %>%
  spread(arrests, count, fill = 0) %>% 
  mutate(share_arrests = arrests / (arrests + no_arrests)) %>%
  select(week, share_arrests) 


# Plot the data
arrests_protests_plot <- ggplot(unique_counts_arrests_share,
                                aes(x = week,
                                    y = share_arrests)
) +
  geom_line() +
  geom_point() +
  scale_x_date(
    date_minor_breaks = "1 week",
    date_breaks = "1 month",
    labels = function(x) format(x, "%B")
  ) +
  labs(title = "Weekly Share of Protests with Arrests in Russia",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_linetype_manual(values = c(political = "solid", other = "dashed")) + 
  geom_vline(xintercept = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")), linetype = "dashed", color = "blue") +  # Add blue vertical lines at the start of 2021, 2022, and 2023
  annotate(geom = "text", x = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")),
           y = 0.7, label = c("2021", "2022", "2023"), vjust = 1, hjust = 0,
           color = "blue", angle = 90, size = 3) + 
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "red") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = 0.7, label = "Invasion", vjust = 1, hjust = 0, color = "red", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "darkgreen") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = 0.7, label = "Mobilisation", vjust = 1, hjust = 0, color = "darkgreen", angle = 90, size = 3)

arrests_protests_plot

ggsave("outputs/share_arrests_plot.png", plot = arrests_protests_plot, width = 10, height = 6, dpi = 300)


#### 5.2. Share of protests by type facing Arrests #### 

# Prepare the data
unique_counts_arrests_share <- acled_subset_for_plot %>%
  mutate(polit_indicator = case_when(pred_labels == "political" ~ "political",
                                     TRUE ~ "other"),
         arrests = ifelse(sub_event_type %in% c("Protest with intervention", 
                                                       "Excessive force against protesters"),
                                 "arrests", "no_arrests"),
         authorized_status = case_when(
           authorized == 1 ~ "authorized",
           unauthorized ==1 & authorized == 0 ~ "unauthorized",
           TRUE ~ NA
         )) %>%
  group_by(week, polit_indicator, arrests) %>%
  summarise(count = n_distinct(notes)) %>%
  spread(arrests, count, fill = 0) %>% # Spread data for calculation
  mutate(share_arrests = arrests / (arrests + no_arrests)) %>%
  select(week, share_arrests) # Select only necessary columns


# Plot the data
arrests_type_protests_plot <- ggplot(unique_counts_arrests_share,
                                     aes(x = week,
                                         y = share_arrests,
                                         linetype = polit_indicator)
) +
  geom_line() +
  geom_point() +
  scale_x_date(
    date_minor_breaks = "1 week",
    date_breaks = "1 month",
    labels = function(x) format(x, "%B")
  ) +
  labs(title = "Weekly Share of Political Protests with Arrests in Russia",
       x = "",
       y = "") +
  theme_minimal() +
  scale_linetype_manual(values = c(political = "solid", other = "dashed")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_linetype_manual(values = c(political = "solid", other = "dashed")) + 
  geom_vline(xintercept = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")), linetype = "dashed", color = "blue") +  # Add blue vertical lines at the start of 2021, 2022, and 2023
  annotate(geom = "text", x = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01")),
           y = 0.7, label = c("2021", "2022", "2023"), vjust = 1, hjust = 0,
           color = "blue", angle = 90, size = 3) + 
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "red") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = 0.7, label = "Invasion", vjust = 1, hjust = 0, color = "red", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "darkgreen") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = 0.7, label = "Mobilisation", vjust = 1, hjust = 0, color = "darkgreen", angle = 90, size = 3)

arrests_type_protests_plot

ggsave("outputs/share_arrests_by_type_plot.png", plot = arrests_type_protests_plot, width = 10, height = 6, dpi = 300)



#### checking events #####

# Filter data for the last three months
# end_date <- Sys.Date()
# start_date <- end_date %m-% months(6)

# 
# end_date <- as.Date("2020-06-01")
# start_date <- as.Date("2020-03-01")
# 
# filtered_df <- acled %>% 
#   filter(unauthorized != 0) %>% 
#   mutate(month_year = as.Date(paste0(year(date), "-", month(date), "-01"))) %>%
#   filter(month_year >= start_date & month_year <= end_date)
# 
# set.seed(123) 
# sampled_events <- filtered_df %>% 
#   sample_n(size = 10)
# 
# sampled_events[c("notes", "unauthorized")]

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