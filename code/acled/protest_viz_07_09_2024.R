# ###
# 
# monthly pro-gov over time, everything else the other line 
# 
# vertical lines elections and war and mobilization 

pacman::p_load(tidyverse, rio, ggplot2, lubridate)


ACLED_DATA <- "C:/Users/murrn/GitHub/nonviolent-repression/data/acled_processed_data/acled_with_dvs_and_controls_09_07_2024.csv"

# acled_more <- read_xlsx(ACLED_DATA)
acled <- read_csv(ACLED_DATA)

# floor_date() takes a date-time object and rounds it down to the nearest boundary of the specified time unit.
# x <- ymd_hms("2009-08-03 12:01:59.23")
# floor_date(x, "month")
# #> [1] "2009-08-01 UTC"

# round_date() takes a date-time object and time unit, and rounds it to the nearest value of the specified time unit. For rounding date-times which are exactly halfway between two consecutive units, the convention is to round up. Note that this is in line with the behavior

acled_subset_for_plot <- acled %>% 
  filter(date >= '2021-01-01') %>%
  mutate(month = floor_date(date, unit = "month"),
         pro_gov = case_when(org_indicator == "pro_kremlin" ~ "Pro Government",
                             org_indicator == "syst_opposition" ~ "Systemic Opposition",
                             TRUE ~ "Other")) 

# Count unique observations of 'notes' for each month
unique_counts <- acled_subset_for_plot %>%
  group_by(month, pro_gov) %>%
  summarise(num_protest = n_distinct(notes))


# Create the plot
monthly_n_plot <- ggplot(unique_counts, aes(x = month, y = num_protest, linetype = pro_gov)) +
  geom_line() +
  geom_point() +
  scale_x_date(
    date_breaks = "1 month",
    labels = function(x) ifelse(month(x) == 1, format(x, "%Y-%b"), format(x, "%b")),
    expand = c(0, 0)
  ) +
  scale_linetype_manual(values = c("Other" = "solid", 
                                   "Systemic Opposition" = "dashed", 
                                   "Pro Government" = "dotted")) +
  labs(title = "Monthly Number of Unique Protests in Russia by Organiser",
       x = "",
       y = "Number of Unique Protests",
       linetype = "Protest Organiser") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "red") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = 160, label = "Invasion", vjust = -0.5, hjust = 0.5, color = "red", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "darkgreen") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = 160, label = "Mobilisation", vjust = -0.5, hjust = 0.5, color = "darkgreen", angle = 90, size = 3)


monthly_n_plot
ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/acled/protests_before_after_inv_by_organiser.png", plot = monthly_n_plot, width = 10, height = 6, dpi = 300)

##### protest by type #####

acled_subset_for_plot <- acled %>% 
  filter(date >= '2021-01-01') %>%
  mutate(month = floor_date(date, unit = "month"),
         pro_gov = case_when(org_indicator == "pro_kremlin" ~ "Pro Government",
                             org_indicator == "syst_opposition" ~ "Systemic Opposition",
                             TRUE ~ "Other"),
         protest_type = case_when(pred_labels == "war_anti" ~ "political",
                                  pred_labels == "cultural" | pred_labels == "legal" ~ "cultural/legal",
                                  TRUE ~ pred_labels)) 

# acled_subset_for_plot %>% group_by(protest_type) %>% summarise(n())
# acled_subset_for_plot %>% group_by(pred_labels) %>% summarise(n())

# Count unique observations of 'notes' for each month
unique_counts_type <- acled_subset_for_plot %>%
  filter(protest_type != "war_pro") %>% 
  group_by(month, protest_type) %>%
  summarise(num_protest = n_distinct(notes))


# Create the plot
monthly_n_plot_type <- ggplot(unique_counts_type, aes(x = month, y = num_protest, color = protest_type)) +
  geom_line() +
  geom_point() +
  scale_x_date(
    date_breaks = "1 month",
    labels = function(x) ifelse(month(x) == 1, format(x, "%Y-%b"), format(x, "%b")),
    expand = c(0, 0)
  ) +
  # scale_linetype_manual(values = c("economic" = "solid",
  #                                  "environmental" = "dashed",
  #                                  "cultural/legal" = "dotted",
  #                                  "political" = "F1",
  #                                  "social" = "dotdash")) +
  labs(title = "Monthly Number of Unique Protests in Russia by Organiser",
       x = "",
       y = "Number of Unique Protests",
       color = "Protest Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "red") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = 160, label = "Invasion", vjust = -0.5, hjust = 0.5, color = "red", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "darkgreen") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = 160, label = "Mobilisation", vjust = -0.5, hjust = 0.5, color = "darkgreen", angle = 90, size = 3)


monthly_n_plot_type
ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/acled/protests_before_after_inv_by_type.png", plot = monthly_n_plot_type, width = 10, height = 6, dpi = 300)
