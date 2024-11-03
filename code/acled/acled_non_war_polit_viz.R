# ###
# 
# monthly pro-gov over time, everything else the other line 
# 
# vertical lines elections and war and mobilization 
##### set up #####
pacman::p_load(tidyverse, rio, ggplot2, lubridate)


ACLED_DATA <- "C:/Users/murrn/GitHub/nonviolent-repression/data/acled_processed_data/acled_with_dvs_and_controls_16_07_2024_utf8byte.csv"

# acled_more <- read_xlsx(ACLED_DATA)
acled <- read_csv(ACLED_DATA)

acled_subset_for_plot <- acled %>% 
  # filter(date >= '2021-01-01') %>%
  mutate(month = ceiling_date(date, unit = "month"),
         pro_gov = case_when(org_indicator == "pro_kremlin" ~ "Pro Government",
                             org_indicator == "syst_opposition" ~ "Systemic Opposition",
                             TRUE ~ "Other"),
         protest_type = case_when(
                                  pred_labels == "cultural" | pred_labels == "legal" ~ "cultural/legal",
                                  TRUE ~ pred_labels),
         political_bin = case_when(
           protest_type=="political" | protest_type=="war_anti" | protest_type=="war_pro" ~ "political",
           T ~ "Other"
         )) 

# acled_subset_for_plot %>% group_by(protest_type) %>% summarise(n())
# acled_subset_for_plot %>% group_by(pred_labels) %>% summarise(n())
# pred_labels == "war_anti" ~ "political",

# Count unique observations of 'notes' for each month
unique_counts_type <- acled_subset_for_plot %>%
  # filter(protest_type != "war_pro") %>% 
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.5)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "black") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = 160, label = "Invasion", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "black") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = 160, label = "Mobilisation", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2021-01-17"), linetype = "solid", color = "black") +
  annotate(geom = "text", x = as.Date("2021-01-17"), y = 160, label = "Navalny's retun to Moscow", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3)

monthly_n_plot_type


##### What is war anti protest before 2022

event <- acled %>%
  filter(date <= '2021-01-01') %>% 
  filter(pred_labels == "war_anti") %>% select(notes) 

# "On Monday, November 26, 2018, around a dozen people took part in a anti-war protest 
# in front the Ukrainian consulate in central Saint Peterburg following a naval clash 
# near Kerch the day before. Even though police was present during the event, no detentions were reported."

##### share of non-war political protest after invasion ####

filtered_data <- acled %>%
  filter(date >= '2022-02-24') %>% 
  filter(pred_labels %in% c("political", "war_anti", "war_pro"))

monthly_share <- filtered_data %>%
  mutate(month = floor_date(as.Date(date), "month")) %>%  
  group_by(month, pred_labels) %>%
  summarise(count = n()) %>%
  spread(pred_labels, count, fill = 0) %>%  
  mutate(total = political + war_anti + war_pro,
         political_share = political / total) %>%
  select(month, political_share)

overall_share <- filtered_data %>%
  group_by(pred_labels) %>%
  summarise(count = n()) %>%
  spread(pred_labels, count, fill = 0) %>%
  mutate(total = political + war_anti + war_pro,
         political_share = political / total) %>%
  select(political_share) %>%
  pull()


## plot

# Create the plot
monthly_n_plot_type <- ggplot(monthly_share, aes(x = month, y = political_share)) +
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
  labs(title = "Monthly Share of Political Protest Unrelated to War",
       x = "",
       y = "Share") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.5)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "black") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = .60, label = "Invasion", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "black") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = .60, label = "Mobilisation", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3) 

monthly_n_plot_type
ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/acled/share_political_non_war.png", plot = monthly_n_plot_type, width = 10, height = 6, dpi = 300)


#### Share of war protest of all protest types #### 

monthly_share <- acled %>%
  filter(date >= '2022-02-24') %>%
  mutate(month = floor_date(as.Date(date), "month")) %>%
  group_by(month, pred_labels) %>%
  summarise(count = n(), .groups = 'drop') %>%
  spread(pred_labels, count, fill = 0) %>%
  mutate(total = rowSums(across(where(is.numeric))),
         political_share = ifelse(!is.na(political), political / total, 0),
         war_anti_share = ifelse(!is.na(war_anti), war_anti / total, 0),
         war_pro_share = ifelse(!is.na(war_pro), war_pro / total, 0)) %>%
  select(month, political_share, war_anti_share, war_pro_share)



monthly_share_plot_pol_war_all <- ggplot(monthly_share, aes(x = month)) +
  geom_line(aes(y = political_share, linetype  = "Political")) +
  geom_line(aes(y = war_anti_share, linetype  = "War-Anti")) +
  geom_line(aes(y = war_pro_share, linetype  = "War-Pro")) +
  geom_point(aes(y = political_share, shape  = "Political")) +
  geom_point(aes(y = war_anti_share, shape  = "War-Anti")) +
  geom_point(aes(y = war_pro_share, shape  = "War-Pro")) +
  scale_x_date(
    date_breaks = "1 month",
    labels = function(x) ifelse(month(x) == 1, format(x, "%Y-%b"), format(x, "%b")),
    expand = c(0, 0)
  ) +
  labs(title = "Monthly Share of Protest Types",
       x = "",
       y = "Share of Total Protests",
       color = "Protest Type") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.5)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "black") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = .60, label = "Invasion", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "black") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = .60, label = "Mobilisation", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3)

monthly_share_plot_pol_war_all
ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/acled/share_political_war_of_all_types.png", plot = monthly_share_plot_pol_war_all, width = 10, height = 6, dpi = 300)


#### Share of protests facing arrests ####

monthly_shares_violence <- acled_subset_for_plot %>%
  filter(pro_gov != "Pro Government") %>%
  group_by(month, political_bin) %>%
  summarise(total_events = n()) %>%
  left_join(
    acled_subset_for_plot %>%
      filter(pro_gov != "Pro Government", police_violence == "1") %>%
      group_by(month, political_bin) %>%
      summarise(violent_events = n()),
    by = c("month", "political_bin")
  ) %>%
  mutate(share_violent = violent_events / total_events * 100) %>%
  replace_na(list(share_violent = 0))

monthly_share_plot_polit_violence <- ggplot(monthly_shares_violence, aes(x = month, y = share_violent, linetype = political_bin, group = political_bin)) +
  geom_line() +  
  geom_point(size=0.5) +  
  scale_x_date(
    date_breaks = "1 year",
    labels = function(x) format(x, "%Y"),  
    expand = c(0, 0)
  ) +
  scale_linetype_manual(values = c("political" = "solid", "Other" = "dashed"))+
  labs(title = "Monthly Share of Protest Events Facing Police Violence",
       x = "",
       y = "Share of Protests",
       linetype = "Protest Type") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.5)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "darkgrey") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = 60, label = "Invasion", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "darkgrey") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = 60, label = "Mobilisation", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3)

monthly_share_plot_polit_violence
ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/acled/monthly_share_plot_polit_violence.png", plot = monthly_share_plot_polit_violence, width = 10, height = 6, dpi = 300)


#### protests facing repression counts ####

# monthly_counts_violence_status <- acled_subset_for_plot %>%
#   filter(pro_gov != "Pro Government") %>%
#   group_by(month, political_bin, police_violence) %>%
#   summarise(event_count = n(), .groups = 'drop') %>%
#   mutate(police_violence = case_when(police_violence==1~"Faced Violence",
#                                      police_violence==0~"No Violence"),
#          police_violence = as.factor(police_violence),)  # Convert police_violence to factor
# 
# # Plot monthly counts of protests, with facets for violence vs. non-violence and political vs. non-political
# monthly_count_plot <- ggplot(monthly_counts_violence_status, aes(x = month, y = event_count, color = police_violence)) +
#   geom_line() +  
#   geom_point(size=0.5) +  
#   scale_x_date(
#     date_breaks = "1 year",
#     labels = function(x) format(x, "%Y"),
#     expand = c(0, 0)
#   ) +
#   scale_color_manual(values = c("Faced Violence" = "red", "No Violence" = "blue")) +
#   labs(
#     title = "Monthly Counts of Political and Non-Political Protests Facing/Not Facing Police Violence",
#     x = "",
#     y = "Event Count",
#     linetype = "Protest Type",
#     color = "Police Violence"
#   ) + 
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.5)) +
#   facet_wrap(~ political_bin + police_violence, ncol = 2, scales = "free_y") +  # Facet by political_bin and police_violence
#   geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "black") +
#   annotate(geom = "text", x = as.Date("2022-02-24"), y = max(monthly_counts_violence_status$event_count) * 0.5, label = "Invasion", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3) +
#   geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "black") +
#   annotate(geom = "text", x = as.Date("2022-09-21"), y = max(monthly_counts_violence_status$event_count) * 0.5, label = "Mobilisation", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3)

monthly_counts_violence_status <- acled_subset_for_plot %>%
  filter(pro_gov != "Pro Government") %>%
  group_by(month, police_violence) %>%
  summarise(event_count = n(), .groups = 'drop') %>%
  mutate(police_violence = case_when(police_violence==1~"Faced Violence",
                                     police_violence==0~"No Violence"),
         police_violence = as.factor(police_violence),)  # Convert police_violence to factor

# Plot monthly counts of protests, with facets for violence vs. non-violence and political vs. non-political
monthly_count_plot <- ggplot(monthly_counts_violence_status, aes(x = month, y = event_count, linetype = police_violence)) +
  geom_line() +  
  geom_point(size=0.15) +  
  scale_x_date(
    date_breaks = "1 year",
    labels = function(x) format(x, "%Y"),
    expand = c(0, 0)
  ) +
  scale_linetype_manual(values = c("Faced Violence" = "solid", "No Violence" = "dashed")) +
  labs(
    title = "",
    x = "",
    y = "",
    linetype = "",
    color = ""
  ) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.5)) +
  #facet_wrap(~ political_bin + police_violence, ncol = 2, scales = "free_y") +  # Facet by political_bin and police_violence
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "darkgrey") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = max(monthly_counts_violence_status$event_count) * 0.65, label = "Invasion", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "darkgrey") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = max(monthly_counts_violence_status$event_count) * 0.65, label = "Mobilisation", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3)


monthly_count_plot
ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/acled/monthly_count_plot.png", plot = monthly_count_plot, width = 10, height = 6, dpi = 300)


monthly_share_plot_violence_protests <- ggplot(monthly_shares_violence, aes(x = month)) +
  geom_line(aes(y = political_share, linetype  = "Political")) +
  geom_line(aes(y = war_anti_share, linetype  = "War-Anti")) +
  scale_x_date(
    date_breaks = "1 month",
    labels = function(x) ifelse(month(x) == 1, format(x, "%Y-%b"), format(x, "%b")),
    expand = c(0, 0)
  ) +
  labs(title = "Monthly Share of Protest Types",
       x = "",
       y = "Share of Total Protests",
       color = "Protest Type") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.5)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "solid", color = "black") +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = .60, label = "Invasion", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3) +
  geom_vline(xintercept = as.Date("2022-09-21"), linetype = "solid", color = "black") +
  annotate(geom = "text", x = as.Date("2022-09-21"), y = .60, label = "Mobilisation", vjust = -0.5, hjust = 0.5, color = "black", angle = 90, size = 3)

monthly_share_plot_pol_war_all



##### Explore Political 

random_sample <- acled %>%
  filter(date >= '2022-02-24') %>% 
  filter(pred_labels %in% c("political")) %>% 
  sample_n(5)
  

print(random_sample$notes)


acled %>%
  filter(date >= '2022-02-24') %>% 
  group_by(pred_labels) %>% 
  summarise(n())

