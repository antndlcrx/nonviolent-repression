pacman::p_load(tidyverse, rio, ggplot2, lubridate)

# setwd(".../nonviolent-repression")

# load data
kprf = read_csv('C:/Users/murrn/GitHub/placeholder_data/kprf_output.csv', 
                quote = "\'")

activatica = read_csv('C:/Users/murrn/GitHub/placeholder_data/activatica_output.csv', 
                      quote = "\'")

kavkaz = read_csv("C:/Users/murrn/GitHub/placeholder_data//kavkaz_output_complete.csv",
                  quote = "\'") 

kommersant = read_csv("C:/Users/murrn/GitHub/placeholder_data//kommersant_output_complete.csv",
                      quote = "\'")


#### harmonise dates and create month-year indicator #### 

## Helper function and mapping from Yana's code

months_map = c(
  января = "January",
  февраля = "February",
  марта = "March",
  апреля = "April",
  мая = "May",
  июня = "June",
  июля = "July",
  августа = "August",
  сентября = "September",
  октября = "October",
  ноября = "November",
  декабря = "December"
)

replace_months = function(date_string, months_map) {
  for (month_rus in names(months_map)) {
    date_string <- gsub(month_rus, months_map[month_rus], date_string, fixed = TRUE)
  }
  return(date_string)
}

# Harmonise data for each source
kprf <- kprf %>%
  mutate(date = sub(" \\(.*", "", date_published),
         date = ymd_hm(date),
         month_year = format(date, "%Y-%m"),
         source = "KPRF")

kommersant <- kommersant %>%
  mutate(date = sub(" \\(.*", "", date_published),
         date = dmy_hm(date),
         month_year = format(date, "%Y-%m"),
         source = "Kommersant")

activatica <- activatica %>%
  mutate(month_year = format(date_created, "%Y-%m"),
         source = "Activatica",
         content = text)

kavkaz <- kavkaz %>%
  mutate(date = replace_months(date_published, months_map),
         date = dmy_hm(date),
         month_year = format(date, "%Y-%m"),
         source = "Kavkaz")

# Combine all dataframes
combined_data <- bind_rows(kprf, kommersant, activatica, kavkaz)
# write_csv(combined_data, "data/combined_media.csv")


# Count unique stories for each month and source
unique_counts <- combined_data %>%
  group_by(month_year, source) %>%
  summarise(unique_stories = n_distinct(content)) %>%
  mutate(month_year = ym(month_year))

# Plotting
monthly_n_plot <- ggplot(unique_counts, aes(x = month_year, y = unique_stories)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", minor_breaks = "1 month") +
  labs(title = "Monthly Number of Stories by Source",
       x = "",
       y = "Number of Stories") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dashed", color = "red") +
  facet_wrap(~source, scales = "free", ncol = 2) # Separate plots for each source

# Save the plot with increased height
ggsave("outputs/monthly_stories_plot.png", monthly_n_plot, width = 10, height = 15, units = "in")



##### check for missing dates #### 

# Convert the date column to Date format if it's not already
combined_data$date <- as.Date(combined_data$date)

# Now calculate the start and end dates
start_date <- min(combined_data$date, na.rm = TRUE)
end_date <- max(combined_data$date, na.rm = TRUE)

# Create a data frame with all dates in the range
all_dates <- data.frame(date = seq.Date(from = start_date, to = end_date, by = "day"))

# Left join with your combined data
check_dates <- left_join(all_dates, combined_data, by = "date")

# Find dates with no data (where all other columns are NA)
missing_dates <- check_dates %>%
  filter(is.na(source)) %>%
  select(date)

missing_dates