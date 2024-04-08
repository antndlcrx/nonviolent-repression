##### Set Up #####
pacman::p_load(tidyverse, readr, broom, 
               lubridate, gt, gtsummary, survey, readxl,
               gridExtra, knitr, haven)

Sys.getlocale()
# Set this to a locale that supports Cyrillic, for example, on Windows
Sys.setlocale("LC_ALL", "Russian")

# categories for education
uni_education <- c("Высшее образование (магистратура)",
                   "Высшее образование (бакалавриат / специалитет)",
                   "Научная степень (кандидат, доктор наук)")


# load survey data
survey_march <- read_csv("data/surveys/survey_march_post_election.csv",
                         locale = locale(encoding = "UTF-8")) %>% 
  mutate(gender = fct_recode(Q4,
                             "Man"= "Мужской",
                             "Woman" = "Женский"),
         #adjust age
         age_group = case_when(
           as.numeric(Q1) %in% c(18:24) ~ '18-24',
           as.numeric(Q1) %in% c(25:34) ~ '25-34',
           as.numeric(Q1) %in% c(35:44) ~ '35-44',
           as.numeric(Q1) %in% c(45:54) ~ '45-54',
           as.numeric(Q1) %in% c(55:64) ~ '55-64',
           as.numeric(Q1) >= 65 ~ '65+',
           TRUE ~ NA),
         university_education = case_when(Q5 %in% uni_education ~ "BA+",
                                          TRUE ~ "BA-")
  )


ru_population_frame <- read_csv("data/surveys/ru_population_frame.csv")[-c(1:3,16:18), -c(3,7)]

wider_age_categories <- list(
  '18-24' = c('18 – 19', '20 – 24'),
  '25-34' = c('25 – 29', '30 – 34'),
  '35-44' = c('35 – 39', '40 – 44'),
  '45-54' = c('45 – 49', '50 – 54'),
  '55-64' = c('55 – 59', '60 – 64'),
  '65+' = c('65 – 69', '70 и более')
)


map_age_category <- function(age) {
  for (category in names(wider_age_categories)) {
    if (age %in% wider_age_categories[[category]]) {
      return(category)
    }
  }
  return(NA)
}

ru_population_frame$wider_age <- sapply(ru_population_frame$Age, map_age_category)

collapsed_df <- ru_population_frame %>%
  group_by(Gender, wider_age) %>%
  summarise(
    `BA+` = sum(`BA+`, na.rm = TRUE),
    `BA-` = sum(`BA-`, na.rm = TRUE) + sum(`NA`, na.rm = TRUE)
  )


# harmonise categories
ru_population_frame <- collapsed_df %>%
  mutate(Gender = fct_recode(Gender,
                             "Man"= "Men",
                             "Woman" = "Women")
  ) %>%
  pivot_longer(
    cols = c("BA+", "BA-"), 
    names_to = "Education",
    values_to = "Count"
  )

# harmonise features
colnames(ru_population_frame) <- c("gender", "age_group", "university_education",
                                   "Freq")



#### weights survey library ####
unweighted_data <- svydesign(ids = ~1, data = survey_march)


weighted <- postStratify(unweighted_data, ~age_group + gender + university_education,
                         ru_population_frame, partial=TRUE)

# save weights 
survey_march$weight_poststratify <- weights(weighted)

### sanity check 
# sum_march <- round(summary(weights(weighted)), 2)
# 
# sum_march_mat <-  matrix(as.numeric(sum_march), nrow = 1, 
#                        dimnames = list(c("Value"),
#                                        names(sum_march)
#                        )
# )
# 
# top_five_rows_march <- survey_march %>%
#   distinct(weight_poststratify, .keep_all = TRUE) %>%  
#   arrange(desc(weight_poststratify)) %>%
#   select(age_group, gender, university_education, weight_poststratify) %>%
#   slice_head(n = 5)



#### weights manually ####
# calculate weights 

survey_march_strata <- survey_march %>% 
  group_by(gender, age_group, university_education) %>% 
  summarise(count = n()) %>%
  mutate(proportion = count / nrow(survey_march),
         source = "March Survey")

pop_strata <- ru_population_frame %>% 
  ungroup() %>% 
  mutate(proportion = Freq/sum(Freq),
         source = "Census 2020")

combined_strata <- combined_strata <- rbind(survey_march_strata,  pop_strata) 


weights_march_strata_man <- left_join(survey_march_strata,
                                    pop_strata,
                                    c("gender", "age_group", "university_education")) %>% 
  rename(population_proportion = proportion.y,
         sample_proportion = proportion.x)  %>% 
  # calculate weights as popul prop/sample prop
  mutate(weight = population_proportion / sample_proportion)

### sanity check
# sum_man_march <- round(summary(weights_march_strata_man$weight), 2)
# 
# sum_man_march_mat <-  matrix(as.numeric(sum_man_march), nrow = 1, 
#                            dimnames = list(c("Value"),
#                                            names(sum_man_march)
#                            )
# )


survey_march <- survey_march %>%
  left_join(weights_march_strata_man, by = c("age_group", "gender", "university_education")) %>%
  rename(weight_manually_calculated = weight) 

write.csv(survey_march, "data/surveys/survey_march_weights.csv", row.names = FALSE)
