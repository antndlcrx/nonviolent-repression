##### Set Up #####
pacman::p_load(tidyverse, readr, broom, 
               lubridate, gt, gtsummary, survey, readxl)


# load survey data
survey_feb <- read_xlsx("data/surveys/survey_feb.xlsx")
# clean data
column_descriptions <- as.character(unlist(survey_feb[1, ]))
names(column_descriptions) <- names(survey_feb)
# example, call: column_descriptions["Q1"]

# remove first row
survey_feb <- survey_feb[-1,]

# categories for education
uni_education <- c("Высшее образование (магистратура)",
                   "Высшее образование (бакалавриат / специалитет)",
                   "Научная степень (кандидат, доктор наук)")

na_education <- c("Затрудняюсь ответить", "Отказ от ответа")

survey_feb <- survey_feb %>%
  mutate(
    gender = as.factor(Q4),
    # adjust dates 
    StartDate = as.POSIXct((as.numeric(StartDate) - 25569) * 86400, origin = "1970-01-01", tz = "UTC"),
    EndDate = as.POSIXct((as.numeric(EndDate) - 25569) * 86400, origin = "1970-01-01", tz = "UTC"),
    RecordedDate = as.POSIXct((as.numeric(RecordedDate) - 25569) * 86400, origin = "1970-01-01", tz = "UTC"),
    # adjust education
    university_education = case_when(Q5 %in% uni_education ~ "BA+",
                                     Q5 %in% na_education ~ "NA",
                                     TRUE ~ "BA-"),
    #adjust age
    age_group = case_when(
      as.numeric(Q1) %in% c(18:19) ~ '18-19',
      as.numeric(Q1) %in% c(20:24) ~ '20-24',
      as.numeric(Q1) %in% c(25:29) ~ '25-29',
      as.numeric(Q1) %in% c(30:34) ~ '30-34',
      as.numeric(Q1) %in% c(35:39) ~ '35-39',
      as.numeric(Q1) %in% c(40:44) ~ '40-44',
      as.numeric(Q1) %in% c(45:49) ~ '45-49',
      as.numeric(Q1) %in% c(50:54) ~ '50-54',
      as.numeric(Q1) %in% c(55:59) ~ '55-59',
      as.numeric(Q1) %in% c(60:64) ~ '60-64',
      as.numeric(Q1) %in% c(65:69) ~ '65-69',
      as.numeric(Q1) >= 70 ~ '70+',
      TRUE ~ NA),
    # pull list experiment into one variable 
    list_treatment = case_when(is.na(`Q14 - version 2`) ~ 0, 
                          TRUE ~ 1),
    list_count = coalesce(`Q14 - version 1`,
                          `Q14 - version 2`),
    # pull framing experiment into one variable
    frame_treatment = case_when(
      `Q15 - Group 1` != 0 ~ "Group 1",
      `Q15 - Group 2` != 0 ~ "Group 2",
      `Q15 - Group 3` != 0 ~ "Group 3",
      `Q15 - Group 4` != 0 ~ "Group 4",
      `Q15 - Group 5` != 0 ~ "Group 5",
      `Q15 - Group 6` != 0 ~ "Group 6",
      `Q15 - Group 7` != 0 ~ "Group 7",
      `Q15 - Group 8` != 0 ~ "Group 8",
      TRUE ~ NA_character_
    )
    ,
    Q15 = coalesce(`Q15 - Group 1`, `Q15 - Group 2`, `Q15 - Group 3`, `Q15 - Group 4`, 
                          `Q15 - Group 5`, `Q15 - Group 6`, `Q15 - Group 7`, `Q15 - Group 8`)
    
  )


# load population frame minus rows containing information on underaged categories and redundant total categories
ru_population_frame <- read_csv("data/surveys/ru_population_frame.csv")[-c(1:3,16:18), -c(3,7)]

# harmonise categories
ru_population_frame <- ru_population_frame %>%
  mutate(Age = fct_recode(Age,
                          "35-39" = "35 – 39",
                          "40-44" = "40 – 44",
                          "25-29" = "25 – 29",
                          "45-49" = "45 – 49",
                          "30-34" = "30 – 34",
                          "55-59" = "55 – 59",
                          "50-54" = "50 – 54",
                          "20-24" = "20 – 24",
                          "60-64" = "60 – 64",
                          "18-19" = "18 – 19",
                          "65-69" = "65 – 69",
                          "70+" = "70 и более"),
         Gender = fct_recode(Gender,
                             "Мужской"= "Men",
                             "Женский" = "Women")
  ) %>%
  pivot_longer(
    cols = c("BA+", "BA-", "NA"), 
    names_to = "Education",
    values_to = "Count"
  )

# harmonise features
colnames(ru_population_frame) <- c("gender", "age_group", "university_education",
                                   "Freq")
# 
ru_population_frame[1,4] = 1
ru_population_frame[37,4] = 1

##### Weighting ####

## explore difference in strata proportions between population and sample
# caluculate proporitons 
survey_strata <- survey_feb %>% 
  group_by(gender, age_group, university_education) %>% 
  summarise(count = n()) %>%
  mutate(proportion = count / nrow(survey_feb),
         source = "survey")

pop_strata <- ru_population_frame %>% 
  mutate(proportion = Freq/sum(Freq),
         source = "population")

pop_age <- ru_population_frame %>%
  select(age_group, Freq)%>%
  group_by(age_group) %>%
  summarise(Freq = sum(Freq))

pop_gender <- ru_population_frame %>%
  select(gender, Freq)%>%
  group_by(gender) %>%
  summarise(Freq = sum(Freq))

# 
# pop_edu <- ru_population_frame %>% 
#   mutate(proportion = Freq/sum(Freq),
#          source = "population")

# combine dfs for plotting
combined_strata <- rbind(survey_strata, pop_strata)

## plot differences
ggplot(combined_strata, aes(x = university_education,
                            y = proportion,
                            color = source)) +
  geom_point(size = 3) +
  geom_line(aes(group = interaction(age_group, university_education))) +
  facet_grid(gender ~ age_group) +
  labs(x = "University Education", y = "Proportion", color = "Gender", shape = "Gender") +
  theme(legend.position = "bottom") + 
  theme_bw()


# sanity check
# xtabs(~age_group + gender + university_education, survey_feb)

## survey library ##
unweighted_data <- svydesign(ids = ~1, data = survey_feb)

# rake(unweighted_data, list(~age_group+gender+university_education),
#      list(ru_population_frame))

weighted <- postStratify(unweighted_data, ~age_group + gender + university_education,
             ru_population_frame, partial=TRUE)


summary(weights(weighted))
# save weights 
survey_feb$weights <- weights(weighted)


# examine weights
ggplot(survey_feb, aes(x = weights)) +
  geom_histogram(fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Weights",
       x = "Weight",
       y = "Frequency")
