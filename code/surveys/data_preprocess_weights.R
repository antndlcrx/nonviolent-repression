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


survey_feb <- survey_feb %>%
  mutate(
    gender = as.factor(Q4),
    # adjust dates 
    StartDate = as.POSIXct((as.numeric(StartDate) - 25569) * 86400, origin = "1970-01-01", tz = "UTC"),
    EndDate = as.POSIXct((as.numeric(EndDate) - 25569) * 86400, origin = "1970-01-01", tz = "UTC"),
    RecordedDate = as.POSIXct((as.numeric(RecordedDate) - 25569) * 86400, origin = "1970-01-01", tz = "UTC"),
    # adjust education
    university_education = case_when(Q5 %in% uni_education ~ "BA+",
                                     Q5 == "no_answer" ~ "NA",
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


