##### COMPARISON OF ATTIRUDES TO REPRESSIONS ###########
#### 1.1 Load packages ####
library(haven)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(labelled)
library(readxl)
library(tidyr)
library(tidyverse)

#library(writexl)


#### 1.2 Load data ####
# Ljuly <- read_stata("/Users/nuff/Library/CloudStorage/OneDrive-SharedLibraries-Nexus365/Katerina\ Tertytchnaya\ -\ Qualtrics/2019_July/omnibus-2019-july.dta")
# Lsep <- read_stata("/Users/emitrokhina/Library/CloudStorage/OneDrive-SharedLibraries-Nexus365/Katerina\ Tertytchnaya\ -\ Qualtrics/2020_September/omnibus/sep_combined_final2.dta", encoding = "utf8")
# Lsep_exp <- read_sav("Library/CloudStorage/OneDrive-SharedLibraries-Nexus365/Katerina Tertytchnaya - Qualtrics/2020_September/20cur09 Эксперимент.sav")
# res <- read_stata("~/Library/CloudStorage/OneDrive-SharedLibraries-Nexus365/Katerina Tertytchnaya - Qualtrics/res_waves/all_waves_and_samples_merged.dta")
# Qaug <- read.csv("/Users/emitrokhina/Library/CloudStorage/OneDrive-SharedLibraries-Nexus365/Katerina\ Tertytchnaya\ -\ Qualtrics/data/survey_with_weights.csv")
# Qfeb <- read.csv("/Users/emitrokhina/Library/CloudStorage/OneDrive-SharedLibraries-Nexus365/Katerina\ Tertytchnaya\ -\ Qualtrics/2024_February/feb_clean.csv")
# Qapr <- read.csv("/Users/emitrokhina/Library/CloudStorage/OneDrive-SharedLibraries-Nexus365/Katerina\ Tertytchnaya\ -\ Qualtrics/2024_MarApril/post_election.csv")


DATA_PATH <- "C:/Users/murrn/GitHub/nonviolent-repression/data/surveys/from_one_drive/"

Ljuly <- read_stata(file.path(DATA_PATH, "omnibus-2019-july.dta"))
Lsep <- read_stata(file.path(DATA_PATH, "sep_combined_final2.dta"), encoding = "utf8")
Lsep_exp <- read_sav(file.path(DATA_PATH, "20cur09 Эксперимент.sav"))
res <- read_stata(file.path(DATA_PATH, "all_waves_and_samples_merged.dta"))
Qaug <- read.csv(file.path(DATA_PATH, "survey_with_weights.csv"))
Qfeb <- read.csv(file.path(DATA_PATH, "feb_clean.csv"))
Qapr <- read.csv(file.path(DATA_PATH, "post_election.csv"))
QJuly <- read_excel(file.path(DATA_PATH, "july_2024.xlsx"))
Q_sept <- survey_sept <- read_csv("data/surveys/survey_sept_2024_with_weights.csv",
                                  locale = locale(encoding = "UTF-8"))

QJuly <- QJuly[-1, ]
# Convert all columns to their appropriate data types if needed after removing first row
QJuly <- QJuly %>% mutate(across(everything(), type.convert, as.is = TRUE))


Q_sept <- Q_sept %>% 
  rename(putin_support = Q12, law_obedience = Q13, unauth_protest_partic = Q16_2,
         approved_protest_partic = Q16_1) %>% 
  mutate(survey_time = "September 2024",
         law_obedience = case_when(
           law_obedience == "В некоторых случаях следовать голосу своей совести, даже если это ведет к нарушению закона" ~ "sometimes_violate",
           law_obedience == "Подчиняться закону во всех случаях, без исключений" ~ "always_abide",
           TRUE ~ law_obedience  
         )
         )

#### 1.3. Selecting variables ####
Ljul_selected <- Ljuly %>%
  select(qobl, qto, qs1, qs2, qs3, qd8, qq10_cod, qq7a, q4, qq7b, qq5) %>%
  rename(region = qobl, settlement = qto, age = qs2, gender = qs1, educaction = qs3, income = qd8, vote_next = qq10_cod,
         approved_protest_partic	= qq7a, putin_support = q4, unauth_protest_partic = qq7b, 
         arrest_participants = qq5)
Ljul_selected$survey_indicator <- 1
Ljul_selected$survey_time <- "July 2020"

# qQ5 
# 1	полностью оправданы
# 2	скорее оправданы
# 3	ни то чтобы оправданы, ни не оправданы
# 4	скорее не оправданы
# 5	совершенно не оправданы
# 97	затрудняюсь ответить
# 98	отказ от ответа

Lsep_s <- Lsep %>%
  select (qS1, age, education, q2A, q19A, qD1B, qD8, qRNP, q19B) %>%
  rename (gender = qS1, age=age, educaction=education, putin_support	= q2A, approved_protest_partic = q19A, unauth_protest_partic = q19B,
          vote_next=	qD1B, income=qD8, region	=qRNP)
Lsep_exp <- Lsep_exp %>%
  select(qQ3B) %>%
  rename(law_obedience_organisers_participants = qQ3B)
Lsep_selected <- cbind(Lsep_s, Lsep_exp)
Lsep_selected$survey_indicator <- 2
Lsep_selected$survey_time <- "September 2020"

unique(Lsep_selected$law_obedience_organisers_participants
       )

# ## <labelled<double>[7]>: Q3B. СКАЖИТЕ, ПОЖАЛУЙСТА, НАСКОЛЬКО ВЫ СОГЛАСНЫ ИЛИ НЕ СОГЛАСНЫ С ТЕМ, 
# ЧТО УЧАСТНИКИ И ОРГАНИЗАТОРЫ ЛЮБЫХ МИТИНГОВ ДОЛЖНЫ ПОДЧИНЯТЬСЯ ЗАКОНУ ВО ВСЕХ СЛУЧАЯХ, 
# ДАЖЕ ЕСЛИ ОНИ НЕ СОГЛАСНЫ С РЕШЕНИЕМ ОРГАНОВ ВЛАСТИ?


res_selected <- res %>%
  select(age_round1, gender_round1, educ_round1, q39, qO1, q80_1, q80_2, q80_3, q81_1, q81_2, q81_3, qG5, qD6) %>%
  rename(age = age_round1, gender = gender_round1, educaction = educ_round1, vote_next = q39, law_obedience = qO1,
         protest_permition = q80_1, arrest_participants = q80_2,
         arrest_organizers = q80_3, limit_websites = q81_1, surveilance = q81_2, foreign_agent = q81_3,
         putin_support = qG5, income = qD6) %>% 
  mutate(law_obedience = as.character(law_obedience)) %>%
  mutate(law_obedience = case_when(
    law_obedience == "2" ~ "sometimes_violate",
    law_obedience == "1" ~ "always_abide",
    law_obedience %in% c("98", "99") ~ "no_answer",
    TRUE ~ law_obedience
  ))

res_selected$survey_indicator <- 3
res_selected$survey_time <- "December 2021"

unique(res_selected$law_obedience)

sum(is.na(res_selected$law_obedience)) #729

# 1. Подчиняться закону во всех случаях, без исключений,
# 2. В некоторых случаях следовать голосу своей совести, даже если это ведет к нарушению закона,
# 98. Затрудняюсь ответить,
# 99. Отказ от ответа

## August
Qaug_selected <- Qaug %>%
  select(age, gender, education, president_candidate_Q9, family_finance_today_Q24, region, law_Q11, protest_authorization_Q12_1, 
         protest_arrest_participants_Q12_2, protest_arrest_organizers_Q12_3, 
         media_foreign_websites_Q13_1, media_surveilliance_Q13_2, media_foreign_agents_Q13_3, 
         protest_participate_authorized_Q16_1, protest_participate_unauthorized_Q16_2, putin_approve_Q8) %>%
  rename(age = age, gender = gender, educaction = education, vote_next = president_candidate_Q9, income = family_finance_today_Q24, region = region, 
         law_obedience = law_Q11, protest_permition = protest_authorization_Q12_1, 
         arrest_participants = protest_arrest_participants_Q12_2,
         arrest_organizers = protest_arrest_organizers_Q12_3,
         limit_websites = media_foreign_websites_Q13_1, surveilance = media_surveilliance_Q13_2, 
         foreign_agent = media_foreign_agents_Q13_3, approved_protest_partic = protest_participate_authorized_Q16_1, 
         unauth_protest_partic = protest_participate_unauthorized_Q16_2, putin_support = putin_approve_Q8)%>% 
  mutate(law_obedience = case_when(
    law_obedience == "В некоторых случаях следовать голосу своей совести, даже если это ведет к нарушению закона" ~ "sometimes_violate",
    law_obedience == "Подчиняться закону во всех случаях, без исключений" ~ "always_abide",
    TRUE ~ law_obedience  
  ))

Qaug_selected$survey_indicator <- 4
Qaug_selected$survey_time <- "August 2023"

unique(Qaug_selected$law_obedience)
sum(is.na(Qaug_selected$law_obedience))


# 12A. По Вашему мнению, насколько в нынешних условиях оправданы следующие действия государства по регулированию протестов в России? 
#   Требование, чтобы организаторы протеста получили разрешение на проведение митинга
# 12B  Задержание людей, участвовавших в несанкционированных митингах
# 12C  Задержание организаторов несанкционированных митингов

# 16.A .... Участие в согласованной демонстрации
# 16B. Есть ряд возможностей, с помощью которых граждане могут выражать свое мнение. Скажите, пожалуйста, участвовали ли Вы когда-либо в своей жизни в таких действиях и считаете ли Вы, что это допустимый способ выражения своей позиции такими способами? 
#   Участие в несогласованной демонстрации

## February
Qfeb_selected <- Qfeb %>% 
  select(q1, q4, q2, q5, q8, q20, q9, q11_1, q11_2, q7) %>%
  rename(age = q1, gender = q4, educaction = q5, vote_next = q8, income = q20, region = q2, law_obedience = q9, 
         approved_protest_partic = q11_1, unauth_protest_partic = q11_2, putin_support = q7) %>% 
  mutate(law_obedience = case_when(
    law_obedience == "2" ~ "sometimes_violate",
    law_obedience == "1" ~ "always_abide",
    law_obedience == "3" ~ "no_answer",
    law_obedience == "4" ~ "no_answer"
  ))

Qfeb_selected$survey_indicator <- 5
Qfeb_selected$survey_time <- "February 2024"

unique(Qfeb_selected$law_obedience)

# 1
# Подчиняться закону во всех случаях, без исключений
# 2
# В некоторых случаях следовать голосу своей совести, даже если это ведет к нарушению закона
# 3
# Затрудняюсь ответить
# 4
# Отказ от ответа



# 11.A .... Участие в согласованной демонстрации
# 11B. Есть ряд возможностей, с помощью которых граждане могут выражать свое мнение. Скажите, пожалуйста, участвовали ли Вы когда-либо в своей жизни в таких действиях и считаете ли Вы, что это допустимый способ выражения своей позиции такими способами? 
#   Участие в несогласованной демонстрации


Qapr_selected <- Qapr %>% 
  select(Q1, Q4, Q5, Q2, Q21, Q12, Q17_1, Q17_2, Q18_1, Q18_2, Q18_3, Q11) %>%
  rename(age = Q1, gender = Q4, educaction = Q5, income = Q21, region = Q2, law_obedience = Q12,limit_websites = Q17_1, 
         protest_permition = Q18_1, arrest_participants = Q18_2,
         arrest_organizers = Q18_3, foreign_agent = Q17_2, putin_support = Q11) %>% 
  mutate(law_obedience = case_when(
    law_obedience == "В некоторых случаях следовать голосу своей совести, даже если это ведет к нарушению закона " ~ "sometimes_violate",
    law_obedience == "Затрудняюсь ответить  " ~ "no_answer",
    law_obedience == "Отказ от ответа " ~ "no_answer",
    law_obedience == "Подчиняться закону во всех случаях, без исключений " ~ "always_abide",
    TRUE ~ law_obedience 
  ))

Qapr_selected$survey_indicator <- 6
Qapr_selected$survey_time <- "April 2024"




# 18A..Tребование, чтобы организаторы протеста получили разрешение на проведение митинга
# 18B..Задержание людей, участвовавших в несанкционированных митингах
# 18C..Задержание людей, участвовавших в несанкционированных митингах (DUPLICATE)

QJuly_selected <- QJuly %>% 
  select(Q1:Q21) %>% 
  rename(age = Q1, gender = Q4, educaction = Q5, income = Q19, region = Q2,
         putin_support = Q11, law_obedience = Q12, vote = Q13,
         protest_permition = Q16_1, arrest_participants = Q16_2,
         arrest_participants_authorised = Q16_3) %>% 
  mutate(law_obedience = case_when(
    law_obedience == "В некоторых случаях следовать голосу своей совести, даже если это ведет к нарушению закона" ~ "sometimes_violate",
    law_obedience == "Затрудняюсь ответить" ~ "no_answer",
    law_obedience == "Отказ от ответа" ~ "no_answer",
    law_obedience == "Подчиняться закону во всех случаях, без исключений" ~ "always_abide",
    TRUE ~ law_obedience  # Retain original value if it doesn't match any of the conditions
  ))

QJuly_selected$survey_time <- "July 2024"


# 16A..Tребование, чтобы организаторы протеста получили разрешение на проведение митинга
# 16B..Задержание людей, участвовавших в несанкционированных митингах
# 16C..Задержание людей, участвовавших в санкционированных митингах (NOT ORGANISERS)


# clean environment
rm(list = c("Ljuly", "Lsep","res", "Qaug", "Qfeb", "Qapr", "Lsep_s", "Lsep_exp")) #remove source dataset 

#### Recoding ####
# Law obidience
table(Qfeb_selected$law_obedience)
# Qfeb_selected$law_obedience[Qfeb_selected$law_obedience == 3] <- "no_answer"
# Qfeb_selected$law_obedience[Qfeb_selected$law_obedience == 4] <- "no_answer"
# 
# table(Ljuly$law_obedience)


# res unk what 1 and 2 stand for
# July 2019 has no law question

#Education
table(Ljul_selected$educaction)
Ljul_selected$educaction[Ljul_selected$educaction == 4] <- 3
Ljul_selected$educaction[Ljul_selected$educaction == 5] <- 4
Ljul_selected$educaction[Ljul_selected$educaction == 6] <- 5

table(Lsep_selected$educaction)
Lsep_selected$educaction[Lsep_selected$educaction == 4] <- 3
Lsep_selected$educaction[Lsep_selected$educaction == 5] <- 4
Lsep_selected$educaction[Lsep_selected$educaction == 6] <- 5

table(res_selected$educaction)
res_selected$educaction[res_selected$educaction == 2] <- 1
res_selected$educaction[res_selected$educaction == 3] <- 2
res_selected$educaction[res_selected$educaction == 4] <- 2
res_selected$educaction[res_selected$educaction == 5] <- 3
res_selected$educaction[res_selected$educaction == 6] <- 4
res_selected$educaction[res_selected$educaction == 7] <- 5
res_selected$educaction[res_selected$educaction == 8] <- 5
res_selected$educaction[res_selected$educaction == 98] <- 6
res_selected$educaction[res_selected$educaction == 99] <- 6

table(Qaug_selected$educaction)
Qaug_selected$educaction[Qaug_selected$educaction == "no_answer"] <- 6
Qaug_selected$educaction[Qaug_selected$educaction == "Начальное"] <- 1
Qaug_selected$educaction[Qaug_selected$educaction == "Законченное среднее образование (школа, лицей, гимназия)"] <- 2
Qaug_selected$educaction[Qaug_selected$educaction == "Неполное среднее плюс начальное профессиональное образование (ПТУ, ФЗУ, РУ без среднего образования)"] <- 3
Qaug_selected$educaction[Qaug_selected$educaction == "Среднее специальное или профессионально-техническое (ПТУ, техникум, училище)"] <- 3
Qaug_selected$educaction[Qaug_selected$educaction == "Незаконченное высшее (не менее трех курсов вуза)"] <- 4
Qaug_selected$educaction[Qaug_selected$educaction == "Высшее образование (бакалавриат / специалитет)"] <- 5
Qaug_selected$educaction[Qaug_selected$educaction == "Высшее образование (магистратура)"] <- 5
Qaug_selected$educaction[Qaug_selected$educaction == "Научная степень (кандидат, доктор наук)"] <- 5

table(Qfeb_selected$educaction)
Qfeb_selected$educaction[Qfeb_selected$educaction == 4] <- 3
Qfeb_selected$educaction[Qfeb_selected$educaction == 5] <- 4
Qfeb_selected$educaction[Qfeb_selected$educaction == 6] <- 5
Qfeb_selected$educaction[Qfeb_selected$educaction == 7] <- 5
Qfeb_selected$educaction[Qfeb_selected$educaction == 8] <- 5
Qfeb_selected$educaction[Qfeb_selected$educaction == 9] <- 6
Qfeb_selected$educaction[Qfeb_selected$educaction == 10] <- 6

table(Qapr_selected$educaction)
Qapr_selected$educaction[Qapr_selected$educaction == "no_answer"] <- 6
Qapr_selected$educaction[Qapr_selected$educaction == "Затрудняюсь ответить"] <- 6
Qapr_selected$educaction[Qapr_selected$educaction == "Начальное"] <- 1
Qapr_selected$educaction[Qapr_selected$educaction == "Законченное среднее образование (школа, лицей, гимназия)"] <- 2
Qapr_selected$educaction[Qapr_selected$educaction == "Неполное среднее плюс начальное профессиональное образование (ПТУ, ФЗУ, РУ без среднего образования)"] <- 3
Qapr_selected$educaction[Qapr_selected$educaction == "Среднее специальное или профессионально-техническое (ПТУ, техникум, училище)"] <- 3
Qapr_selected$educaction[Qapr_selected$educaction == "Незаконченное высшее (не менее трех курсов вуза)"] <- 4
Qapr_selected$educaction[Qapr_selected$educaction == "Высшее образование (бакалавриат / специалитет)"] <- 5
Qapr_selected$educaction[Qapr_selected$educaction == "Высшее образование (магистратура)"] <- 5
Qapr_selected$educaction[Qapr_selected$educaction == "Научная степень (кандидат, доктор наук)"] <- 5

### Income
table(Ljul_selected$income)
Ljul_selected$income[Ljul_selected$income == 0] <- 8
Ljul_selected$income[Ljul_selected$income == 6] <- 7
Ljul_selected$income[Ljul_selected$income == 5] <- 6
Ljul_selected$income[Ljul_selected$income == 4] <- 5  
Ljul_selected$income[Ljul_selected$income == 3] <- 4
Ljul_selected$income[Ljul_selected$income == 2] <- 3

table(Lsep_selected$income)
Lsep_selected$income[Lsep_selected$income == 0] <- 8
Lsep_selected$income[Lsep_selected$income == 6] <- 7
Lsep_selected$income[Lsep_selected$income == 5] <- 6
Lsep_selected$income[Lsep_selected$income == 4] <- 5  
Lsep_selected$income[Lsep_selected$income == 3] <- 4
Lsep_selected$income[Lsep_selected$income == 2] <- 3

table(res_selected$income)
res_selected$income[res_selected$income == 98] <- 8
res_selected$income[res_selected$income == 99] <- 8

table(Qaug_selected$income)
Qaug_selected$income[Qaug_selected$income == "no_answer"] <- 8
Qaug_selected$income[Qaug_selected$income == "В настоящее время Вы можете практически ни в чем себе не отказывать"] <- 7
Qaug_selected$income[Qaug_selected$income == "Покупка машины не вызывает у Вас особых трудностей, однако покупка жилья Вам пока недоступна"] <- 6
Qaug_selected$income[Qaug_selected$income == "Покупка дорогих товаров не вызывает у Вас особых трудностей, однако покупка машины Вам пока недоступна"] <- 5
Qaug_selected$income[Qaug_selected$income == "Денег в основном хватает, но для покупки дорогих товаров, таких как, например, холодильник, телевизор, стиральная машина, Вы должны очень долго копить деньги или брать в долг, в кредит"] <- 4
Qaug_selected$income[Qaug_selected$income == "На повседневные расходы денег достаточно, но уже покупка одежды представляет для Вас трудности"] <- 3
Qaug_selected$income[Qaug_selected$income == "Денег хватает только на самое необходимое "] <- 2  
Qaug_selected$income[Qaug_selected$income == "Денег не хватает даже на еду"] <- 1
  
table(Qfeb_selected$income)
Qfeb_selected$income[Qfeb_selected$income == 9] <- 8

table(Qapr_selected$income)
Qapr_selected$income[Qapr_selected$income == "Отказ от ответа "] <- 8
Qapr_selected$income[Qapr_selected$income == "Затрудняюсь ответить  "] <- 8
Qapr_selected$income[Qapr_selected$income == "В настоящее время Вы можете практически ни в чем себе не отказывать "] <- 7
Qapr_selected$income[Qapr_selected$income == "Покупка машины не вызывает у Вас особых трудностей, однако покупка жилья Вам пока недоступна "] <- 6
Qapr_selected$income[Qapr_selected$income== "Покупка дорогих товаров не вызывает у Вас особых трудностей, однако покупка машины Вам пока недоступна "] <- 5
Qapr_selected$income[Qapr_selected$income == "Денег в основном хватает, но для покупки дорогих товаров, таких как, например, холодильник, телевизор, стиральная машина, Вы должны очень долго копить деньги или брать в долг, в кредит "] <- 4
Qapr_selected$income[Qapr_selected$income == "На повседневные расходы денег достаточно, но уже покупка одежды представляет для Вас трудности "] <- 3
Qapr_selected$income[Qapr_selected$income == "Денег хватает только на самое необходимое  "] <- 2  
Qapr_selected$income[Qapr_selected$income == "Денег не хватает даже на еду "] <- 1

### Vote next
table(Ljul_selected$vote_next)
Ljul_selected$vote_next[Ljul_selected$vote_next == 4] <- 8
Ljul_selected$vote_next[Ljul_selected$vote_next == 5] <- 8
Ljul_selected$vote_next[Ljul_selected$vote_next == 6] <- 4
Ljul_selected$vote_next[Ljul_selected$vote_next == 7] <- 5
Ljul_selected$vote_next[Ljul_selected$vote_next == 9] <- 8
Ljul_selected$vote_next[Ljul_selected$vote_next == 97] <- 10
Ljul_selected$vote_next[Ljul_selected$vote_next == 98] <- 10
Ljul_selected$vote_next[Ljul_selected$vote_next == 99] <- 10

table(Lsep_selected$vote_next)
Lsep_selected$vote_next[Lsep_selected$vote_next == 8] <- 101
Lsep_selected$vote_next[Lsep_selected$vote_next == 1] <- 8
Lsep_selected$vote_next[Lsep_selected$vote_next == 2] <- 1
Lsep_selected$vote_next[Lsep_selected$vote_next == 3] <- 2
Lsep_selected$vote_next[Lsep_selected$vote_next == 5] <- 8
Lsep_selected$vote_next[Lsep_selected$vote_next == 4] <- 5
Lsep_selected$vote_next[Lsep_selected$vote_next == 6] <- 8
Lsep_selected$vote_next[Lsep_selected$vote_next == 7] <- 8
Lsep_selected$vote_next[Lsep_selected$vote_next == 101] <- 6

table(res_selected$vote_next)
res_selected$vote_next[res_selected$vote_next == 1] <- 100
res_selected$vote_next[res_selected$vote_next == 2] <- 1
res_selected$vote_next[res_selected$vote_next == 100] <- 2
res_selected$vote_next[res_selected$vote_next == 8] <- 9
res_selected$vote_next[res_selected$vote_next == 4] <- 8
res_selected$vote_next[res_selected$vote_next == 5] <- 4
res_selected$vote_next[res_selected$vote_next == 6] <- 5
res_selected$vote_next[res_selected$vote_next == 7] <- 6
res_selected$vote_next[res_selected$vote_next == 98] <- 10
res_selected$vote_next[res_selected$vote_next == 99] <- 10
res_selected$vote_next[res_selected$vote_next == 96] <- 10

table(Qaug_selected$vote_next)
Qaug_selected$vote_next[Qaug_selected$vote_next == "no_answer"] <- 10
Qaug_selected$vote_next[Qaug_selected$vote_next == "Владимир Путин"] <- 5
Qaug_selected$vote_next[Qaug_selected$vote_next == "Леонид Слуцкий"] <- 7
Qaug_selected$vote_next[Qaug_selected$vote_next == "Сергей Миронов"] <- 8
Qaug_selected$vote_next[Qaug_selected$vote_next == "Алексей Навальный"] <- 4
Qaug_selected$vote_next[Qaug_selected$vote_next == "Григорий Явлинский"] <- 6
Qaug_selected$vote_next[Qaug_selected$vote_next == "Павел Грудинин"] <- 1
Qaug_selected$vote_next[Qaug_selected$vote_next == "Не стал бы участвовать в выборах"] <- 10
Qaug_selected$vote_next[Qaug_selected$vote_next == "Опустили бы недействительный бюллетень / испортили или унесли бюллетень"] <- 9

table(Qfeb_selected$vote_next)
Qfeb_selected$vote_next[Qfeb_selected$vote_next == 9] <- 10
Qfeb_selected$vote_next[Qfeb_selected$vote_next == 7] <- 10
Qfeb_selected$vote_next[Qfeb_selected$vote_next == 8] <- 9
Qfeb_selected$vote_next[Qfeb_selected$vote_next == 1] <- 7
Qfeb_selected$vote_next[Qfeb_selected$vote_next == 3] <- 8
Qfeb_selected$vote_next[Qfeb_selected$vote_next == 2] <- 1

table(QJuly_selected$vote)
QJuly_selected$vote_next <- QJuly_selected$vote
QJuly_selected$vote_next[QJuly_selected$vote_next == "Отказ от ответа"] <- 10
QJuly_selected$vote_next[QJuly_selected$vote_next == "Владимир Путин"] <- 5
QJuly_selected$vote_next[QJuly_selected$vote_next == "Леонид Слуцкий"] <- 7
QJuly_selected$vote_next[QJuly_selected$vote_next == "Владислав Даванков"] <- 9 # ARBITRARY, NEEDS RECONSIDERATION!
QJuly_selected$vote_next[QJuly_selected$vote_next == "Затрудняюсь ответить"] <- 10
QJuly_selected$vote_next[QJuly_selected$vote_next == "Николай Харитонов"] <- 1 # Instead of Grudinin
QJuly_selected$vote_next[QJuly_selected$vote_next == "Не участвовал(а) в выборах"] <- 10
QJuly_selected$vote_next[QJuly_selected$vote_next == "Опустил(а) недействительный бюллетень / испортил(а) или унесл(а) бюллетень"] <- 9


#Gender
table(Ljul_selected$gender)
table(Lsep_selected$gender)
table(res_selected$gender)
table(Qaug_selected$gender)
table(Qfeb_selected$gender)
table(Qapr_selected$gender)

Qapr_selected$gender[Qapr_selected$gender == "Мужской "] <- 1
Qapr_selected$gender[Qapr_selected$gender == "Женский"] <- 2

QJuly_selected$gender[QJuly_selected$gender == "Мужской "] <- 1
QJuly_selected$gender[QJuly_selected$gender == "Женский"] <- 2

#Approve protest participation
table(Ljul_selected$approved_protest_partic)
Ljul_selected$approved_protest_partic[Ljul_selected$approved_protest_partic == 97] <- 4
Ljul_selected$approved_protest_partic[Ljul_selected$approved_protest_partic == 98] <- 4

table(Lsep_selected$approved_protest_partic)
Lsep_selected$approved_protest_partic[Lsep_selected$approved_protest_partic == 5] <- 4

table(Qaug_selected$approved_protest_partic)
Qaug_selected$approved_protest_partic[Qaug_selected$approved_protest_partic == "Я считаю этот способ допустимым, и сам(а) участвовал (а)"] <- 1
Qaug_selected$approved_protest_partic[Qaug_selected$approved_protest_partic == "Я считаю такой способ допустимым, но сам(а) не участвовал(а)"] <- 2
Qaug_selected$approved_protest_partic[Qaug_selected$approved_protest_partic == "Я считаю такой способ недопустимым и сам(а) не участвовал(а)"] <- 3
Qaug_selected$approved_protest_partic[Qaug_selected$approved_protest_partic == "no_answer"] <- 4

table(Qfeb_selected$approved_protest_partic)
Qfeb_selected$approved_protest_partic[Qfeb_selected$approved_protest_partic == 5] <- 4

#Putin support
table(Ljul_selected$putin_support)
Ljul_selected$putin_support[Ljul_selected$putin_support == 9] <- 3

table(Lsep_selected$putin_support)

table(res_selected$putin_support)
res_selected$putin_support[res_selected$putin_support == 98] <- 3
res_selected$putin_support[res_selected$putin_support == 99] <- 3

table(Qaug_selected$putin_support)
Qaug_selected$putin_support[Qaug_selected$putin_support == "Одобряю"] <- 1
Qaug_selected$putin_support[Qaug_selected$putin_support == "Не одобряю"] <- 2
Qaug_selected$putin_support[Qaug_selected$putin_support == "no_answer"] <- 3

table(Qfeb_selected$putin_support)
Qfeb_selected$putin_support[Qfeb_selected$putin_support == 4] <- 3

table(Qapr_selected$putin_support)
Qapr_selected$putin_support[Qapr_selected$putin_support == "Затрудняюсь ответить  "] <- 3
Qapr_selected$putin_support[Qapr_selected$putin_support == "Отказ от ответа "] <- 3
Qapr_selected$putin_support[Qapr_selected$putin_support == "Одобряю  "] <- 1
Qapr_selected$putin_support[Qapr_selected$putin_support == "Не одобряю  "] <- 2

QJuly_selected$putin_support[QJuly_selected$putin_support == "Затрудняюсь ответить"] <- 3
QJuly_selected$putin_support[QJuly_selected$putin_support == "Отказ от ответа"] <- 3
QJuly_selected$putin_support[QJuly_selected$putin_support == "Одобряю"] <- 1
QJuly_selected$putin_support[QJuly_selected$putin_support == "Не одобряю"] <- 2

#Law obedience
table(Lsep_selected$law_obedience)
Lsep_selected$law_obedience[Lsep_selected$law_obedience == 1] <- 2
Lsep_selected$law_obedience[Lsep_selected$law_obedience == 2] <- 2
Lsep_selected$law_obedience[Lsep_selected$law_obedience == 4] <- 1
Lsep_selected$law_obedience[Lsep_selected$law_obedience == 5] <- 1
Lsep_selected$law_obedience[Lsep_selected$law_obedience == 88] <- 3
Lsep_selected$law_obedience[Lsep_selected$law_obedience == 99] <- 3

table(res_selected$law_obedience)
res_selected$law_obedience[res_selected$law_obedience == 98] <- 3
res_selected$law_obedience[res_selected$law_obedience == 99] <- 3

table(Qaug_selected$law_obedience)
Qaug_selected$law_obedience[Qaug_selected$law_obedience == "Подчиняться закону во всех случаях, без исключений"] <- 1
Qaug_selected$law_obedience[Qaug_selected$law_obedience == "В некоторых случаях следовать голосу своей совести, даже если это ведет к нарушению закона"] <- 2
Qaug_selected$law_obedience[Qaug_selected$law_obedience == "no_answer"] <- 3

table(Qfeb_selected$law_obedience)
Qfeb_selected$law_obedience[Qfeb_selected$law_obedience == "no_answer"] <- 3

table(Qapr_selected$law_obedience)
Qapr_selected$law_obedience[Qapr_selected$law_obedience == "Подчиняться закону во всех случаях, без исключений "] <- 1
Qapr_selected$law_obedience[Qapr_selected$law_obedience == "В некоторых случаях следовать голосу своей совести, даже если это ведет к нарушению закона "] <- 2
Qapr_selected$law_obedience[Qapr_selected$law_obedience == "Отказ от ответа "] <- 3
Qapr_selected$law_obedience[Qapr_selected$law_obedience == "Затрудняюсь ответить  "] <- 3

# Protest permition
table(res_selected$protest_permition)
res_selected$protest_permition[res_selected$protest_permition == 2] <- 1
res_selected$protest_permition[res_selected$protest_permition == 3] <- 2
res_selected$protest_permition[res_selected$protest_permition == 4] <- 2
res_selected$protest_permition[res_selected$protest_permition == 98] <- 3
res_selected$protest_permition[res_selected$protest_permition == 99] <- 3

table(Qaug_selected$protest_permition)
Qaug_selected$protest_permition[Qaug_selected$protest_permition == "Полностью не оправдано"] <- 1
Qaug_selected$protest_permition[Qaug_selected$protest_permition == "Скорее не оправдано"] <- 1
Qaug_selected$protest_permition[Qaug_selected$protest_permition == "Скорее оправданно"] <- 2
Qaug_selected$protest_permition[Qaug_selected$protest_permition == "Совершенно оправданно"] <- 2
Qaug_selected$protest_permition[Qaug_selected$protest_permition == "no_answer"] <- 3

table(Qapr_selected$protest_permition)
Qapr_selected$protest_permition[Qapr_selected$protest_permition == "Полностью не оправдано"] <- 1
Qapr_selected$protest_permition[Qapr_selected$protest_permition == "Скорее не оправдано"] <- 1
Qapr_selected$protest_permition[Qapr_selected$protest_permition == "Совершенно оправданно"] <- 2
Qapr_selected$protest_permition[Qapr_selected$protest_permition == "Отказ от ответа"] <- 3
Qapr_selected$protest_permition[Qapr_selected$protest_permition == "Затрудняюсь ответить"] <- 3

QJuly_selected$protest_permition[QJuly_selected$protest_permition == "Полностью не оправдано"] <- 1
QJuly_selected$protest_permition[QJuly_selected$protest_permition == "Скорее не оправдано"] <- 1
QJuly_selected$protest_permition[QJuly_selected$protest_permition == "Совершенно оправданно"] <- 2
QJuly_selected$protest_permition[QJuly_selected$protest_permition == "Отказ от ответа"] <- 3
QJuly_selected$protest_permition[QJuly_selected$protest_permition == "Затрудняюсь ответить"] <- 3

#Arrest participants
table(res_selected$arrest_participants)
res_selected$arrest_participants[res_selected$arrest_participants == 2] <- 1
res_selected$arrest_participants[res_selected$arrest_participants == 3] <- 2
res_selected$arrest_participants[res_selected$arrest_participants == 4] <- 2
res_selected$arrest_participants[res_selected$arrest_participants == 98] <- 3
res_selected$arrest_participants[res_selected$arrest_participants == 99] <- 3

table(Qaug_selected$arrest_participants)
Qaug_selected$arrest_participants[Qaug_selected$arrest_participants == "Полностью не оправдано"] <- 1
Qaug_selected$arrest_participants[Qaug_selected$arrest_participants == "Скорее не оправдано"] <- 1
Qaug_selected$arrest_participants[Qaug_selected$arrest_participants == "Скорее оправданно"] <- 2
Qaug_selected$arrest_participants[Qaug_selected$arrest_participants == "Совершенно оправданно"] <- 2
Qaug_selected$arrest_participants[Qaug_selected$arrest_participants == "no_answer"] <- 3

table(Qapr_selected$arrest_participants)
Qapr_selected$arrest_participants[Qapr_selected$arrest_participants == "Полностью не оправдано"] <- 1
Qapr_selected$arrest_participants[Qapr_selected$arrest_participants == "Скорее не оправдано"] <- 1
Qapr_selected$arrest_participants[Qapr_selected$arrest_participants == "Совершенно оправданно"] <- 2
Qapr_selected$arrest_participants[Qapr_selected$arrest_participants == "Затрудняюсь ответить"] <- 3
Qapr_selected$arrest_participants[Qapr_selected$arrest_participants == "Отказ от ответа"] <- 3

QJuly_selected$arrest_participants[QJuly_selected$arrest_participants == "Полностью не оправдано"] <- 1
QJuly_selected$arrest_participants[QJuly_selected$arrest_participants == "Скорее не оправдано"] <- 1
QJuly_selected$arrest_participants[QJuly_selected$arrest_participants == "Совершенно оправданно"] <- 2
QJuly_selected$arrest_participants[QJuly_selected$arrest_participants == "Затрудняюсь ответить"] <- 3
QJuly_selected$arrest_participants[QJuly_selected$arrest_participants == "Отказ от ответа"] <- 3

#Arrest organizers
table(res_selected$arrest_organizers)
res_selected$arrest_organizers[res_selected$arrest_organizers == 2] <- 1
res_selected$arrest_organizers[res_selected$arrest_organizers == 3] <- 2
res_selected$arrest_organizers[res_selected$arrest_organizers == 4] <- 2
res_selected$arrest_organizers[res_selected$arrest_organizers == 98] <- 3
res_selected$arrest_organizers[res_selected$arrest_organizers == 99] <- 3

table(Qaug_selected$arrest_organizers)
Qaug_selected$arrest_organizers[Qaug_selected$arrest_organizers == "Полностью не оправдано"] <- 1
Qaug_selected$arrest_organizers[Qaug_selected$arrest_organizers == "Скорее не оправдано"] <- 1
Qaug_selected$arrest_organizers[Qaug_selected$arrest_organizers == "Скорее оправданно"] <- 2
Qaug_selected$arrest_organizers[Qaug_selected$arrest_organizers == "Совершенно оправданно"] <- 2
Qaug_selected$arrest_organizers[Qaug_selected$arrest_organizers == "no_answer"] <- 3

table(Qapr_selected$arrest_organizers) # WRONG QUESTION WORDING IN THE SURVEY APR! 
Qapr_selected$arrest_organizers[Qapr_selected$arrest_organizers == "Полностью не оправдано"] <- 1
Qapr_selected$arrest_organizers[Qapr_selected$arrest_organizers == "Скорее не оправдано"] <- 1
Qapr_selected$arrest_organizers[Qapr_selected$arrest_organizers == "Совершенно оправданно"] <- 2
Qapr_selected$arrest_organizers[Qapr_selected$arrest_organizers == "Отказ от ответа"] <- 3
Qapr_selected$arrest_organizers[Qapr_selected$arrest_organizers == "Затрудняюсь ответить"] <- 3

#Limit webside
table(res_selected$limit_websites)
res_selected$limit_websites[res_selected$limit_websites == 2] <- 1
res_selected$limit_websites[res_selected$limit_websites == 3] <- 2
res_selected$limit_websites[res_selected$limit_websites == 4] <- 2
res_selected$limit_websites[res_selected$limit_websites == 96] <- 3
res_selected$limit_websites[res_selected$limit_websites == 98] <- 3
res_selected$limit_websites[res_selected$limit_websites == 99] <- 3

table(Qaug_selected$limit_websites)
Qaug_selected$limit_websites[Qaug_selected$limit_websites == "Полностью не оправдано"] <- 1
Qaug_selected$limit_websites[Qaug_selected$limit_websites == "Скорее не оправдано"] <- 1
Qaug_selected$limit_websites[Qaug_selected$limit_websites == "Скорее оправданно"] <- 2
Qaug_selected$limit_websites[Qaug_selected$limit_websites == "Совершенно оправданно"] <- 2
Qaug_selected$limit_websites[Qaug_selected$limit_websites == "Не слышали о таком"] <- 3
Qaug_selected$limit_websites[Qaug_selected$limit_websites == "no_answer"] <- 3

table(Qapr_selected$limit_websites)
Qapr_selected$limit_websites[Qapr_selected$limit_websites == "Полностью не оправдано"] <- 1
Qapr_selected$limit_websites[Qapr_selected$limit_websites == "Скорее не оправдано"] <- 1
Qapr_selected$limit_websites[Qapr_selected$limit_websites == "Совершенно оправданно"] <- 2
Qapr_selected$limit_websites[Qapr_selected$limit_websites == "Затрудняюсь ответить"] <- 3
Qapr_selected$limit_websites[Qapr_selected$limit_websites == "Отказ от ответа"] <- 3

#Surveillance
table(res_selected$surveilance)
res_selected$surveilance[res_selected$surveilance == 96] <- 5
res_selected$surveilance[res_selected$surveilance == 98] <- 5
res_selected$surveilance[res_selected$surveilance == 99] <- 5

table(Qaug_selected$surveilance)
Qaug_selected$surveilance[Qaug_selected$surveilance == "no_answer"] <- 5
Qaug_selected$surveilance[Qaug_selected$surveilance == "Не слышали о таком"] <- 5
Qaug_selected$surveilance[Qaug_selected$surveilance == "Полностью не оправдано"] <- 1
Qaug_selected$surveilance[Qaug_selected$surveilance == "Скорее не оправдано"] <- 2
Qaug_selected$surveilance[Qaug_selected$surveilance == "Скорее оправданно"] <- 3
Qaug_selected$surveilance[Qaug_selected$surveilance == "Совершенно оправданно"] <- 4

# Foreign agent
table(res_selected$foreign_agent)
res_selected$foreign_agent[res_selected$foreign_agent == 2] <- 1
res_selected$foreign_agent[res_selected$foreign_agent == 3] <- 2
res_selected$foreign_agent[res_selected$foreign_agent == 4] <- 2
res_selected$foreign_agent[res_selected$foreign_agent == 96] <- 3
res_selected$foreign_agent[res_selected$foreign_agent == 98] <- 3
res_selected$foreign_agent[res_selected$foreign_agent == 99] <- 3

table(Qaug_selected$foreign_agent)
Qaug_selected$foreign_agent[Qaug_selected$foreign_agent == "no_answer"] <- 3
Qaug_selected$foreign_agent[Qaug_selected$foreign_agent == "Не слышали о таком"] <- 3
Qaug_selected$foreign_agent[Qaug_selected$foreign_agent == "Полностью не оправдано"] <- 1
Qaug_selected$foreign_agent[Qaug_selected$foreign_agent == "Скорее не оправдано"] <- 1
Qaug_selected$foreign_agent[Qaug_selected$foreign_agent == "Скорее оправданно"] <- 2
Qaug_selected$foreign_agent[Qaug_selected$foreign_agent == "Совершенно оправданно"] <- 2

table(Qapr_selected$foreign_agent)
Qapr_selected$foreign_agent[Qapr_selected$foreign_agent == "Затрудняюсь ответить"] <- 3
Qapr_selected$foreign_agent[Qapr_selected$foreign_agent == "Отказ от ответа"] <- 3
Qapr_selected$foreign_agent[Qapr_selected$foreign_agent == "Полностью не оправдано"] <- 1
Qapr_selected$foreign_agent[Qapr_selected$foreign_agent == "Скорее не оправдано"] <- 1
Qapr_selected$foreign_agent[Qapr_selected$foreign_agent == "Совершенно оправданно"] <- 2

#Uanthorised protest
table(Lsep_selected$unauth_protest_partic)
Lsep_selected$unauth_protest_partic[Lsep_selected$unauth_protest_partic == 5] <- 4

table(Ljul_selected$unauth_protest_partic)
Ljul_selected$unauth_protest_partic[Ljul_selected$unauth_protest_partic == 97] <- 4
Ljul_selected$unauth_protest_partic[Ljul_selected$unauth_protest_partic == 98] <- 4

table(Qaug_selected$unauth_protest_partic)
Qaug_selected$unauth_protest_partic[Qaug_selected$unauth_protest_partic == "no_answer"] <- 4
Qaug_selected$unauth_protest_partic[Qaug_selected$unauth_protest_partic == "Я считаю этот способ допустимым, и сам(а) участвовал (а)"] <- 1
Qaug_selected$unauth_protest_partic[Qaug_selected$unauth_protest_partic == "Я считаю такой способ допустимым, но сам(а) не участвовал(а)"] <- 2
Qaug_selected$unauth_protest_partic[Qaug_selected$unauth_protest_partic == "Я считаю такой способ недопустимым и сам(а) не участвовал(а)"] <- 3

table(Qfeb_selected$unauth_protest_partic)
Qfeb_selected$unauth_protest_partic[Qfeb_selected$unauth_protest_partic == 5] <- 4

#### Plots #### 


#### Unauthrised protest support by regime support ####
# regime support as putins approval (1 for approve, 2 for disaprove, 3 NA)
# unauthr protest as approv and participate question (vals 1, 2 for approve, 3 disaprove, 4 NA)
# time survey_time as str variable

## standardise putins support var type
library(purrr)
datasets <- list(Ljul_selected, Lsep_selected, Qaug_selected, Qfeb_selected,
                 Qapr_selected, QJuly_selected, res_selected, Q_sept)
datasets <- map(datasets, ~ .x %>%
                  mutate(
                    putin_support = as.character(putin_support),
                    unauth_protest_partic = if("unauth_protest_partic" %in% names(.x)) {
                      as.character(unauth_protest_partic)
                    } else {
                      NA_character_  # Add a column with NA if unauth_protest_partic is not present
                    },
                    protest_permition = if("protest_permition" %in% names(.x)) {
                      as.character(protest_permition)
                    } else {
                      NA_character_  # Add a column with NA if protest_permition is not present
                    },
                    arrest_participants = if("arrest_participants" %in% names(.x)) {
                      as.character(arrest_participants)
                    } else {
                      NA_character_  # Add a column with NA if arrest_participants is not present
                    }
                  ))

Ljul_selected <- datasets[[1]]
Lsep_selected <- datasets[[2]]
Qaug_selected <- datasets[[3]]
Qfeb_selected <- datasets[[4]]
Qapr_selected <- datasets[[5]]
QJuly_selected <- datasets[[6]]
res_selected <- datasets[[7]]
Q_sept_selected <- datasets[[8]]


# law obedience
data_law_obedience <- bind_rows(
  Q_sept_selected,
  Qaug_selected %>% select(survey_time, putin_support, law_obedience),
  Qfeb_selected %>% select(survey_time, putin_support, law_obedience),
  Qapr_selected %>% select(survey_time, putin_support, law_obedience),
  QJuly_selected %>% select(survey_time, putin_support, law_obedience),
  res_selected %>%
    mutate(law_obedience = replace_na(law_obedience, "no_answer"))
  %>% select(survey_time, putin_support, law_obedience),
) %>% 
  mutate(
    putin_support = case_when(
      putin_support == 1 ~ "approve",
      putin_support == 2 ~ "disapprove",
      putin_support == "Одобряю" ~ "approve",
      putin_support == "Не одобряю" ~ "disapprove",
      putin_support == "Одобряю  " ~ "approve",
      putin_support == "Не одобряю  " ~ "disapprove",
      T ~ NA
    ),
    law_obedience = case_when(law_obedience=="Затрудняюсь ответить"~"no_answer",
                              law_obedience=="Отказ от ответа" ~ "no_answer",
                              TRUE~law_obedience),
    survey_time = factor(survey_time, levels = c("July 2020", "September 2020", "December 2021",
                                                 "August 2023", "February 2024", "April 2024", "July 2024",
                                                 "September 2024"))
  ) 

report_shares = data_law_obedience %>%
  group_by(survey_time) %>%
  summarize(
    no_answer_law_obedience = mean(law_obedience == "no_answer", na.rm = TRUE) * 100,
    na_putin_support = mean(is.na(putin_support), na.rm = TRUE) * 100
  )

plot_na_law <- ggplot(report_shares, aes(x = survey_time)) +
  geom_line(aes(y = no_answer_law_obedience, color = "No Answer - Law Obedience", group = 1), size = 1) +
  geom_line(aes(y = na_putin_support, color = "NA - Putin Support", group = 2), size = 1) +
  geom_point(aes(y = no_answer_law_obedience, color = "No Answer - Law Obedience"), size = 3) +
  geom_point(aes(y = na_putin_support, color = "NA - Putin Support"), size = 3) +
  labs(
    title = "Shares of 'No Answer' for Law Obedience and NA for Putin Support Across Survey Times",
    x = "Survey Time",
    y = "Percentage",
    color = "Response Type"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/surveys/no_answer_law.png", plot = plot_na_law, width = 10, height = 6, dpi = 300)


# unique(data_law_obedience$putin_support)
# sum(is.na(data_law_obedience$putin_support))

# sum(is.na(data_law_obedience$law_obedience)) # 729, all from RES
# sum(is.na(data_law_obedience$putin_support)) # 5823

# Calculate the shares
plot_law_data <- data_law_obedience %>%
  group_by(survey_time, putin_support) %>%
  summarize(
    share_always_abide = mean(law_obedience == "always_abide", na.rm = F),
    total = n()
  )

# Plot 1: Share of People Who Approve or Participate in Unauthorized Protests
plot_law <- ggplot(plot_law_data, aes(x = survey_time, y = share_always_abide, color = putin_support, group = putin_support)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Share of People Who Think One Should Always Abide the Law",
    x = "Survey Time",
    y = "Share of Respondents",
    color = "Putin's Approval"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/surveys/share_always_abide_law.png", plot = plot_law, width = 10, height = 6, dpi = 300)




# Unauthr Protest

data_approv_protest <- bind_rows(
  Q_sept,
  Ljul_selected %>% select(survey_time, putin_support, unauth_protest_partic),
  Lsep_selected %>% select(survey_time, putin_support, unauth_protest_partic),
  Qaug_selected %>% select(survey_time, putin_support, unauth_protest_partic),
  Qfeb_selected %>% select(survey_time, putin_support, unauth_protest_partic)
) %>% 
  mutate(
    unauth_protest_approve = case_when(
      unauth_protest_partic %in% c(1, 2) ~ "approve",
      unauth_protest_partic %in% c("Я считаю такой способ допустимым, но сам(а) не участвовал(а)", 
                                   "Я считаю этот способ допустимым, и сам(а) участвовал (а)") ~ "approve",
      unauth_protest_partic == 3 ~ "disapprove",
      unauth_protest_partic == "Я считаю такой способ недопустимым и сам(а) не участвовал(а)" ~ "disapprove",
      TRUE ~ "no_answer"
    ),
    putin_support = case_when(
      putin_support == 1 ~ "approve",
      putin_support == 2 ~ "disapprove",
      putin_support == "Одобряю" ~ "approve",
      putin_support == "Не одобряю" ~ "disapprove",
      putin_support == "Одобряю  " ~ "approve",
      putin_support == "Не одобряю  " ~ "disapprove",
      T ~ NA
    ),
    survey_time = factor(survey_time, levels = c("July 2020", "September 2020", "August 2023", "February 2024",
                                                 "September 2024"))
  ) 

unique(data_approv_protest$unauth_protest_approve)

report_no_answer <- data_approv_protest %>%
  group_by(survey_time) %>%
  summarize(
    no_answer_protest = mean(unauth_protest_approve == "no_answer", na.rm = TRUE) * 100,
    no_answer_putin = mean(is.na(putin_support), na.rm = TRUE) * 100
  )

plot_na_prot_part <- ggplot(report_no_answer, aes(x = survey_time)) +
  geom_line(aes(y = no_answer_protest, color = "No Answer - Protest", group = 1), size = 1) +
  geom_line(aes(y = no_answer_putin, color = "No Answer - Putin", group = 2), size = 1) +
  geom_point(aes(y = no_answer_protest, color = "No Answer - Protest"), size = 3) +
  geom_point(aes(y = no_answer_putin, color = "No Answer - Putin"), size = 3) +
  labs(
    title = "Shares of 'No Answer' for Unauthorized Protest Participation and NA for Putin Support",
    x = "Survey Time",
    y = "Percentage",
    color = "Response Type"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/surveys/na_prot_part.png", plot = plot_na_prot_part, width = 10, height = 6, dpi = 300)



# Calculate the shares
plot1_data <- data_approv_protest %>%
  group_by(survey_time, putin_support) %>%
  summarize(
    share_approve_or_participate = mean(unauth_protest_approve == "approve", na.rm = F),
    total = n()
  )


# Plot 1: Share of People Who Approve or Participate in Unauthorized Protests
plot1 <- ggplot(plot1_data, aes(x = survey_time, y = share_approve_or_participate, color = putin_support, group = putin_support)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Share of People Who Approve or Participate in Unauthorized Protests",
    x = "Survey Time",
    y = "Share of Respondents",
    color = "Putin's Approval"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/surveys/share_approve_participate_unauthorised.png", plot = plot1, width = 10, height = 6, dpi = 300)


data_justified_repressions <- bind_rows(
  Qapr_selected %>% select(survey_time, putin_support, protest_permition, arrest_participants),
  QJuly_selected %>% select(survey_time, putin_support, protest_permition, arrest_participants),
  res_selected %>% select(survey_time, putin_support, protest_permition, arrest_participants),
  Qaug_selected %>% select(survey_time, putin_support, protest_permition, arrest_participants),
) %>% 
  mutate(
    justified_protest_permission = case_when(
      protest_permition == 1 ~ "not justified",
      protest_permition == "Полностью не оправдано" ~ "not justified",
      protest_permition == "Скорее не оправдано" ~ "not justified",
      protest_permition == 2 ~ "justified",
      protest_permition == "Совершенно оправданно" ~ "justified",
      protest_permition == "Скорее оправданно" ~ "justified",
      TRUE ~ "no_answer"
    ),
    justified_arrest_participants = case_when(
      arrest_participants == 1 ~ "not justified",
      arrest_participants == "Полностью не оправдано" ~ "not justified",
      arrest_participants == "Скорее не оправдано" ~ "not justified",
      arrest_participants == 2 ~ "justified",
      arrest_participants == "Совершенно оправданно" ~ "justified",
      arrest_participants == "Скорее оправданно" ~ "justified",
      TRUE ~ "no_answer"
    ),
    putin_support = case_when(
      putin_support == 1 ~ "approve",
      putin_support == 2 ~ "disapprove",
      putin_support == "Одобряю" ~ "approve",
      putin_support == "Не одобряю" ~ "disapprove",
      putin_support == "Одобряю  " ~ "approve",
      putin_support == "Не одобряю  " ~ "disapprove",
      T ~ NA
    ),
    survey_time = factor(survey_time, levels = c("December 2021", "August 2023", "April 2024", "July 2024"))
  ) 


unique(data_justified_repressions$justified_protest_permission)

# NA report
report_no_answer_repressions <- data_justified_repressions %>%
  group_by(survey_time) %>%
  summarize(
    no_answer_protest_permission = mean(justified_protest_permission == "no_answer", na.rm = TRUE) * 100,
    no_answer_arrest_participants = mean(justified_arrest_participants == "no_answer", na.rm = TRUE) * 100,
    no_answer_putin = mean(is.na(putin_support), na.rm = TRUE) * 100
  )

plot_na_perm_arrest <- ggplot(report_no_answer_repressions, aes(x = survey_time)) +
  geom_line(aes(y = no_answer_protest_permission, color = "No Answer - Protest Permission", group = 1), size = 1) +
  geom_line(aes(y = no_answer_arrest_participants, color = "No Answer - Arrest Participants", group = 2), size = 1) +
  geom_line(aes(y = no_answer_putin, color = "No Answer - Putin", group = 3), size = 1) +
  geom_point(aes(y = no_answer_protest_permission, color = "No Answer - Protest Permission"), size = 3) +
  geom_point(aes(y = no_answer_arrest_participants, color = "No Answer - Arrest Participants"), size = 3) +
  geom_point(aes(y = no_answer_putin, color = "No Answer - Putin"), size = 3) +
  labs(
    title = "Shares of 'No Answer' for Justified Protest Permission, Arrest Participants, and NA for Putin Support",
    x = "Survey Time",
    y = "Percentage",
    color = "Response Type"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/surveys/na_permis_arrest.png", plot = plot_na_perm_arrest, width = 10, height = 6, dpi = 300)



# Calculate the shares for protest permission
plot2_data_permission <- data_justified_repressions %>%
  group_by(survey_time, putin_support) %>%
  summarize(
    share_not_justified_permission = mean(justified_protest_permission == "not justified", na.rm = F)
  )


# Calculate the shares for arrest participants
plot3_data_arrest <- data_justified_repressions %>%
  group_by(survey_time, putin_support) %>%
  summarize(
    share_not_justified_arrest = mean(justified_arrest_participants == "not justified", na.rm = F)
  )


# Plot 2: Share of People Who Think Government Repression Requirement Is Not Justified (Protest Permission)
plot2 <- ggplot(plot2_data_permission, aes(x = survey_time, y = share_not_justified_permission, color = putin_support, group = putin_support)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Share of People Who Think Protest Permission Requirement Is Not Justified",
    x = "Survey Time",
    y = "Share of Respondents",
    color = "Putin's Approval"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/surveys/share_protest_permission_unjustified.png", plot = plot2, width = 10, height = 6, dpi = 300)

# Plot 3: Share of People Who Think Government Repressions Are Not Justified (Protest Participants Arrsts)
plot3 <- ggplot(plot3_data_arrest, aes(x = survey_time, y = share_not_justified_arrest, color = putin_support, group = putin_support)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Share of People Who Think Arrests of Protesters Are Not Justified",
    x = "Survey Time",
    y = "Share of Respondents",
    color = "Putin's Approval"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/surveys/share_arrests_unjustified.png", plot = plot3, width = 10, height = 6, dpi = 300)



#### Law obedience ####
table(Lsep_selected$law_obedience)
table(res_selected$law_obedience)
table(Qaug_selected$law_obedience)
table(Qfeb_selected$law_obedience)
table(Qapr_selected$law_obedience)

#line plot
Q_Levada <- round(c(prop.table(table(Lsep_selected$law_obedience))[1]*100, 
                    prop.table(table(Lsep_selected$law_obedience, Lsep_selected$putin_support))[1]*100,
                    prop.table(table(Lsep_selected$law_obedience, Lsep_selected$putin_support))[4]*100), 2)

RES <- round(c(prop.table(table(res_selected$law_obedience))[1]*100, 
               prop.table(table(res_selected$law_obedience, res_selected$putin_support))[1]*100,
               prop.table(table(res_selected$law_obedience, res_selected$putin_support))[4]*100),2)

Q_August <- round(c(prop.table(table(Qaug_selected$law_obedience))[1]*100, 
                    prop.table(table(Qaug_selected$law_obedience,Qaug_selected$putin_support))[1]*100,
                    prop.table(table(Qaug_selected$law_obedience,Qaug_selected$putin_support))[4]*100), 2)

Q_February <- round(c(prop.table(table(Qfeb_selected$law_obedience))[1]*100,
                      prop.table(table(Qfeb_selected$law_obedience, Qfeb_selected$putin_support))[1]*100,
                      prop.table(table(Qfeb_selected$law_obedience, Qfeb_selected$putin_support))[4]*100), 2)    

Q_April <- round(c(prop.table(table(Qapr_selected$law_obedience))[1]*100,
                   prop.table(table(Qapr_selected$law_obedience, Qapr_selected$putin_support))[1]*100,
                   prop.table(table(Qapr_selected$law_obedience, Qapr_selected$putin_support))[4]*100), 2)

obedience_all <- rbind(Q_Levada, RES, Q_August, Q_February, Q_April) 
y <- c (1, 2, 3, 4, 5)
obedience_all_n <- cbind(obedience_all, y)
obedience_all_n <- data.frame(obedience_all_n)
names = c("2020 September", "2021 December", "2023 August", "2024 February", "2024 April")

plot(obedience_all_n$X1 ~ y, type = "b", xlim = c(1, 5), ylim = c(0, 70), 
     xlab = "Survey date", ylab = "Proportion of people who support law obedience", xaxt = "n", axes = FALSE, lwd = 3)
lines(obedience_all_n$V2, type = "b", col = 2, lwd = 3) # Same X values
lines(obedience_all_n$V3, type = "b", col = 3, lwd = 3) # Same X values
axis(1, at = 1:5, labels = names, cex.axis = 1) 
axis(2)
legend("topright", legend = c("Full sample", "Support Putin", 'Do not support Putin'), lty = 1, col = 1:3)

### Authorized protest approve
table(Ljul_selected$approved_protest_partic)
table(Lsep_selected$approved_protest_partic)
table(Qaug_selected$approved_protest_partic)
table(Qfeb_selected$approved_protest_partic)

L_july <- round(c(prop.table(table(Ljul_selected$approved_protest_partic))[1]*100,
                  prop.table(table(Ljul_selected$approved_protest_partic, Ljul_selected$putin_support))[1]*100,
                  prop.table(table(Ljul_selected$approved_protest_partic, Ljul_selected$putin_support))[4]*100), 2)

L_sep <- round(c(prop.table(table(Lsep_selected$approved_protest_partic))[1]*100,
                 prop.table(table(Lsep_selected$approved_protest_partic, Lsep_selected$putin_support))[1]*100,
                 prop.table(table(Lsep_selected$approved_protest_partic, Lsep_selected$putin_support))[4]*100), 2)

Q_August <- round(c(prop.table(table(Qaug_selected$approved_protest_partic))[1]*100,
                    prop.table(table(Qaug_selected$approved_protest_partic, Qaug_selected$putin_support))[1]*100,
                    prop.table(table(Qaug_selected$approved_protest_partic, Qaug_selected$putin_support))[4]*100), 2)

Q_February <- round(c(prop.table(table(Qfeb_selected$approved_protest_partic))[1]*100,
                      prop.table(table(Qfeb_selected$approved_protest_partic, Qfeb_selected$putin_support))[1]*100,
                      prop.table(table(Qfeb_selected$approved_protest_partic, Qfeb_selected$putin_support))[4]*100), 2)
###Line plot 
approved_prot <- rbind(L_july, L_sep, Q_August, Q_February)
waves <- c (1, 2, 3, 4)
approved_prot <- cbind(approved_prot, waves)
approved_prot <- data.frame(approved_prot)
names = c("2020 September", "2020 July", "2023 August", "2024 February")
plot(approved_prot$X1 ~ waves, type = "b", xlim = c(1, 5), ylim = c(0, 30), 
     xlab = "Survey date", ylab = "Proportion of people who approve authorised protests", xaxt = "n", axes = FALSE, lwd = 3)
lines(approved_prot$V2, type = "b", col = 2, lwd = 3) # Same X values
lines(approved_prot$V3, type = "b", col = 3, lwd = 3) # Same X values
axis(1, at = 1:4, labels = names, cex.axis = 1) 
axis(2)
legend("topright", legend = c("Full sample", "Support Putin", 'Do not support Putin'), lty = 1, col = 1:3)

### UnAuthorised protest approve
table(Ljul_selected$unauth_protest_partic)
table(Lsep_selected$unauth_protest_partic)
table(Qaug_selected$unauth_protest_partic)
table(Qfeb_selected$unauth_protest_partic)

L_july <- round(c(prop.table(table(Ljul_selected$unauth_protest_partic))[1]*100,
                  prop.table(table(Ljul_selected$unauth_protest_partic, Ljul_selected$putin_support))[1]*100,
                  prop.table(table(Ljul_selected$unauth_protest_partic, Ljul_selected$putin_support))[4]*100), 2)

L_sep <- round(c(prop.table(table(Lsep_selected$unauth_protest_partic))[1]*100,
                 prop.table(table(Lsep_selected$unauth_protest_partic, Lsep_selected$putin_support))[1]*100,
                 prop.table(table(Lsep_selected$unauth_protest_partic, Lsep_selected$putin_support))[4]*100), 2)

Q_August <- round(c(prop.table(table(Qaug_selected$unauth_protest_partic))[1]*100,
                    prop.table(table(Qaug_selected$unauth_protest_partic, Qaug_selected$putin_support))[1]*100,
                    prop.table(table(Qaug_selected$unauth_protest_partic, Qaug_selected$putin_support))[4]*100), 2)

Q_February <- round(c(prop.table(table(Qfeb_selected$unauth_protest_partic))[1]*100,
                      prop.table(table(Qfeb_selected$unauth_protest_partic, Qfeb_selected$putin_support))[1]*100,
                      prop.table(table(Qfeb_selected$unauth_protest_partic, Qfeb_selected$putin_support))[4]*100), 2)

unauth_prot <- rbind(L_july, L_sep, Q_August, Q_February)
waves <- c (1, 2, 3, 4)
unauth_prot <- cbind(unauth_prot, waves)
unauth_prot <- data.frame(unauth_prot)
names = c("2020 September", "2020 July", "2023 August", "2024 February")
plot(unauth_prot$X1 ~ waves, type = "b", xlim = c(1, 5), ylim = c(0, 20), 
     xlab = "Survey date", ylab = "Proportion of people who support unauthorised protests", xaxt = "n", axes = FALSE, lwd = 3)
lines(approved_prot$V2, type = "b", col = 2, lwd = 3) # Same X values
lines(approved_prot$V3, type = "b", col = 3, lwd = 3) # Same X values
axis(1, at = 1:4, labels = names, cex.axis = 1) 
axis(2)
legend("topright", legend = c("Full sample", "Support Putin", 'Do not support Putin'), lty = 1, col = 1:3)

#Approve protest permition
table(res_selected$protest_permition)
table(Qaug_selected$protest_permition)
table(Qapr_selected$protest_permition)

RES <- round(c(prop.table(table(res_selected$protest_permition))[1]*100,
              prop.table(table(res_selected$protest_permition, res_selected$putin_support))[1]*100,
              prop.table(table(res_selected$protest_permition, res_selected$putin_support))[4]*100), 2)

Q_August <- round(c(prop.table(table(Qaug_selected$protest_permition))[1]*100,
                    prop.table(table(Qaug_selected$protest_permition, Qaug_selected$putin_support))[1]*100,              
                    prop.table(table(Qaug_selected$protest_permition, Qaug_selected$putin_support))[4]*100), 2)       

Q_April <- round(c(prop.table(table(Qapr_selected$protest_permition))[1]*100,
                   prop.table(table(Qapr_selected$protest_permition, Qapr_selected$putin_support))[1]*100,             
                   prop.table(table(Qapr_selected$protest_permition, Qapr_selected$putin_support))[4]*100), 2)

protest_permition <- cbind(RES, Q_August, Q_April) 
protest_permition <- data.frame(protest_permition)
protest_permition <- as.matrix(protest_permition)
type = c("Full sample", "Support Putin", "Do not support Putin")
names = c("2021 December", "2023 August", "2024 April")
barplot(protest_permition ~ type, beside = T, ylim = c(0, 70),
        legend.text = names,
        args.legend = list(cex = .7),
        xlab="")


#### Merging data set
merged_data <- rbind.fill(Ljul_selected, Lsep_selected, res_selected, Qaug_selected, Qfeb_selected, Qapr_selected)
names(merged_data)
str(merged_data)
merged_data <- as.data.frame(sapply(merged_data, haven::zap_label))
#write_xlsx(merged_data, "/Users/emitrokhina/Library/CloudStorage/OneDrive-SharedLibraries-Nexus365/Katerina\ Tertytchnaya\ -\ Qualtrics/Survey_comparison/merged_surveys.xlsx", col_names = TRUE, format_headers = TRUE)


#### Analysis ####
merged_data$putin_support <- as.numeric(merged_data$putin_support)
merged_data$vote_next <- as.numeric(merged_data$vote_next)
merged_data$law_obedience <- as.numeric(merged_data$law_obedience)
merged_data$gender <- as.numeric(merged_data$gender)
merged_data$age <- as.numeric(merged_data$age)
merged_data$educaction <- as.numeric(merged_data$educaction)
merged_data$income <- as.numeric(merged_data$income)
merged_data$approved_protest_partic <- as.numeric(merged_data$approved_protest_partic)
merged_data$unauth_protest_partic <- as.numeric(merged_data$unauth_protest_partic)
merged_data$survey_indicator <-  as.numeric(merged_data$survey_indicator)
merged_data$protest_permition <- as.numeric(merged_data$protest_permition)
merged_data$arrest_participants <- as.numeric(merged_data$arrest_participants)
merged_data$arrest_organizers <- as.numeric(merged_data$arrest_organizers)
merged_data$limit_websites <- as.numeric(merged_data$limit_websites)
merged_data$surveilance <- as.numeric(merged_data$surveilance)
merged_data$foreign_agent <- as.numeric(merged_data$foreign_agent)


cor.test(merged_data$putin_support, merged_data$vote_next, use="complete.obs", method = "pearson")


m1 <- lm(law_obedience ~ as.factor(survey_indicator) + gender + age + educaction + income + putin_support + vote_next, data = merged_data)
m2 <- lm(protest_permition ~ survey_indicator + gender + age + educaction + income + putin_support + vote_next, data = merged_data)
m3 <- lm(arrest_participants ~ survey_indicator + gender + age + educaction + income + putin_support + vote_next, data = merged_data)
m4 <- lm(arrest_organizers ~ survey_indicator + gender + age + educaction + income + putin_support + vote_next, data = merged_data)
m5 <- lm(limit_websites ~ survey_indicator + gender + age + educaction + income + putin_support + vote_next, data = merged_data)
m6 <- lm(surveilance ~ survey_indicator + gender + age + educaction + income + putin_support + vote_next, data = merged_data)
m7 <- lm(foreign_agent ~ survey_indicator + gender + age + educaction + income + putin_support + vote_next, data = merged_data)
m8 <- lm(unauth_protest_partic ~ survey_indicator + gender + age + educaction + income + putin_support + vote_next, data = merged_data)


table(merged_data$arrest_participants, merged_data$survey_indicator)


summary(lm(unauth_protest_partic ~ survey_indicator + gender + age + educaction + income + putin_support + vote_next, data = merged_data))







ggcorrmat(
  data = merged_data[, c("putin_support", "vote_next")],
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("white", "steelblue") # change default colors
)


merged_data <-  as.numeric(merged_data)

law_obedience <- as.numeric(merged_data$law_obedience)
class(merged_data$law_obedience)



class(merged_data$law_obedience)
merged_data$law_obedience
table(haven::zap_label(merged_data$law_obedience))

                            library(corrplot)


cor.test(as.numeric(merged_data$putin_support), as.numeric(merged_data$vote_next), use="complete.obs", method = "pearson")

library(ggstatsplot)




m1 <- lm(as.numeric(law_obedience) ~ as.factor(survey_indicator) + gender + age + educaction + income + putin_support + vote_next, data = merged_data)

class(merged_data$law_obedience)
merged_data$law_obedience[!is.na(merged_data$law_obedience)]


corrplot(cor(as.numeric(merged_data$putin_support), as.numeric(merged_data$vote_next)),
         method = "number",
         type = "upper" # show only upper side
)

table(merged_data$putin_support)
table(merged_data$vote_next)


 


#Unclear coding
table(merged_data$region)
table(merged_data$settlement)

#Clear coding
table(merged_data$age)
table(merged_data$income)
table(merged_data$educaction)
table(merged_data$vote_next)
table(merged_data$gender)
table(merged_data$approved_protest_partic)

























#### Analysis




