##### Script adding further variables of interest to ACLED #####
## Variables are: Pre-post invasion dummy, Federal Election month, 
## Police violence during protest
## Some further data cleaning is applied


pacman::p_load(rio, tidyverse, readr, broom, 
               lubridate, gt, gtsummary, survey, readxl,
               gridExtra, knitr, haven, stargazer, nnet, ggeffects)

DATA_CODES = "https://cdn.githubraw.com/antndlcrx/nonviolent-repression/main/data/acled_raw_data/region_codes.xlsx"

acled <- import("C:/Users/murrn/GitHub/nonviolent-repression/data/acled_processed_data/acled_with_org_indicator.csv") 

codes = data <- openxlsx::read.xlsx(DATA_CODES, sheet = "Sheet1")
codes <- codes %>%
  mutate(region_name_en = str_trim(region_name_en))


## unique observations 

length(unique(acled$notes))
# Finding duplicate observations in 'variable'
duplicates <- duplicated(acled$notes)

# Getting the row IDs of these duplicates
row_ids <- which(duplicates)

# Print events
acled$notes[row_ids]
acled_clean <- acled[!duplicated(acled$event_id_cnty), ]

# Convert date column to Date object and create DVs:
# pre/post invasion
# election_month
acled_clean <- acled_clean %>% 
  mutate(date = ymd(event_date),
         pred_labels = recode(pred_labels,
                              "war (pro)" = "war_pro",
                              "war (anti)" = "war_anti"),
         month_year = format(date, "%Y-%m"),
         post_invasion = factor(case_when(
           date >= as.Date("2022-02-24") ~ 1,
           TRUE ~ 0)),
         police_violence = factor(case_when(
           sub_event_type %in% c("Protest with intervention", "Excessive force against protesters") ~ 1,
           TRUE ~ 0
         )),
         election_month = factor(case_when(
           month_year %in% c("2021-09", "2018-03", "2024-03", "2020-06", "2020-07") ~ 1,
           TRUE ~ 0))
  )


mapping <- c(
  'Saint Petersburg' = 'The City of Saint-Petersburg',
  'Sverdlovsk' = 'Sverdlovsk region',
  'Moscow' = 'The City of Moscow',
  'Republic of Tatarstan' = 'Republic of Tatarstan',
  'Novosibirsk' = 'Novosibirsk region',
  'Novgorod' = 'Novgorod region',
  'Samara' = 'Samara region',
  'Republic of Mari El' = 'Republic of Mariy El',
  'Republic of Dagestan' = 'Republic of Daghestan',
  'Tomsk' = 'Tomsk region',
  'Republic of Karachay-Cherkessia' = 'Karachaev-Circassian Republic',
  'Moscow Oblast' = 'Moscow region',
  'Volgograd' = 'Volgograd region',
  'Chelyabinsk' = 'Chelyabinsk region',
  'Kaliningrad' = 'Kaliningrad region',
  'Khabarovsk' = 'Khabarovsk territory',
  'Republic of Khakassia' = 'Republic of Khakassia',
  'Perm' = 'Perm region',
  'Vologda' = 'Vologda region',
  'Republic of Sakha' = 'Republic of Sakha (Yakutia)',
  'Tula' = 'Tula region',
  'Kemerovo' = 'Kemerovo region',
  'Altai' = 'Altai territory',
  'Republic of Bashkortostan' = 'Republic of Bashkortastan',
  'Penza' = 'Penza region',
  'Republic of Chuvash' = 'Chuvash Republic',
  'Krasnoyarsk' = 'Krasnoyarsk territory',
  'Orenburg' = 'Orenburg region',
  'Tyumen' = 'Tyumen region',
  'Republic of Kabardino-Balkaria' = 'Kabardian-Balkar Republic',
  'Kostroma' = 'Kostroma region',
  'Vladimir' = 'Vladimir region',
  'Arkhangelsk' = 'Arkhangelsk region',
  'Rostov' = 'Rostov region',
  'Krasnodar' = 'Krasnodar territory',
  'Khanty-Mansi' = 'Khanty-Mansi autonomous',
  'Kaluga' = 'Kaluga region',
  'Republic of Karelia' = 'Republic of Karelia',
  'Voronezh' = 'Voronezh region',
  'Ryazan' = 'Ryazan region',
  'Zabaykalskiy' = 'Chita region',
  'Ulyanovsk' = 'Ulyanovsk region',
  'Republic of Komi' = 'Republic of Komi',
  'Republic of Buryatia' = 'Republic of Buryatia',
  'Nizhny Novgorod' = 'Nizhny novgorod region',
  'Ivanovo' = 'Ivanovo region',
  'Saratov' = 'Saratov region',
  'Irkutsk' = 'Irkutsk region',
  'Republic of Altai' = 'Republic of Altai',
  'Primorskiy' = 'Primorsky territory',
  'Republic of Chechnya' = 'Chechen Republic',
  'Belgorod' = 'Belgorod region',
  'Lipetsk' = 'Lipetsk region',
  'Udmurt Republic' = 'Udmurt Republic',
  'Astrakhan' = 'Astrakhan region',
  'Kirov' = 'Kirov region',
  'Yaroslavl' = 'Yaroslavl region',
  'Kursk' = 'Kursk region',
  'Omsk' = 'Omsk region',
  'Republic of Mordovia' = 'Republic of Mordovia',
  'Tambov' = 'Tambov region',
  'Smolensk' = 'Smolensk region',
  'Republic of Ingushetia' = 'Republic of Ingushetia',
  'Oryol' = 'Oryol region',
  'Pskov' = 'Pskov region',
  'Republic of Tuva' = 'Republic of Tyva',
  'Republic of North Ossetia-Alania' = 'Republic of North Ossetia -Alania',
  'Jewish Autonomous Oblast' = 'Jewish autonomous region',
  'Kamchatka' = 'Kamchatka region',
  'Magadan' = 'Magadan region',
  'Republic of Kalmykia' = 'Republic of Kalmykia',
  'Yamalo-Nenets' = 'Yamalo-Nenets autonomous',
  'Bryansk' = 'Bryansk region',
  'Stavropol' = 'Stavropol territory',
  'Tver' = 'Tver region',
  'Murmansk' = 'Murmansk region',
  'Republic of Adygea' = 'Republic of Adygeya',
  'Amur' = 'Amur region',
  'Sakhalin' = 'Sakhalin region',
  'Leningrad' = 'Leningrad region',
  'Chukotka' = 'Chukotka autonomous area',
  'Nenets' = 'Nenets autonomous area',
  'Kurgan' = 'Kurgan region'
)


# apply mapping
acled_clean$federal_subject <- mapping[acled_clean$admin1]

acled_clean <- acled_clean %>%
  left_join(codes, by = c("federal_subject" = "region_name_en"))

# write_csv(acled_clean, "C:/Users/murrn/GitHub/nonviolent-repression/data/acled_processed_data/acled_with_dvs_and_controls_09_07_2024.csv")
write_excel_csv(acled_clean, "C:/Users/murrn/GitHub/nonviolent-repression/data/acled_processed_data/acled_with_dvs_and_controls_09_07_2024_utf8byte.csv")
