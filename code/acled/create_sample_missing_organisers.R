#### random NA's sample ####

pacman::p_load(rio, tidyverse, readr, broom, 
               lubridate, gt, gtsummary, survey, readxl,
               gridExtra, knitr, haven, stargazer, nnet, ggeffects)

acled <- import("C:/Users/murrn/GitHub/nonviolent-repression/data/acled_processed_data/acled_with_dvs_and_controls.csv") 

## select just missing org observations and subset into war/pre_war
acled_missing_orgs <- acled %>% 
  filter(org_indicator == "")

pre_war_subset <- acled_missing_orgs %>% 
  filter(post_invasion == "0")
post_war_subset <- acled_missing_orgs %>% 
  filter(post_invasion == "1")

# pre war n: 2395
# post war n: 840


## sample and combine
set.seed(42)
prewar_sample <- slice_sample(pre_war_subset, n=100, replace = FALSE)
postwar_sample <- slice_sample(post_war_subset, n=100, replace = FALSE)
head(postwar_sample$`Unnamed: 0`, 5)

final_sample <- bind_rows(prewar_sample, postwar_sample)
write_csv(final_sample, "C:/Users/murrn/GitHub/nonviolent-repression/data/acled_processed_data/sample_missing_organisers.csv")
