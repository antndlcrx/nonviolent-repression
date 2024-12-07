#### test ####
pacman::p_load(rio, tidyverse, readr, broom, 
               lubridate, gt, gtsummary, survey, readxl,
               gridExtra, knitr, haven, stargazer, nnet, ggeffects)

acled1 <- read_csv("C:/Users/murrn/GitHub/nonviolent-repression/data/acled_processed_data/acled_with_org_indicator.csv")
acled2 <- read_csv("C:/Users/murrn/GitHub/nonviolent-repression/data/acled_processed_data/acled_deberta_preds_05_02_2024.csv")
acled3 <- read_csv("C:/Users/murrn/GitHub/nonviolent-repression/data/acled_processed_data/acled_deberta_preds_17_06_2024.csv")
acled_final <- read_csv("C:/Users/murrn/GitHub/nonviolent-repression/data/acled_processed_data/acled_with_dvs_and_controls_18_06_2024.csv")


acled1 <- acled1 %>% mutate(
       pred_labels = recode(pred_labels,
                            "war (pro)" = "war_pro",
                            "war (anti)" = "war_anti")
       )

acled1 <- acled1[!duplicated(acled1$notes), ]

acled2 <- acled2 %>% mutate(
  pred_labels = recode(pred_labels,
                       "war (pro)" = "war_pro",
                       "war (anti)" = "war_anti")
)
acled2 <- acled2[!duplicated(acled2$notes), ]

acled3 <- acled3 %>% mutate(
  pred_labels = recode(pred_labels,
                       "war (pro)" = "war_pro",
                       "war (anti)" = "war_anti")
)
acled3 <- acled3[!duplicated(acled3$notes), ]


pred_labels_acled1 <- acled1$pred_labels
pred_labels_acled2 <- acled2$pred_labels
pred_labels_acled3 <- acled3$pred_labels
pred_labels_acled_fin <- acled_final$pred_labels

# Calculate differences between acled1 and acled2
diff_acled1_acled2 <- sum(pred_labels_acled1 != pred_labels_acled2)

# Calculate differences between acled1 and acled3
diff_acled1_acled3 <- sum(pred_labels_acled1 != pred_labels_acled3)

# Calculate differences between acled2 and acled3
diff_acled2_acled3 <- sum(pred_labels_acled2 != pred_labels_acled3)

# Calculate differences between acled_fin and acled3
diff_acled_fin_acled3 <- sum(pred_labels_acled_fin != pred_labels_acled3)

# Calculate differences between acled_fin and acled3
diff_acled_fin_acled2 <- sum(pred_labels_acled_fin != pred_labels_acled2)

# Print the results
cat("Differences between acled1 and acled2:", diff_acled1_acled2, "\n")
cat("Differences between acled1 and acled3:", diff_acled1_acled3, "\n")
cat("Differences between acled2 and acled3:", diff_acled2_acled3, "\n")
cat("Differences between acled_final and acled3:", diff_acled_fin_acled3, "\n")
cat("Differences between acled_final and acled2:", diff_acled_fin_acled2, "\n")