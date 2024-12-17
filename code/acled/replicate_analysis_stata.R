pacman::p_load(tidyverse, rio, ggplot2,
               sandwich, lmtest, lubridate, margins, stargazer,
               sjPlot, interplot, ggeffects)


data <- read_csv("C:/Users/murrn/GitHub/nonviolent-repression/data/acled_processed_data/acled_with_dvs_and_controls_16_07_2024_utf8byte.csv")


# Identify duplicates
duplicates <- data %>%
  group_by(event_id_cnty) %>%
  filter(n() > 1)

if (nrow(duplicates) > 0) {
  print("Duplicate rows found:")
  print(duplicates)
} else {
  print("No duplicate rows found.")
}

# Tabulate `pred_labels`
table(data$pred_labels)


# create pol expand and topic relevel
data = data %>% 
  mutate(pol_expand = case_when(pred_labels %in% c("political", "war_anti", "war_pro") ~ 1,
                                T ~ 0),
         pol_expand2 = case_when(pred_labels %in% c("political", "war_anti") ~ 1,
                                 T ~ 0)) %>% 
  mutate(topics_recoded = relevel(as.factor(case_when(pol_expand == 1 ~ "0",
                                                      pred_labels %in% c("cultural", "legal") ~ "1",
                                                      pred_labels == "environmental" ~ "2",
                                                      pred_labels %in% c("economic", "social") ~ "3")),
                                  ref="0"),
          topics_recoded2 = relevel(as.factor(case_when(pol_expand == 1 ~ "0",
                                    pred_labels %in% c("cultural", "legal") ~ "1",
                                    pred_labels == "environmental" ~ "2",
                                    pred_labels == "economic" ~ "3",
                                    pred_labels == "social" ~ "4",
                                    )),
                                  ref="0"),
          topics_recoded3 = relevel(as.factor(case_when(pol_expand == 1 ~ "0",
                                                        pred_labels %in% c("cultural", "legal", "environmental") ~ "1",
                                                        pred_labels  %in% c("economic", "social") ~ "2"
          )),
          ref="0"),
         )


table(data$topics_recoded)


data <- data %>%
  mutate(
    auth_rec = case_when(
      authorized == 0 ~ 0,
      authorized %in% 1:2 ~ 1,
      TRUE ~ NA_real_
    ),
    unauth_rec = case_when(
      unauthorized == 0 ~ 0,
      unauthorized %in% 1:2 ~ 1,
      TRUE ~ NA_real_
    )
  )

table(data$org_indicator)
sum(is.na(data$org_indicator))

data = data %>% 
  mutate(org_c1 = case_when(org_indicator=="other" ~ 1,
                            T ~ 0)
         )

### do authorities remain more tolerant of non-polit vs polit ####
model <- lm(police_violence ~ topics_recoded * post_invasion + auth_rec + org_c1 +
              factor(reg_code) + factor(year) + election_month, data = data)

# Calculate robust standard errors
robust_se <- vcovHC(model, type = "HC1")

# Extract coefficients and robust standard errors
robust_results <- coeftest(model, vcov = robust_se)
robust_coef <- robust_results[, 1]  # Coefficients
robust_se_values <- robust_results[, 2]  # Robust standard errors

stargazer(
  model,
  # type = "text",
  se = list(robust_se_values), # Replace standard errors with robust SEs
  omit = c("factor\\(reg_code\\)", "factor\\(year\\)"), # Omit factor variables
  omit.labels = c("Region Code", "Year"),              # Optional: Custom labels for omitted variables
  keep.stat = c("n", "rsq", "adj.rsq"),                # Show sample size, R², adjusted R²
  dep.var.labels = "Police Violence"
  #covariate.labels = c("Topics * Post-Invasion", "Auth Rec", "Org C1", "Election Month") # Rename predictors
)



plot1 = plot_model(model, type = "pred", terms = c("topics_recoded", "post_invasion"),
           vcov.fun = robust_se)+
  scale_color_manual(values = c("royalblue", "tomato"),
                     labels = c("Before Invasion", "After Invasion"),
                     name = "") + 
  xlab("Protest Type") + 
  ylab("Probability of a Protest facing Police Violence") + 
  scale_x_continuous(
                     labels = c("Political", "Cultural & Legal",
                                "Environmental", "Social & Economic")) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw() + ggtitle(NULL)

ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/acled/protest_type_post_inv.png",
       plot = plot1, width = 10, height = 6, dpi = 300)


#### misssing orgs ####

data = data %>% 
  mutate(missing_org = case_when(is.na(org_indicator) ~ 1,
                                 T ~ 0))


model2 <- lm(missing_org ~ factor(year) + factor(reg_code) + factor(topics_recoded2), data = data)

stargazer(
  model2,
  #type = "text",
  omit = c("factor\\(reg_code\\)", "factor\\(year\\)"), # Omit factor variables
  omit.labels = c("Region Code", "Year"),              # Optional: Custom labels for omitted variables
  keep.stat = c("n", "rsq", "adj.rsq")                # Show sample size, R², adjusted R²
  #dep.var.labels = "Police Violence"
  #covariate.labels = c("Topics * Post-Invasion", "Auth Rec", "Org C1", "Election Month") # Rename predictors
)



#### Analysis for PSA ####
data <- data %>%
  mutate(
    time_indicator = case_when(
      # Group 0: Dates up to February 23, 2022
      date %in% as.Date(c("2022-02-14", "2022-02-15", "2022-02-16", "2022-02-17",
                          "2022-02-18", "2022-02-19", "2022-02-20", "2022-02-21",
                          "2022-02-22", "2022-02-23")) ~ 0,
      
      # Group 1: Dates from February 24 to March 3, 2022
      date %in% as.Date(c("2022-02-24", "2022-02-25", "2022-02-26", "2022-02-27",
                          "2022-02-28", "2022-03-01", "2022-03-02", "2022-03-03")) ~ 1,
      
      # Group 2: Dates from March 4 to March 20, 2022
      date %in% as.Date(c("2022-03-04", "2022-03-05", "2022-03-06", "2022-03-07",
                          "2022-03-08", "2022-03-09", "2022-03-10", "2022-03-11",
                          "2022-03-12", "2022-03-13", "2022-03-14", "2022-03-15",
                          "2022-03-16", "2022-03-17", "2022-03-18", "2022-03-19",
                          "2022-03-20")) ~ 2,
      
      # Default case (optional)
      TRUE ~ NA_real_
    )
  )


filtered_data <- data %>% filter(pro_kremlin_indicator == 0)
data_no_moscow <- filtered_data %>% filter(federal_subject != "The City of Moscow")

model_psa1 <- lm(police_violence ~ factor(pol_expand) * factor(year) + auth_rec + org_c1 + 
              election_month + factor(reg_code), data = filtered_data)

# Calculate robust standard errors
robust_se <- vcovHC(model_psa1, type = "HC1")
# Extract coefficients and robust standard errors
robust_results <- coeftest(model_psa1, vcov = robust_se)
robust_coef <- robust_results[, 1]  # Coefficients
robust_se_values <- robust_results[, 2]  # Robust standard errors

stargazer(
  model_psa1,
  type = "text",
  se = list(robust_se_values), # Replace standard errors with robust SEs
  omit = c("factor\\(reg_code\\)"), # Omit factor variables
  omit.labels = c("Federal Region FE"),              # Optional: Custom labels for omitted variables
  keep.stat = c("n", "rsq", "adj.rsq"),                # Show sample size, R², adjusted R²
  dep.var.labels = "Police Violence",
  covariate.labels = c("Political Protest", "Year 2019", "Year 2020", "Year 2021","Year 2022","Year 2023",
  "Protest authorised", "Organisers", "Election Month", "Political x 2019", 
  "Political x 2020", "Political x 2021", "Political x 2022", "Political x 2023")
)



## plot
plot2 = plot_model(model_psa1, type = "pred", terms = c("year", "pol_expand"
                                                ),
           vcov.fun = robust_se) + theme_bw()+
  scale_color_manual(values = c("darkgrey", "black"),
                     labels = c("Non-Political", "Political"),
                     name = "")+
  xlab("")+
  ylab("Predicted Probability")+ ggtitle(NULL)

ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/acled/polit_protest_years.png",
       plot = plot2, width = 10, height = 6, dpi = 300)

ggpredict(model_psa1, terms = c("year", "pol_expand"))


## logit for robustness ## 
model_psa1_logit <- glm(police_violence ~ factor(pol_expand) * factor(year) + auth_rec + org_c1 + 
                    election_month + factor(reg_code), 
                  data = filtered_data, 
                  family = binomial(link = "logit"))

# Calculate robust standard errors
robust_se_logit <- vcovHC(model_psa1_logit, type = "HC1")  # Robust covariance matrix
robust_results_logits <- coeftest(model_psa1_logit, vcov = robust_se_logit)  # Coefficients with robust SE
robust_coef_logits <- robust_results_logits[, 1]  # Coefficients
robust_se_values_logits <- robust_results_logits[, 2]  # Robust standard errors

## same model but no Moscow 
model_psa1_no_moscow <- lm(police_violence ~ factor(pol_expand) * factor(year) + auth_rec + org_c1 + 
                   election_month + factor(reg_code), data = data_no_moscow)

# Calculate robust standard errors
robust_se_no_moscow <- vcovHC(model_psa1_no_moscow, type = "HC1")
# Extract coefficients and robust standard errors
robust_results_no_moscow <- coeftest(model_psa1_no_moscow, vcov = robust_se_no_moscow)
robust_coef_no_moscow <- robust_results_no_moscow[, 1]  # Coefficients
robust_se_values_no_moscow <- robust_results_no_moscow[, 2]  # Robust standard errors

stargazer(model_psa1,
          model_psa1_logit,
          model_psa1_no_moscow,
          #type = "text",
          se = list(robust_se_values, robust_se_values_logits, robust_se_values_no_moscow), # Replace standard errors with robust SEs
          omit = c("factor\\(reg_code\\)"), # Omit factor variables
          omit.labels = c("Federal Region FE"),              # Optional: Custom labels for omitted variables
          keep.stat = c("n", "aic", "ll"),                   # For logistic models, show sample size, AIC, log-likelihood
          dep.var.labels = "Police Violence (Logit)",
          covariate.labels = c("Political Protest", "Year 2019", "Year 2020", "Year 2021","Year 2022","Year 2023",
                               "Protest authorised", "Organisers", "Election Month", "Political x 2019", 
                               "Political x 2020", "Political x 2021", "Political x 2022", "Political x 2023")
)


## check if robust to alternative ##
model_psa2 <- lm(police_violence ~ factor(pol_expand2) * factor(year) + auth_rec + org_c1 + 
                   election_month + factor(reg_code), data = filtered_data)

# Calculate robust standard errors
robust_se <- vcovHC(model_psa2, type = "HC1")
# Extract coefficients and robust standard errors
robust_results <- coeftest(model_psa2, vcov = robust_se)
robust_coef <- robust_results[, 1]  # Coefficients
robust_se_values <- robust_results[, 2]  # Robust standard errors

stargazer(
  model_psa2,
  #type = "text",
  se = list(robust_se_values), # Replace standard errors with robust SEs
  omit = c("factor\\(reg_code\\)"), # Omit factor variables
  omit.labels = c("Region Code"),              # Optional: Custom labels for omitted variables
  keep.stat = c("n", "rsq", "adj.rsq"),                # Show sample size, R², adjusted R²
  dep.var.labels = "Police Violence",
  covariate.labels = c("Political Protest", "Year 2019", "Year 2020", "Year 2021","Year 2022","Year 2023",
                       "Protest authorised", "Organizers", "Election Month", "Political x 2019", 
                       "Political x 2020", "Political x 2021", "Political x 2022", "Political x 2023"))

plot3 = plot_model(model_psa2, type = "pred", terms = c("pol_expand2",
                                                "year"),
           vcov.fun = robust_se) + theme_bw()+
  scale_x_continuous(breaks = c(0, 1),
                     labels = c("Political (war anti)",
                                "Other")) + 
  xlab("")+
  ylab("Probability of a Protest facing Police Violence") + ggtitle(NULL)

ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/acled/polit_protest_years_no_pro_war.png",
       plot = plot3, width = 10, height = 6, dpi = 300)


## extensions with smaller windows/pre/post invasion & law ##
model_psa3 <- lm(police_violence ~ factor(time_indicator) + pol_expand + org_c1 + 
              factor(reg_code), data = filtered_data)

# Calculate robust standard errors
robust_se <- vcovHC(model_psa3, type = "HC1")
robust_results <- coeftest(model_psa3, vcov = robust_se)
robust_coef <- robust_results[, 1]  # Coefficients
robust_se_values <- robust_results[, 2]  # Robust standard errors

stargazer(
  model_psa3,
  #type = "text",
  se = list(robust_se_values), # Replace standard errors with robust SEs
  omit = c("factor\\(reg_code\\)"), # Omit factor variables
  omit.labels = c("Region Code"),              # Optional: Custom labels for omitted variables
  keep.stat = c("n", "rsq", "adj.rsq"),                # Show sample size, R², adjusted R²
  dep.var.labels = "Police Violence"
  #covariate.labels = c("Topics * Post-Invasion", "Auth Rec", "Org C1", "Election Month") # Rename predictors
)


model_psa4 <- lm(police_violence ~ factor(time_indicator) * pol_expand + org_c1 + 
              factor(reg_code), data = filtered_data)

# Calculate robust standard errors
robust_se <- vcovHC(model_psa4, type = "HC1")
robust_results <- coeftest(model_psa4, vcov = robust_se)
robust_coef <- robust_results[, 1]  # Coefficients
robust_se_values <- robust_results[, 2]  # Robust standard errors

stargazer(
  model_psa4,
  #type = "text",
  se = list(robust_se_values), # Replace standard errors with robust SEs
  omit = c("factor\\(reg_code\\)"), # Omit factor variables
  omit.labels = c("Region Code"),              # Optional: Custom labels for omitted variables
  keep.stat = c("n", "rsq", "adj.rsq"),                # Show sample size, R², adjusted R²
  dep.var.labels = "Police Violence"
  #covariate.labels = c("Topics * Post-Invasion", "Auth Rec", "Org C1", "Election Month") # Rename predictors
)

plot4 = plot_model(model_psa4, type = "pred", terms = c("time_indicator",
                                                "pol_expand"),
           vcov.fun = robust_se) + theme_bw()+
  xlab("")+
  ylab("Probability of a Protest facing Police Violence")+
  scale_color_manual(values = c("royalblue", "tomato"),
                     labels = c("Other", "Political"),
                     name = "")+
  scale_x_continuous(breaks = c(0, 1, 2),
                     labels = c("before Feb 23", "Feb 24 to March 3", "March 4 to March 20"))+
  ggtitle(NULL)

ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/acled/polit_protest_first_war_weeks.png",
       plot = plot4, width = 10, height = 6, dpi = 300)


filtered_data <- filtered_data %>%
  mutate(
    month_year_date = as.Date(paste0(month_year, "-01"), format = "%Y-%m-%d"),
    
    # Calculate `mycnt` as months since January 2018
    mycnt = 12 * (year(month_year_date) - 2018) + month(month_year_date)
  )


model_psa5 <- lm(police_violence ~ mycnt * pol_expand + org_c1 + factor(reg_code), data = filtered_data)
# Calculate robust standard errors
robust_se <- vcovHC(model_psa5, type = "HC1")
robust_results <- coeftest(model_psa5, vcov = robust_se)
robust_coef <- robust_results[, 1]  # Coefficients
robust_se_values <- robust_results[, 2]  # Robust standard errors

stargazer(
  model_psa5,
  #type = "text",
  se = list(robust_se_values), # Replace standard errors with robust SEs
  omit = c("factor\\(reg_code\\)"), # Omit factor variables
  omit.labels = c("Region Code"),              # Optional: Custom labels for omitted variables
  keep.stat = c("n", "rsq", "adj.rsq"),                # Show sample size, R², adjusted R²
  dep.var.labels = "Police Violence"
  #covariate.labels = c("Topics * Post-Invasion", "Auth Rec", "Org C1", "Election Month") # Rename predictors
)

plot5 = plot_model(model_psa5, type = "pred", terms = c("mycnt",
                                                "pol_expand"),
           vcov.fun = robust_se) + theme_bw()+
  scale_color_manual(values = c("royalblue", "tomato"),
                     labels=c("Other", "Political"), name="")+
  xlab("Weeks (Numeric)")+
  ylab("Probability of a Protest facing Police Violence")+ ggtitle(NULL)

ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/acled/polit_protest_weeks_num.png",
       plot = plot5, width = 10, height = 6, dpi = 300)



#### 2021-2022 Subset #####

subset_2021_2022 = filtered_data %>% filter(year %in% c("2021", "2022")) %>% 
  mutate(
    month_year_date = as.Date(paste0(month_year, "-01"), format = "%Y-%m-%d"),
    
    # Calculate `mycnt` as months since January 2021
    mycnt = 12 * (year(month_year_date) - 2021) + month(month_year_date),
    month_year_categorical = format(month_year_date, "%b %y")
    ) %>% 
  mutate(
      # Ensure the categorical variable is ordered
      month_year_categorical = factor(
        month_year_categorical,
        levels = format(seq(as.Date("2021-01-01"), as.Date("2022-12-01"), by = "month"), "%b %y"),
        ordered = TRUE)
      )




model_psa6 <- lm(police_violence ~  pol_expand * month_year_categorical + auth_rec + org_c1 + factor(reg_code), data = subset_2021_2022)
# Calculate robust standard errors
robust_se <- vcovHC(model_psa6, type = "HC1")
robust_results <- coeftest(model_psa6, vcov = robust_se)
robust_coef <- robust_results[, 1]  # Coefficients
robust_se_values <- robust_results[, 2]  # Robust standard errors

stargazer(
  model_psa6,
  type = "text",
  se = list(robust_se_values), # Replace standard errors with robust SEs
  omit = c("factor\\(reg_code\\)", "pol_expand:month", "month_year_"), # Omit factor variables
  omit.labels = c("Region Code", "Month Year", "Month Year x Political"),              # Optional: Custom labels for omitted variables
  keep.stat = c("n", "rsq", "adj.rsq"),                # Show sample size, R², adjusted R²
  dep.var.labels = "Police Violence",
  covariate.labels = c("Political", "Protest Authorised", "Organizers") # Rename predictors
)

## plot
plot6 = plot_model(model_psa6, type = "pred", terms = c("month_year_categorical", "pol_expand"
),
vcov.fun = robust_se) + theme_bw()+
  scale_color_manual(values = c("darkgrey", "black"),
                     labels = c("Non-Political", "Political"),
                     name = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("")+
  ylab("Predicted Probability")+ ggtitle(NULL)
plot6

ggsave("C:/Users/murrn/GitHub/nonviolent-repression/outputs/acled/polit_protest_2021_2022.png",
       plot = plot6, width = 10, height = 6, dpi = 300)

ggpredict(model_psa6, terms = c("month_year_categorical", "pol_expand"))
