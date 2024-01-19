##### set up ####

pacman::p_load(tidyverse, rio, ggplot2, lubridate, quanteda, keyATM, readxl, caret)

acled <- import("data/processed_data/acled_protest_krml_indicator.csv")
# %>% 
#   mutate(docid = paste("text", row_number(), sep = ""))

corp_acled = corpus(acled, text_field = "notes",
                    docid_field = "event_id_cnty")

# tokenize texts (copying code from acled_keyatm.R)
toks_acled = tokens(corp_acled, remove_punct = TRUE, remove_number = TRUE) %>% 
  tokens_remove(pattern = c(stopwords("en"), "protest*", "size", "=")) %>% 
  tokens_wordstem() # stem the keywords to make it easier to use later

dfmt_acled = dfm(toks_acled) %>%
  dfm_trim(max_termfreq = 0.80, termfreq_type = "prop") #filter out most common words


keyATM_docs <- keyATM_read(texts = dfmt_acled)
summary(keyATM_docs)



##### Use KeyATM to assign protest labels from keywords ##### 

## prepare keywords 
# taking keywords from the acled_keyatm.R file 

keywords_with_war = list(cultural = c('histor','tear','demolish','commemor',
                                      'center','museum','heritag','council',
                                      'street','demolit','cultur'),
                         economic = c('wage','system','econom','compani',
                                      'nationwid','annual','anti-capitalist',
                                      'fairer','paid','promis','pay','land',
                                      'health','increas','bonus',
                                      'payment','left','work'), 
                         environmental = c('park','wast','zone',
                                           'forest','green','road','tree',
                                           'expans','villag','block','cut',
                                           'ecolog','pollut'),
                         legal = c('amend','term','constitut','propos',
                                   'presidenti','extend',
                                   'limit','law'),
                         political = c('arrest','releas','governor',
                                       'polit','sentenc',
                                       'assassin','attempt','administr',
                                       'fsb','ldpr','jail','murder','motiv',
                                       'journalist','involv','denounc','critic'),
                         social = c('vaccin','public','space','certif',
                                    'shop','restaur','mall','mandatori',
                                    'farm','school','interest'),
                         war = c('ukrain', 'war', 'invas', 'invad', 'militari', 'annex'))


out <- keyATM(
  docs              = keyATM_docs,    # text input
  no_keyword_topics = 1,              # number of topics without keyword
  keywords          = keywords_with_war,       # keywords
  model             = "base",         # select the model
  options           = list(seed = 250)
)

# save(out, file = "acled_keyatm_base_2711.RData")
load("outputs/acled_keyatm_base_2711.RData")

##### model fit plots (not sure how to interpret them)
key_viz <- visualize_keywords(docs = keyATM_docs, keywords = keywords_with_war)
key_viz

top_words(out, 15)


plot_topicprop(out, show_topic = 1:7)

fig_modelfit <- plot_modelfit(out)
fig_modelfit


plot_alpha(out)
plot_pi(out)

# semantic coherence
semantic_coherence(out, keyATM_docs, n = 10)

##### get protest type based on topic probs ####
acled_with_types = out$theta %>%
  as.data.frame() %>%
  cbind(acled) %>%
  pivot_longer(cols = `1_cultural`:`Other_1`, names_to = "topic", values_to = "proportion") %>%
  group_by(event_id_cnty) %>%
  mutate(max = proportion == max(proportion)) %>%
  filter(max) %>%
  mutate(protest_type = case_when(
           topic == "Other_1" ~ "unclear",
           TRUE ~ str_extract(topic, "(?<=_).+")) %>% 
           factor(levels = c('environmental', 'cultural', 
                             'political', 'social', 
                             'economic', 'legal',
                             'war', 'unclear')))


##### (un)authorised indicator #####

#dictionary
dict = quanteda::dictionary(list(unauthorized = c("not authorized", "unauthorized", "unlawful",
                                                  "unsanctioned", "did not approve", 
                                                  "did not authorize", "not sanctioned",
                                                  "not been authorized"),
                                 authorized = c("was authorized", "an authorized", 
                                                "was approved", "was sanctioned", 
                                                "authorized protest*")))

# alternative tokens with no pre-processing 
# as compared to stemmed acled tokens used for keyATM
acled_toks = tokens(corp_acled)

dict_toks = tokens_lookup(acled_toks, dictionary = dict)

res = convert(dfm(dict_toks), to = "data.frame") %>% 
  rename(event_id_cnty = doc_id)

# check for the indicator 
non_zero_count = sum(res$unauthorized != 0 & res$authorized != 0)
# there are 7 observations non-zero values for both auth and non-auth indicator

non_zero_observations <- res %>%
  filter(unauthorized != 0 & authorized != 0)
# merge 
acled_res = acled_with_types %>% left_join(res, by = "event_id_cnty")

write_csv(acled_res, "data/processed_data/acled_types_auth_2018_2023.csv")


##### merge with election dataset ####
elections_protests_v4 <- import("elections_protests_v4.xlsx")


# create named list for region harmonisation 
mapping_vector <- c(
  "Adygeya" = "Republic of Adygea",
  "Altai krai" = "Altai",
  "Amur oblast" = "Amur",
  "Arkhangelsk oblast" = "Arkhangelsk",
  "Astrakhan oblast" = "Astrakhan",
  "Bashkortostan" = "Republic of Bashkortostan",
  "Belgorod oblast" = "Belgorod",
  "Bryansk oblast" = "Bryansk",
  "Buryatia" = "Republic of Buryatia",
  "Chechen Republic" = "Republic of Chechnya",
  "Chelyabinsk oblast" = "Chelyabinsk",
  "Chuvashia" = "Republic of Chuvash",
  "Crimea" = "Not mapped",
  "Dagestan" = "Republic of Dagestan",
  "Ingushetia" = "Republic of Ingushetia",
  "Irkutsk oblast" = "Irkutsk",
  "Ivanovo oblast" = "Ivanovo",
  "Jewish a.ob." = "Jewish Autonomous Oblast",
  "Kabardino-Balkaria" = "Republic of Kabardino-Balkaria",
  "Kaliningrad oblast" = "Kaliningrad",
  "Kalmykia" = "Republic of Kalmykia",
  "Kaluga oblast" = "Kaluga",
  "Kamchatka krai" = "Kamchatka",
  "Karelia" = "Republic of Karelia",
  "Kemerovo oblast" = "Kemerovo",
  "Khabarovsk krai" = "Khabarovsk",
  "Khakassia" = "Republic of Khakassia",
  "Kirov oblast" = "Kirov",
  "Komi Republic" = "Republic of Komi",
  "Kostroma oblast" = "Kostroma",
  "Krasnodar krai" = "Krasnodar",
  "Krasnoyarsk krai" = "Krasnoyarsk",
  "Kurgan oblast" = "Kurgan",
  "Kursk oblast" = "Kursk",
  "Leningrad oblast" = "Leningrad",
  "Lipetsk oblast" = "Lipetsk",
  "Marij El" = "Republic of Mari El",
  "Moscow (city)" = "Moscow",
  "Moscow oblast" = "Moscow Oblast",
  "Murmansk oblast" = "Murmansk",
  "Nenets a.o." = "Nenets",
  "Nizhni Novgorod oblast" = "Nizhny Novgorod",
  "North Ossetia-Alania" = "Republic of North Ossetia-Alania",
  "Novgorod oblast" = "Novgorod",
  "Novosibirsk oblast" = "Novosibirsk",
  "Omsk oblast" = "Omsk",
  "Orenburg oblast" = "Orenburg",
  "Oryol oblast" = "Oryol",
  "Penza oblast" = "Penza",
  "Perm krai" = "Perm",
  "Primorsky krai" = "Primorskiy",
  "Pskov oblast" = "Pskov",
  "Republic of Altai" = "Republic of Altai",
  "Republic of Sakha (Yakutia)" = "Republic of Sakha",
  "Rostov oblast" = "Rostov",
  "Ryazan oblast" = "Ryazan",
  "Saint-Petersburg" = "Saint Petersburg",
  "Sakhalin oblast" = "Sakhalin",
  "Samara oblast" = "Samara",
  "Saratov oblast" = "Saratov",
  "Sevastopol" = "Not mapped",
  "Smolensk oblast" = "Smolensk",
  "Stavropol krai" = "Stavropol",
  "Sverdlovsk oblast" = "Sverdlovsk",
  "Tambov oblast" = "Tambov",
  "Tatarstan" = "Republic of Tatarstan",
  "Tomsk oblast" = "Tomsk",
  "Tula oblast" = "Tula",
  "Tuva" = "Republic of Tuva",
  "Tver oblast" = "Tver",
  "Tyumen oblast" = "Tyumen",
  "Udmurtia" = "Udmurt Republic",
  "Ulyanovsk oblast" = "Ulyanovsk",
  "Vladimir oblast" = "Vladimir",
  "Volgograd oblast" = "Volgograd",
  "Vologda oblast" = "Vologda",
  "Voronezh oblast" = "Voronezh",
  "Yamalo-Nenets a.o." = "Yamalo-Nenets",
  "Yaroslavl oblast" = "Yaroslavl",
  "Zabaikal krai" = "Zabaykalskiy",
  "Aginsk Buryat a.o." = "Aginsk Buryat a.o."
)

# reverse mapping - I did not consider the order would 
# matter but it does, the quickest way to harmonise region
# values was to just reverse it
reversed_mapping <- setNames(names(mapping_vector), mapping_vector)

# create new region var with values corresponding to 
# electiton_protests data
acled_res <- acled_res %>%
  mutate(region_name = reversed_mapping[admin1])

# join the datasets
merged_protest_elections <- right_join(elections_protests_v4, acled_res,
                                      by = c("reg_year" = "year", 
                                             "Name" = "region_name")) %>% 
  select(-"region") %>% # remove variable region containing constant "Europe"
  rename(region = Name,
         year = reg_year)

write_csv(merged_protest_elections, "merged_protest_elections.csv")

##### checks ####
# check for duplicates
sum(duplicated(acled_with_types$event_id_cnty))


##### merge with manually coded dataset ####

acled_types_auth_2018_2023 <- read_csv("data/processed_data/acled_types_auth_2018_2023.csv")
acled_preprocessed_jul21_dec22 <- read_excel("data/processed_data/acled_preprocessed_jul21_dec22.xlsx")

manual_cats <- acled_preprocessed_jul21_dec22 %>% 
  select(event_id_cnty, topic_manual)

merged_data <- left_join(acled_types_auth_2018_2023, manual_cats, by = "event_id_cnty") 

write_csv(merged_data, "data/processed_data/acled_main_with_manual_coding_2018_2023.csv")



##### session info ####
# R version 4.3.1 (2023-06-16 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 22621)
# 
# Matrix products: default
# 
# locale:
#   LC_COLLATE=English_United States.utf8 
# LC_CTYPE=English_United States.utf8   
# LC_MONETARY=English_United States.utf8
# LC_NUMERIC=C                          
# LC_TIME=English_United States.utf8    
# 
# time zone: Europe/London
# tzcode source: internal
# 
# attached base packages:
#   stats     graphics 
# grDevices utils    
# datasets  methods  
# base     
# 
# other attached packages:
#   caret_6.0-94   
# lattice_0.21-8 
# readxl_1.4.3   
# keyATM_0.5.0   
# quanteda_3.3.1 
# rio_1.0.1      
# lubridate_1.9.3
# forcats_1.0.0  
# stringr_1.5.0  
# dplyr_1.1.3    
# purrr_1.0.2    
# readr_2.1.4    
# tidyr_1.3.0    
# tibble_3.2.1   
# ggplot2_3.4.4  
# tidyverse_2.0.0
# 
# loaded via a namespace (and not attached):
#   fastmatch_1.1-4     
# gtable_0.3.4        
# recipes_1.0.8       
# tzdb_0.4.0          
# vctrs_0.6.3         
# tools_4.3.1         
# generics_0.1.3      
# stats4_4.3.1        
# parallel_4.3.1      
# fansi_1.0.5         
# pacman_0.5.1        
# ModelMetrics_1.2.2.2
# pkgconfig_2.0.3     
# R.oo_1.25.0         
# Matrix_1.6-2        
# data.table_1.14.8   
# RcppParallel_5.1.7  
# lifecycle_1.0.3     
# compiler_4.3.1      
# munsell_0.5.0       
# codetools_0.2-19    
# SnowballC_0.7.1     
# class_7.3-22        
# prodlim_2023.08.28  
# pillar_1.9.0        
# MASS_7.3-60         
# R.utils_2.12.2      
# gower_1.0.1         
# iterators_1.0.14    
# rpart_4.1-19        
# foreach_1.5.2       
# parallelly_1.36.0   
# lava_1.7.3          
# nlme_3.1-162        
# stopwords_2.3       
# digest_0.6.33       
# tidyselect_1.2.0    
# future_1.33.0       
# stringi_1.7.12      
# reshape2_1.4.4      
# listenv_0.9.0       
# splines_4.3.1       
# grid_4.3.1          
# colorspace_2.1-0    
# cli_3.6.1           
# magrittr_2.0.3      
# survival_3.5-5      
# utf8_1.2.3          
# future.apply_1.11.0 
# withr_2.5.1         
# scales_1.2.1        
# timechange_0.2.0    
# globals_0.16.2      
# nnet_7.3-19         
# timeDate_4022.108   
# cellranger_1.1.0    
# R.methodsS3_1.8.2   
# hms_1.1.3           
# hardhat_1.3.0       
# rlang_1.1.1         
# Rcpp_1.0.11         
# glue_1.6.2          
# pROC_1.18.5         
# ipred_0.9-14        
# rstudioapi_0.15.0   
# R6_2.5.1            
# plyr_1.8.9  
