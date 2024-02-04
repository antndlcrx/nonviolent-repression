pacman::p_load(tidyverse, rio, ggplot2, lubridate, quanteda, keyATM, readxl, caret)

# setwd(".../nonviolent-repression")

##### load and preprocess data #####
acled <- import("data/processed_data/acled_merged_18_23_edited_MT.csv")

unique(acled$event_id_cnty)

acled_with_type = acled %>% filter(!is.na(topic_manual))

#### Yana's code to get top freq words per category ####
#create corpus object
corp_acled = corpus(acled,
                    docid_field = "event_id_cnty",
                    text_field = "notes")
# tokenize texts
toks_acled = tokens(corp_acled, remove_punct = TRUE, remove_number = TRUE) %>% 
  tokens_remove(pattern = c(stopwords("en"), "protest*", "size", "=")) %>% 
  tokens_wordstem() # stem the keywords to make it easeir to use later

dfmt_acled = dfm(toks_acled) %>%
  dfm_trim(max_termfreq = 0.80, termfreq_type = "prop") #filter out most common words

# #generate testing data
# set.seed(123)
# test_ids = acled_with_type %>%
#   group_by(topic_manual) %>%
#   sample_frac(0.3) %>%
#   pull(event_id_cnty)
# 
# 
# #generate labels (with NAs for testing data)
# acled_train <- acled %>% filter(!(event_id_cnty %in% test_ids))
# 
# corp_train = corpus(acled_train,
#                     docid_field = "event_id_cnty",
#                     text_field = "notes")
# # tokenize texts
# toks_acled_tr = tokens(corp_train, remove_punct = TRUE, remove_number = TRUE) %>% 
#   tokens_remove(pattern = c(stopwords("en"), "protest*", "size", "=")) %>% 
#   tokens_wordstem() # stem the keywords to make it easeir to use later
# 
# dfmt_acled_tr = dfm(toks_acled_tr) %>%
#   dfm_trim(max_termfreq = 0.80, termfreq_type = "prop") #filter out most common words

#get top 75 features for each category
top_words = topfeatures(dfmt_acled, n = 75, groups = dfmt_acled$topic_manual)
#make a data frame with top words for all categories 
top_words_df = top_words %>% bind_cols()


all_values = data.frame(value = unlist(top_words),
                        name = names(unlist(top_words))) %>%
  separate(name, c("type", "token"), "\\.") %>%
  group_by(token) %>%
  filter(n() == 1)  # this is to remove tokens that appear in more than one category - 
# to make classification easier later, we need unique tokens

# Group by 'type' and then create a list of 'token' for each 'type'
keywords <- all_values %>%
  group_by(type) %>%
  summarise(words = list(token), .groups = 'drop') %>%
  deframe() %>% # Converts the dataframe to a named list
  map(~ unlist(.x)) # To ensure each element is a vector of words rather than a list


keyATM_docs <- keyATM_read(texts = dfmt_acled)
summary(keyATM_docs)

##### fit new key atm #### 

# Fit keyatm with labels 

out <- keyATM(
  docs              = keyATM_docs,    # text input
  no_keyword_topics = 0,              # number of topics without keyword
  keywords          = keywords,       # keywords
  model             = "base",         # select the model
  # model_settings    = list(labels = labels_use),  # set labels
  options           = list(seed = 123)
)

# save(out, file = "outputs/models/acled_keyatm_base_02_04_susbet.RData")
load("outputs/models/acled_keyatm_base_02_04.RData")

## diagnostics 
plot_topicprop(out, show_topic = 1:7)

fig_modelfit <- plot_modelfit(out)
fig_modelfit

plot_alpha(out)
plot_pi(out)

acled_with_preds = out$theta %>%
  as.data.frame() %>%
  cbind(acled) %>%
  pivot_longer(cols = `1_cultural`:`8_war (pro)`, names_to = "topic", values_to = "proportion") %>%
  group_by(event_id_cnty) %>%
  mutate(max = proportion == max(proportion)) %>%
  filter(max) %>%
  mutate(protest_type = str_extract(topic, "(?<=_).+")) %>%
  mutate(protest_type = factor(protest_type, levels = c('environmental', 'cultural', 
                                                        'political', 'social', 
                                                        'economic', 'legal',
                                                        'war (anti)', 'war (pro)')))
#### quality labels check ##### 

new_subset <- acled_with_preds %>% 
  # filter(event_id_cnty %in% test_ids)
  filter(!is.na(topic_manual))

new_subset$protest_type <- as.factor(new_subset$protest_type)
new_subset$topic_manual <- as.factor(new_subset$topic_manual)

levels(new_subset$protest_type)
unique(new_subset$protest_type)

# Confusion Matrix
conf_matrix_protest_type <- confusionMatrix(new_subset$protest_type, new_subset$topic_manual)
print(conf_matrix_protest_type)


##### Check on pro-regime ##### 

new_subset <- acled_with_preds %>% 
  # filter(event_id_cnty %in% test_ids)
  filter(!is.na(pro_regime))

new_subset$pro_kremlin_indicator <- as.factor(new_subset$pro_kremlin_indicator)
new_subset$pro_regime <- as.factor(new_subset$pro_regime)

levels(new_subset$pro_kremlin_indicator)
unique(new_subset$pro_kremlin_indicator)

# Confusion Matrix
conf_matrix_regime <- confusionMatrix(new_subset$pro_kremlin_indicator, new_subset$pro_regime)
print(conf_matrix_regime)

# save env for RMD report
save.image(file = "outputs/keyatm_env.RData")

