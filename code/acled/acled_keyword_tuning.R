pacman::p_load(tidyverse, rio, ggplot2, lubridate, quanteda, keyATM, readxl, caret)

# setwd(".../nonviolent-repression")

##### load and preprocess data #####
acled <- import("data/manually_labelled_data/acled_main_with_manual_coding_2018_2023.csv")

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

##### Keyword expansion #####

new_keywords <- list(
  political = c("Anti-government", "Protests", "Political rights", "Electoral fraud", "Resignation demands", 
                "Elected officials", "Political repression", "Political prisoners", "Memorial protests", 
                "Police abuse", "Political activism", "Foreign policy dissent", "Kuril Islands dispute", 
                "International solidarity", "Historical repression", "Minority rights", "Corruption", 
                "Ethnic republics", "Regional discontent", "Civil disobedience", "Freedom of speech", 
                "Political demonstrations", "Government accountability", "Opposition movements", 
                "Human rights advocacy"),
  
  economic = c("Government economic policies", "Exchange rates", "Wages", "Worker rights", 
               "Labour strikes", "Payment protests", "Mobilized personnel", "Family compensation", 
               "Casualty compensation", "Economic grievances", "Financial reform", 
               "Economic inequality", "Labor disputes", "Salary disputes", "Economic injustice", 
               "Financial instability", "Economic demands", "Work conditions", 
               "Economic policy dissent", "Financial rights"),
  
  social = c("Socially vulnerable groups", "Pensioners", "Chernobyl victims", "Students", "Disabled people", 
             "State benefits", "Community needs", "Infrastructure issues", "Defrauded flat owners", 
             "Social facilities", "Schools", "Hospitals", "Transport issues", "Soldiers' rights", 
             "Conscripts conditions", "Draftees", "Anti-LGBTQ protests", "Immigrant issues", 
             "Mobilisation policy dissent", "Crime and policing", "Coronavirus protests", 
             "Compulsory vaccination", "Elderly rights", "Youth activism", "Disability advocacy", 
             "Public service demands", "Military welfare", "Social inequality", "Healthcare access", 
             "Educational reform"),
  
  cultural = c("Monument destruction", "Historical buildings preservation", "Site preservation", 
               "City name changes", "Cultural figures", "Cultural viewpoints", "Museum closures", 
               "Library closures", "Memorial demonstrations", "Immortal regiment march", 
               "Religious protests", "Cultural heritage", "Historical conservation", "Cultural identity", 
               "Public memory", "Cultural policy dissent", "Religious freedom", "Cultural preservation", 
               "Historical remembrance", "Cultural activism"),
  
  legal = c("Unpopular legislation", "Legislation implementation", "Labour code", "Criminal code", 
            "Administrative code", "Illegal state acts", "Private company violations", "Forced eviction", 
            "Inappropriate construction", "Legal reform", "Judicial injustice", "Regulatory dissent", 
            "Labor rights", "Criminal justice", "Administrative fairness", "Corporate accountability", 
            "Property rights", "Urban planning disputes", "Legal advocacy", "Civil liberties"),
  
  environmental = c("Environmental issues", "Hazardous work conditions", "Waste dumping", 
                    "Forest reserves destruction", "Park conservation", "Protected woodlands", 
                    "Animal rights", "Urban development", "Town planning", "Zoning issues", 
                    "Eco-activism", "Pollution protests", "Wildlife protection", "Natural resource management", 
                    "Sustainable development", "Environmental regulation", "Green spaces preservation", 
                    "Ecological impact", "Land use disputes", "Environmental justice"),
  war = c("Operation")
)

# Function to preprocess keywords: lowercase, remove stopwords, stem, and split into words
preprocess_keywords <- function(keywords_list) {
  lapply(keywords_list, function(vec) {
    unlist(lapply(vec, function(keyword) {
      tokens <- tokens(keyword, what = "word", remove_punct = TRUE) %>%  # Tokenize
        tokens_tolower() %>%                                      # Convert to lowercase
        tokens_remove(stopwords("en")) %>%                        # Remove stopwords
        tokens_wordstem()                                         # Stem words
      paste(tokens, collapse=" ")
    }))
  })
}

# Apply the preprocessing function to the list
preprocessed_keywords <- preprocess_keywords(new_keywords)

# old keywords
old_keywords_with_war = list(cultural = c('histor','tear','demolish','commemor',
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


# Function to combine elements of two vectors
combine_vectors <- function(vec1, vec2) {
  c(vec1, vec2)
}

# Creating the combined list
combined_list <- Map(combine_vectors, old_keywords_with_war, preprocessed_keywords)

##### fit new key atm #### 

out <- keyATM(
  docs              = keyATM_docs,    # text input
  no_keyword_topics = 0,              # number of topics without keyword
  keywords          = combined_list,       # keywords
  model             = "base",         # select the model
  options           = list(seed = 250)
)

save(out, file = "acled_keyatm_base_no_other_1901.RData")
# load("outputs/acled_keyatm_base_1901.RData")

## diagnostics 
plot_topicprop(out, show_topic = 1:7)

fig_modelfit <- plot_modelfit(out)
fig_modelfit


plot_alpha(out)
plot_pi(out)


####

acled_subset <- acled %>% 
  select(-c(topic, proportion, max, protest_type))


##### get protest type based on topic probs ####
# acled_with_types = out$theta %>%
#   as.data.frame() %>%
#   cbind(acled_subset) %>%
#   pivot_longer(cols = `1_cultural`:`Other_1`, names_to = "topic", values_to = "proportion") %>%
#   group_by(event_id_cnty) %>%
#   mutate(max = proportion == max(proportion)) %>%
#   filter(max) %>%
#   mutate(protest_type = case_when(
#     topic == "Other_1" ~ "unclear",
#     TRUE ~ str_extract(topic, "(?<=_).+")) %>% 
#       factor(levels = c('environmental', 'cultural', 
#                         'political', 'social', 
#                         'economic', 'legal',
#                         'war', 'unclear')))

acled_with_types = out$theta %>%
  as.data.frame() %>%
  cbind(acled_subset) %>%
  pivot_longer(cols = `1_cultural`:`7_war`, names_to = "topic", values_to = "proportion") %>%
  group_by(event_id_cnty) %>%
  mutate(max = proportion == max(proportion)) %>%
  filter(max) %>%
  mutate(protest_type = str_extract(topic, "(?<=_).+")) %>%
  mutate(protest_type = factor(protest_type, levels = c('environmental', 'cultural', 
                                                        'political', 'social', 
                                                        'economic', 'legal',
                                                        'war')))


#### quality labels check ##### 

new_subset <- acled_with_types %>% 
  filter(!is.na(topic_manual)) %>%
  mutate(protest_manual = case_when(
    topic_manual %in% c("war (pro)", "war (anti)") ~ "war",
    TRUE ~ topic_manual
  ))

new_subset$protest_type <- as.factor(new_subset$protest_type)
new_subset$protest_manual <- as.factor(new_subset$protest_manual)

levels(new_subset$protest_type)
unique(new_subset$protest_type)

# Confusion Matrix
conf_matrix <- confusionMatrix(new_subset$protest_type, new_subset$protest_manual)
print(conf_matrix)

