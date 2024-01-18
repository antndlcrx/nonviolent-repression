pacman::p_load(tidyverse, rio, ggplot2, lubridate, quanteda)

setwd("C:/Users/murrn/GitHub/nonviolent-repression")


# load data
kprf = read_csv('C:/Users/murrn/GitHub/placeholder_data/kprf_output.csv', 
                quote = "\'")

activatica = read_csv('C:/Users/murrn/GitHub/placeholder_data/activatica_output.csv', 
                      quote = "\'")

kavkaz = read_csv("C:/Users/murrn/GitHub/placeholder_data//kavkaz_output_complete.csv",
                  quote = "\'") 

kommersant = read_csv("C:/Users/murrn/GitHub/placeholder_data//kommersant_output_complete.csv",
                      quote = "\'")


#### harmonise dates and create month-year indicator #### 

## Helper function and mapping from Yana's code

months_map = c(
  января = "January",
  февраля = "February",
  марта = "March",
  апреля = "April",
  мая = "May",
  июня = "June",
  июля = "July",
  августа = "August",
  сентября = "September",
  октября = "October",
  ноября = "November",
  декабря = "December"
)

replace_months = function(date_string, months_map) {
  for (month_rus in names(months_map)) {
    date_string <- gsub(month_rus, months_map[month_rus], date_string, fixed = TRUE)
  }
  return(date_string)
}

# Harmonise data for each source
kprf <- kprf %>%
  mutate(date = sub(" \\(.*", "", date_published),
         date = ymd_hm(date),
         month_year = format(date, "%Y-%m"),
         source = "KPRF")

kommersant <- kommersant %>%
  mutate(date = sub(" \\(.*", "", date_published),
         date = dmy_hm(date),
         month_year = format(date, "%Y-%m"),
         source = "Kommersant")

activatica <- activatica %>%
  mutate(month_year = format(date_created, "%Y-%m"),
         source = "Activatica",
         content = text)

kavkaz <- kavkaz %>%
  mutate(date = replace_months(date_published, months_map),
         date = dmy_hm(date),
         month_year = format(date, "%Y-%m"),
         source = "Kavkaz")

# bind data
all_media = bind_rows(kprf, kommersant, activatica, kavkaz)

##### check duplicates ####

length(unique(all_media_subset$content))
# Finding duplicate observations in 'variable'
duplicates <- duplicated(all_media$content)

# Getting the row IDs of these duplicates
row_ids <- which(duplicates)

# Print events
all_media$content[row_ids]

# duplicates seem to be the result of persisting parsing error
# create a subset df with duplicates to see n by source

media_duplicates <- all_media[row_ids, ]

# how many lost observations per source due to parsing error 
count_table <- table(media_duplicates$source)

print(count_table)

##### isolate protests with keywords ####

# temp: until parsing is fixed: subset with no duplicates 
all_media_subset <- all_media %>% 
  distinct(content, .keep_all = TRUE) %>%
  mutate(doc_id = paste0("doc_", row_number()))

corp_media = corpus(all_media_subset, text_field = "content",
                    docid_field = "doc_id")


#dictionary
dict <- quanteda::dictionary(list(
  protest = c('протест'),
  rally = c('митинг'),
  demonstration = c('демонстрация'),
  revolt = c('бунт'),
  manifestation = c('манифестация'),
  boycott = c('бойкот'),
  strike = c('забастовка'),
  picketing = c('пикетирование'),
  picket = c('пикет'),
  walkout = c('стачка')
))


# tokens with no pre-processing 
media_toks = tokens(corp_media)

dict_toks = tokens_lookup(media_toks, dictionary = dict)

res = convert(dfm(dict_toks), to = "data.frame") 
media_res = all_media_subset %>% left_join(res, by = "doc_id")

# create protest story indicator
media_res <- media_res %>%
  mutate(protest_indicator = as.integer(protest > 0 | rally > 0 |
                                          demonstration > 0 | protest > 0 |
                                          revolt > 0 | manifestation > 0 |
                                          boycott > 0 | strike > 0 |
                                          picketing > 0 | picket > 0 | walkout > 0))

write_csv(media_res, "data/processed_data/media_protests.csv")

##### descriptive stats #####

# Creating a vector of variable names 
variables_vector <- c("protest", "rally", "demonstration", 
                      "revolt", "manifestation", "boycott", "strike", 
                      "picketing", "picket", "walkout")

## Count and ratio of protest stories for each source
protest_stats_source <- media_res %>%
  group_by(source) %>%
  summarise(
    total_stories = n(),
    protest_stories = sum(protest_indicator),
    ratio_protest = protest_stories / total_stories
  )

protest_stats_source


## How many mentions of a keyword in corpus? 

# Calculate the sum of each variable
sums <- sapply(media_res[variables_vector], sum, na.rm = TRUE)

# Print the sums
print(sums)

## How many stories mention the keywords

# Count the number of observations > 0 for each variable
counts_greater_than_zero <- sapply(media_res[variables_vector], function(x) sum(x > 0, na.rm = TRUE))

# Print the counts
print(counts_greater_than_zero)
