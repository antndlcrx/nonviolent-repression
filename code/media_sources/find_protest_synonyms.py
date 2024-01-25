### script to look-up protest words using gensim RU embeddings ###

import gensim.downloader as api
import numpy as np
import pandas as pd

# load ru embeddings 
model = api.load('word2vec-ruscorpora-300')



# Function to filter for only nouns
def filter_nouns(similar_words):
    return [word for word, similarity in similar_words if word.endswith('_NOUN')]

# Find most similar words
similar_words = model.most_similar('протест_NOUN', topn=30)

# Filter results to include only nouns
nouns_only = filter_nouns(similar_words)

# Display the filtered nouns
print(nouns_only)

# ['митинг_NOUN', 'демонстрация_NOUN', 'бунт_NOUN', 'манифестация_NOUN',
#                     'бойкот_NOUN', 'забастовка_NOUN', 'пикетирование_NOUN', 'пикет_NOUN',
#                     'стачка_NOUN',]
