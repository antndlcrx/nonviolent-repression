pacman::p_load(tidyverse, rio, ggplot2, lubridate, quanteda, keyATM, readxl, caret)

# to create pro-kremlin indicator, run this script
source('code/acled_pro_kremlin_indicator.py')

# same script but online on google collab
# https://colab.research.google.com/drive/1ZstQI4Q9vwUwKGj9SetYuutOUzi_QhqI?usp=sharing

# to assign protest type categories and create (un)authorised indicator, run 
source('code/acled_protest_auth_elections.R')

# to visualise monthly protest counts, run: 
source('code/acled_vizualisations.R')
