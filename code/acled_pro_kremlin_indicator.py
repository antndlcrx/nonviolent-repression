import pandas as pd
import numpy as np
import re

# read data
acled = pd.read_csv("data/raw_data/acled_ru_2018_2023_mz.csv")

## find unique categories
# make a list of lists len = len(acled_protest)
splitted_list = [actors.split("; ") if isinstance(actors, str) else np.nan for actors in acled_protest["assoc_actor_1"]]

# Flatten the list of lists, checking if each element is iterable (i.e., a list)
flattened_list = [item for sublist in splitted_list for item in (sublist if isinstance(sublist, list) else [sublist])]

# Find unique values, excluding nan
unique_values = {x for x in flattened_list if x == x}

len(unique_values)

# manually select pro-kremlin actors
pro_kremlin = [
    'ER: United Russia',
  'Former Government of Russia (2000-)',
  'Former Military Forces of Russia (2000-)',
  'Former Police Forces of Russia (2000-)',
  'Government of Russia (2000-) Republic of Chechnya',
  'Military Forces of Russia (2000-)',
  'Military Forces of Russia (2000-) National Guard - Special Purpose Police Unit',
  'Motherland Party',
  'NOD: National Liberation Movement',
  'NPSR: National Patriotic Forces of Russia',
  "NPSR: People's Patriotic Union of Russia",
  'New People',
  'Police Forces of Russia (2000-)',
  'VVPOD: Young Army Cadets National Movement',
  "Zakhar Prilepin's Guard"
]

# Function to check if any sub-value is in pro_kremlin
def check_pro_kremlin(value):
  """
  input: str
  output: int {0, 1}

  example: 'Government of Russia (2000-); Labour Group (Russia)' ->
              ['Government of Russia (2000-)', 'Labour Group (Russia)'] ->
                1
  """
  if pd.isna(value):
        return 0
  return int(any(actor in pro_kremlin for actor in str(value).split('; ')))

# Apply the function to each row in the 'assoc_actor_1' column
acled['pro_kremlin_indicator'] = acled['assoc_actor_1'].apply(check_pro_kremlin)

# write csv file
acled.to_csv("data/processed_data/acled_protest_krml_indicator.csv", index=False)
