import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split

# Function to get stratified sample for a specific year
def get_yearly_sample(df, year):
    df_year = df[df['year'] == year]
    sample, _ = train_test_split(df_year, shuffle=True,
                                 random_state=123,
                                 train_size=100,
                                 stratify=df_year['month'])
    return sample



acled = pd.read_csv("data/processed_data/acled_types_auth_2018_2023.csv")

# Getting samples for each year
sample_2018 = get_yearly_sample(acled, 2018)
sample_2019 = get_yearly_sample(acled, 2019)
sample_2020 = get_yearly_sample(acled, 2020)
sample_2023 = get_yearly_sample(acled, 2023)

# Concatenate the samples from all four years
combined_sample = pd.concat([sample_2018, sample_2019, sample_2020, sample_2023])
combined_sample['date'] = pd.to_datetime(combined_sample['date'])

combined_sample = combined_sample.drop('topic', axis=1)
combined_sample = combined_sample.drop('proportion', axis=1)
combined_sample = combined_sample.drop('protest_type', axis=1)
combined_sample = combined_sample.drop('max', axis=1)

combined_sample.to_csv('acled_sample_2018_2023.csv', index=False)
