import pandas as pd
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt

acled = pd.read_csv("acled_types_auth_2018_2023.csv")


def get_yearly_sample(df, year):
    """
    Function to get stratified sample for a specific year
    """
    df_year = df[df['year'] == year]
    sample, _ = train_test_split(df_year, shuffle=True,
                                 random_state=123,
                                 train_size=100,
                                 stratify=df_year['month'])
    return sample

sample_2018 = get_yearly_sample(acled, 2018)
sample_2019 = get_yearly_sample(acled, 2019)
sample_2020 = get_yearly_sample(acled, 2020)
sample_2023 = get_yearly_sample(acled, 2023)

combined_sample = pd.concat([sample_2018, sample_2019, sample_2020, sample_2023])


combined_sample['date'] = pd.to_datetime(combined_sample['date'])

# plt.figure(figsize=(10, 6))
# combined_sample['date'].groupby([combined_sample['date'].dt.year, combined_sample['date'].dt.month]).count().plot(kind='bar')

# plt.title('Distribution of Data Over Time (2018-2023)')
# plt.xlabel('Year-Month')
# plt.ylabel('Number of Observations')
# plt.xticks(rotation=45)  # Rotate labels for better readability
# plt.tight_layout()  # Adjust layout
# plt.show()


combined_sample.to_csv('acled_sample_2018_2023.csv', index=False)

