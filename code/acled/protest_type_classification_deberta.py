# !pip install accelerate -U
# !pip install datasets

import pandas as pd
from sklearn.metrics import precision_recall_fscore_support
from sklearn.model_selection import train_test_split
from transformers import DebertaTokenizer, DebertaForSequenceClassification, TrainingArguments, Trainer
from sklearn.preprocessing import LabelEncoder
from datasets import Dataset
import numpy as np
import torch

# save path for gdrive
model_save_path = '/content/drive/MyDrive/non_violent_repressions/models'
tokenizer_save_path = '/content/drive/MyDrive/non_violent_repressions/models'

acled = pd.read_csv("/data/processed_data/acled_merged_18_23_edited_MT.csv")

# Convert 'event_date' to datetime format
acled['event_date'] = pd.to_datetime(acled['event_date'], format='%d %B %Y')

# Extract year for stratification
acled['year'] = acled['event_date'].dt.year

acled_labels = acled[acled['topic_manual'].notna()]
print("total n: ", acled_labels.shape)

# Label encoding for 'topic_manual'
label_encoder = LabelEncoder()
acled_labels['labels'] = label_encoder.fit_transform(acled_labels['topic_manual'])

# Split the dataset into train, validation, and test sets stratified by year
train_val, test = train_test_split(acled_labels, test_size=0.2, stratify=acled_labels['year'], random_state=123)
train, val = train_test_split(train_val, test_size=0.25, stratify=train_val['year'], random_state=123) 

# Initialize tokenizer
tokenizer = DebertaTokenizer.from_pretrained('microsoft/deberta-base')

# Tokenizing function
def tokenize_function(examples):
    return tokenizer(examples['notes'], padding="max_length", truncation=True)

# Convert DataFrames to Dataset objects
train_dataset = Dataset.from_pandas(train)
val_dataset = Dataset.from_pandas(val)
test_dataset = Dataset.from_pandas(test)

# Tokenize all datasets
train_dataset = train_dataset.map(tokenize_function, batched=True)
val_dataset = val_dataset.map(tokenize_function, batched=True)
test_dataset = test_dataset.map(tokenize_function, batched=True)

# Initialize model
model = DebertaForSequenceClassification.from_pretrained('microsoft/deberta-base', num_labels=len(np.unique(acled_labels['labels'])))

# 
training_args = TrainingArguments(
    output_dir='./results',
    num_train_epochs=3,
    per_device_train_batch_size=8,
    per_device_eval_batch_size=8,
    warmup_steps=500,
    weight_decay=0.01,
    logging_dir='./logs',
    logging_steps=10,
    evaluation_strategy="steps",  # Evaluate every `eval_steps` steps
    eval_steps=100,  # Number of steps to run evaluation
    save_strategy="steps",  # Save strategy
    save_steps=100,  # Save checkpoint every 100 steps
    load_best_model_at_end=True,  # Load the best model at the end of training
)

# Evaluate the model
trainer.evaluate(test_dataset)


# Initialize Trainer
trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=train_dataset,
    eval_dataset=val_dataset,
)


# Train the model
trainer.train()



# Save the model
model.save_pretrained(model_save_path)

# Save the tokenizer associated with the model
tokenizer.save_pretrained(tokenizer_save_path)

# Predict on the test dataset
predictions = trainer.predict(test_dataset)
pred_labels = np.argmax(predictions.predictions, axis=1)
true_labels = predictions.label_ids

# Calculate precision, recall, and F1-score for each label
precision, recall, f1, _ = precision_recall_fscore_support(true_labels, pred_labels, average=None, labels=np.unique(true_labels))

# Calculate micro and macro averages for overall metrics
precision_micro, recall_micro, f1_micro, _ = precision_recall_fscore_support(true_labels, pred_labels, average='micro')
precision_macro, recall_macro, f1_macro, _ = precision_recall_fscore_support(true_labels, pred_labels, average='macro')

# Print per-label metrics
label_names = label_encoder.inverse_transform(np.unique(true_labels))  # Assuming label_encoder is your LabelEncoder instance
for label, p, r, f in zip(label_names, precision, recall, f1):
    print(f"Label: {label} - Precision: {p}, Recall: {r}, F1: {f}")

# Print overall metrics
print(f"Overall (Micro Avg) - Precision: {precision_micro}, Recall: {recall_micro}, F1: {f1_micro}")
print(f"Overall (Macro Avg) - Precision: {precision_macro}, Recall: {recall_macro}, F1: {f1_macro}")

##### inferece ### 
# Load the tokenizer
tokenizer = DebertaTokenizer.from_pretrained(tokenizer_save_path)

# Load the model
model = DebertaForSequenceClassification.from_pretrained(model_save_path)

# Tokenize all texts in the dataset
tokenized_notes = tokenizer(list(acled['notes']), padding=True, truncation=True, return_tensors="pt")

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
model.to(device)

# Reduce batch size for tokenization and prediction
batch_size = 16  # Adjust based on your GPU's memory capacity

predicted_labels = []
for i in range(0, len(acled['notes']), batch_size):
    batch_notes = list(acled['notes'][i:i+batch_size])
    tokenized_notes = tokenizer(batch_notes, padding=True, truncation=True, return_tensors="pt")
    inputs = {key: value.to(device) for key, value in tokenized_notes.items()}
    with torch.no_grad():
        outputs = model(**inputs)
        batch_predictions = torch.argmax(outputs.logits, dim=-1)
        batch_labels = [label_encoder.inverse_transform([label.item()])[0] for label in batch_predictions]
        predicted_labels.extend(batch_labels)
        
acled['pred_labels'] = predicted_labels
acled.to_csv('/content/drive/MyDrive/non_violent_repressions/data/acled_with_preds_05_02.csv')
