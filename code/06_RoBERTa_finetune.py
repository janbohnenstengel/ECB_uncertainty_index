# %% 
import os
import pandas as pd
from datasets import Dataset
from transformers import AutoTokenizer, AutoModelForSequenceClassification, TrainingArguments, Trainer
import numpy as np
import evaluate
from pathlib import Path
from sklearn.model_selection import GroupShuffleSplit

MODEL_NAME = "Moritz-Pfeifer/CentralBankRoBERTa-sentiment-classifier"

try:
    script_dir = Path(__file__).resolve().parent
except NameError:            # interactive: no __file__
    script_dir = Path.cwd()  # assumes cwd is the script's folder

os.chdir(script_dir.parent)

print(os.getcwd())

# %% 
# 1) Load labeled data
lab = pd.read_csv(r"data/RoBERTa/silver_labels/uncertainty_statements_sentence_sample.csv")

# 2) Basic cleaning
# If llm_label is numeric with NaNs:
lab = lab.dropna(subset=["uncertain"]).copy()
lab["uncertain"] = lab["uncertain"].astype(int)

# Parse dates
lab["date"] = pd.to_datetime(lab["date"]).dt.date

# 3) Time-based split
gss = GroupShuffleSplit(n_splits=1, test_size=0.3, random_state=42)
train_idx, temp_idx = next(gss.split(lab, groups=lab["statement_id"]))

temp = lab.iloc[temp_idx]
gss2 = GroupShuffleSplit(n_splits=1, test_size=0.5, random_state=42)
val_idx, test_idx = next(gss2.split(temp, groups=temp["statement_id"]))

lab["split"] = "train"
lab.loc[temp.index[val_idx], "split"] = "val"
lab.loc[temp.index[test_idx], "split"] = "test"

# Check for sentences appearing in train and test and exclude if exists
train_sents = set(lab.loc[lab["split"] == "train", "sentence"])
leak = lab[(lab["split"] != "train") & (lab["sentence"].isin(train_sents))]
print("Val/test sentences also in train:", len(leak))

if len(leak) > 0:
    lab = lab.drop(leak.index)
    print(f"Dropped {len(leak)} val/test sentences that also appear in train")

# 4) Sanity checks
print(lab["split"].value_counts())
print(pd.crosstab(lab["split"], lab["uncertain"], normalize="index"))

# 5) Save training table
df = lab[["date", "statement_id", "sentence_id", "sentence", "uncertain", "split"]].copy()
df.to_csv("data/RoBERTa/silver_labels/finetune_train_data.csv", index=False)

# %%
# --- Split data
train_df = df[df["split"] == "train"][["sentence", "uncertain"]].rename(columns={"uncertain": "labels"}).copy()
val_df   = df[df["split"] == "val"][["sentence", "uncertain"]].rename(columns={"uncertain": "labels"}).copy()
test_df  = df[df["split"] == "test"][["sentence", "uncertain"]].rename(columns={"uncertain": "labels"}).copy()

print("Sizes:", len(train_df), len(val_df), len(test_df))
print("Train label share:", train_df["labels"].mean())

# %%
# --- Tokenize
tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME)

def tokenize(batch):
    return tokenizer(batch["sentence"], truncation=True, max_length=512)

train_ds = Dataset.from_pandas(train_df, preserve_index=False).map(tokenize, batched=True)
val_ds   = Dataset.from_pandas(val_df, preserve_index=False).map(tokenize, batched=True)
test_ds  = Dataset.from_pandas(test_df, preserve_index=False).map(tokenize, batched=True)


# %% 
# --- Model
model = AutoModelForSequenceClassification.from_pretrained(MODEL_NAME, num_labels=2)

# %%
# --- Metrics
f1 = evaluate.load("f1")
acc = evaluate.load("accuracy")

def compute_metrics(eval_pred):
    logits, labels = eval_pred
    preds = np.argmax(logits, axis=-1)
    return {
        "accuracy": acc.compute(predictions=preds, references=labels)["accuracy"],
        "f1": f1.compute(predictions=preds, references=labels, average="binary")["f1"],
    }

# %%
# --- Training args
args = TrainingArguments(
    output_dir="cbroberta_models/version_temp",
    learning_rate=2e-5,
    per_device_train_batch_size=16,
    per_device_eval_batch_size=16,
    num_train_epochs=3,
    eval_strategy="epoch",
    save_strategy="epoch",
    load_best_model_at_end=True,
    metric_for_best_model="f1",
    greater_is_better=True,
    logging_steps=50,
    report_to="none",
    dataloader_num_workers=0,
    fp16=False,
)

# Optional: if dataset is highly imbalanced, class weights can be set.
# The simplest first pass is to train without weights and inspect precision/recall/F1.
# If needed, class-weighted loss can be done via a custom Trainer.


# %%
# Train Model (Takes ~40min on my device)
trainer = Trainer(
    model=model,
    args=args,
    train_dataset=train_ds,
    eval_dataset=val_ds,
    tokenizer=tokenizer,
    compute_metrics=compute_metrics,
)

trainer.train()

print("Validation:")
print(trainer.evaluate(val_ds))

print("Test:")
print(trainer.evaluate(test_ds))

# %%
# --- Save the best model + tokenizer
trainer.save_model("cbroberta_models/best")
tokenizer.save_pretrained("cbroberta_models/best")


# %%
# Check Label distribution
print("Train distribution:")
print(train_df["labels"].value_counts())
print(train_df["labels"].value_counts(normalize=True))

print("\nVal distribution:")
print(val_df["labels"].value_counts())

print("\nTest distribution:")
print(test_df["labels"].value_counts())


# %%
# Check predictions
predictions = trainer.predict(test_ds)
pred_labels = np.argmax(predictions.predictions, axis=-1)
print("Predicted labels distribution:")
print(pd.Series(pred_labels).value_counts())
print("\nActual labels distribution:")
print(pd.Series(test_df["labels"].values).value_counts())

# %%
