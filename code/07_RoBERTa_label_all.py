# %%
# Setup
import os
import pandas as pd
import numpy as np
import torch
from transformers import AutoTokenizer, AutoModelForSequenceClassification
from torch.utils.data import DataLoader
from pathlib import Path

# %%
# --- Paths
try:
    script_dir = Path(__file__).resolve().parent
except NameError:            # interactive: no __file__
    script_dir = Path.cwd()  # assumes cwd is the script's folder

os.chdir(script_dir.parent)

MODEL_DIR = "cbroberta_models/best"
INFILE    = "data/clean_text/statement_sentences.csv"
OUTFILE   = "data/RoBERTa/all_sentences_labeled_RoBERTa.csv"

# %%
# --- Load data
df = pd.read_csv(INFILE)

# Expect a column called "sentence" (adjust if yours differs)
assert "sentence" in df.columns, f"Expected a 'sentence' column, found: {df.columns.tolist()}"

texts = df["sentence"].fillna("").astype(str).tolist()

# %%
# --- Device
device = "cuda" if torch.cuda.is_available() else "cpu"
print("Device:", device)

# %%
# --- Load tokenizer/model
tokenizer = AutoTokenizer.from_pretrained(MODEL_DIR)
model = AutoModelForSequenceClassification.from_pretrained(MODEL_DIR)
model.to(device)
model.eval()

# %%
# --- Quick Check
text = "We are highly uncertain about the outlook given recent developments."
enc = tokenizer(text, return_tensors="pt", truncation=True, max_length=512).to(device)

with torch.no_grad():
    logits = model(**enc).logits
    prob1 = torch.softmax(logits, dim=-1)[0, 1].item()
    pred = int(prob1 >= 0.5)

print("prob_uncertain:", prob1, "pred:", pred)

# %%
# --- Dataset: pretokenize in chunks in the collate for speed/memory balance
BATCH_SIZE = 32  # increase if GPU RAM allows; CPU might like 16-64

def collate_fn(batch_texts):
    return tokenizer(
        batch_texts,
        padding=True,
        truncation=True,
        max_length=512,
        return_tensors="pt",
    )

loader = DataLoader(texts, batch_size=BATCH_SIZE, shuffle=False, collate_fn=collate_fn)

all_prob1 = []
all_pred  = []


# %%
# --- Predictions (take ~15mins on my device)
softmax = torch.nn.Softmax(dim=-1)

with torch.no_grad():
    for enc in loader:
        enc = {k: v.to(device) for k, v in enc.items()}
        logits = model(**enc).logits
        probs = softmax(logits)                # shape: [bs, 2]
        prob1 = probs[:, 1].detach().cpu().numpy()
        pred  = (prob1 >= 0.5).astype(int)     # threshold 0.5 (change if you want)

        all_prob1.append(prob1)
        all_pred.append(pred)

prob1 = np.concatenate(all_prob1)
pred  = np.concatenate(all_pred)

df["uncertain_prob"] = prob1
df["uncertain_pred"] = pred

df.to_csv(OUTFILE, index=False)
print("Wrote:", OUTFILE)
print(df[["sentence", "uncertain_prob", "uncertain_pred"]].head(10))