import subprocess
import sys

def install_package(package):
    """Installs a package if it's not already installed."""
    try:
        __import__(package)
    except ImportError:
        print(f"Installing {package}...")
        subprocess.check_call([sys.executable, "-m", "pip", "install", package])

# Ensure required packages are installed
required_packages = ["torch", "transformers"]
for package in required_packages:
    install_package(package)

# Now import the necessary libraries
import torch
from transformers import pipeline

def classify_text(texts, model_choice):
    """
    Classifies text using the specified transformer model.

    Parameters:
        texts (list of str): List of input texts to classify.
        model_choice (str): Choice of model ('xlm-roberta', 'mbert', 'xlnet').

    Returns:
        list of dicts: [{'label': str, 'score': float}, ...] for each text input.
    """
    # Define model name based on choice
    if model_choice == "xlm-roberta":
        model_name = "xlm-roberta-large-finetuned-conll03-english"
    elif model_choice == "mbert":
        model_name = "bert-base-multilingual-cased"
    elif model_choice == "xlnet":
        model_name = "xlnet-base-cased"
    else:
        raise ValueError("Invalid model choice. Choose 'xlm-roberta', 'mbert', or 'xlnet'.")

    # Load the model pipeline
    classifier = pipeline("text-classification", model=model_name, tokenizer=model_name, device=0 if torch.cuda.is_available() else -1)

    # Ensure texts is a list
    if isinstance(texts, str):
        texts = [texts]

    # Perform classification
    predictions = classifier(texts, truncation=True)

    # Extract labels & scores
    return [{"label": pred["label"], "score": pred["score"]} for pred in predictions]
