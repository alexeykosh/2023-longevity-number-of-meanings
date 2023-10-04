#!/bin/bash

# Run the Wiki tokenizer
python3 src/wiki_tokenizer.py

# Preprocess the Wiktionary dump
python3 src/wiktionary_preprocessing.py

# Compute longevity and number of meanings
python3 src/etymology_extraction.py
