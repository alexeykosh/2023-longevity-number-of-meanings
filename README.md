# The Semantic Snowball Effect: Long-lived Words Accrue More Meanings
## Online supplement

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10988781.svg)](https://doi.org/10.5281/zenodo.10988781)

Authors: 

- Alexey Koshevoy
- Isabelle Dautriche
- Olivier Morin

## 0. Pre-registration 

This study was preregistered on the Open Science Framework. The pre-registration document is stored [here]([https://osf.io/h8mqk/?view_only=3d6d827e16ce4330ae2ba25d56195e79](https://osf.io/7rfp4/?view_only=076fb4537e8d404394ab9f92df03a41f)).

## 1. Pre-requisites. 

Before running the code, you need to download the following files and put them in the data folder: 

- The Wiki40b dataser for French (fr) can be downloaded using the [wiki-tokenizer](https://github.com/tpimentelms/wiki-tokenizer) tool.
- The Wikitionary dump for French can be downloaded from [WikitionaryX](http://redac.univ-tlse2.fr/lexiques/wiktionaryx.html). 

The folder with the Wiki40b dataset should be names `wiki-corpus' and put in the data folder, together with the unpacked WikinoaryX file. 

## 2. Getting the dataset.

First, install the necessary Python packages by running the following command in your terminal:

```bash
pip install -r requirements.txt
```

To process the Wiki40b dataset, you can use the provided bash script. Make sure you have Python 3 installed on your system.

1. First, ensure the bash script (`run_preprocessing.sh`) is present in the same directory as your Python scripts.

2. Make the bash script executable by running the following command in your terminal:

```bash
chmod +x run_preprocessing.sh
```

3. Run the bash script by running the following command in your terminal:

```bash
./run_preprocessing.sh
```

This script will execute the necessary Python commands to tokenize the Wiki40b dataset, preprocess the Wiktionary dump, and compute the longevity and number of meanings for the words in the dataset. The results will be saved as **age_estimations.csv** in the **data** folder.

## 3. Analysis

### 3.1. Validity check 

The [change_point_detection.ipynb](https://github.com/alexeykosh/2023-longevity-number-of-meanings/tree/main/notebooks) notebook contains the code to check the validity of the etymology extraction method using Google n-gram and change point detection algorithms.

### 3.2. Analysis of the results

The *analysis.r* file contains the code to analyze the results of the etymology extraction method.
