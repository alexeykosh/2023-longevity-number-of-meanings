# 2023-longevity-number-of-meanings

## 1. Pre-requisites. 

Before running the code, you need to download the following files and put them in the data folder: 

- The Wiki40b dataser for French (fr) can be downloaded using the [wiki-tokenizer](https://github.com/tpimentelms/wiki-tokenizer) tool.
- The Wikitionary dump for French can be downloaded from [WikitionaryX](http://redac.univ-tlse2.fr/lexiques/wiktionaryx.html). 

The folder with the Wiki40b dataset should be names `wiki-corpus' and put in the data folder, together with the unpacked WikinoaryX file. 

## 2. Getting the dataset.

First, install the Python requirements by running:

```{bash}
pip install -r requirements.txt
```

Then, run the following command in bash:

```{bash}
python3 src/wiki_tokenizer.py
```

After that, you can run the following command to preprocess the Wiktionary dump:

```{bash}
python3 src/wikitionnary_preprocessing.py
```

Finally, you can run the following command to compute the longevity and number of meanings of the words in the Wiki40b dataset:

```{bash}
python3 src/etymology_extraction.py
```

The results will be saved as a CSV file in the data folder. 

## 3. Analysis

### 3.1. Validity check 

The *change_point_detection.ipynb* notebook contains the code to check the validity of the etymology extraction method using Google n-gram and change point detection algorithms.

### 3.2. Analysis of the results

The *analysis.r* file contains the code to analyze the results of the etymology extraction method.