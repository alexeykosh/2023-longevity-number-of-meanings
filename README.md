# The Semantic Snowball Effect: Long-lived Words Accrue More Meanings
## Online supplement

**Manual Version Number:** 1.0.0

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13740276.svg)](https://doi.org/10.5281/zenodo.13740276)

### Short Summary:
Why do some words have more meanings than others? Some researchers have argued for an explanation based on efficient communication: words that are shorter, more frequent, and easier to pronounce get more meanings, allowing for a more compact organization of the lexicon. However, these explanations mostly address synchronic effects, while linguistic ambiguity is inherently diachronic. We propose a novel approach where we rely on the longevity of words to estimate their degree of ambiguity. Using data covering more than half of a millennium, we find that words used for longer periods become more ambiguous. Our results support the intuition that the process of meaning accumulation is time-driven, indicating that time signatures are important predictors of linguistic features.

### Folder/File Overview:

#### Root Folder:
- **analysis.R**: R script for analyzing the results of the study.

#### `data/` Folder:
Contains input and output data related to word age estimation and frequencies:
- `age_estimations.csv`: Main dataset
- `age_estimation_1800.csv`: Cross-validated dataset

#### `figures/` Folder:
Contains the figures used both in the paper and the supplementary material.

#### `notebooks/` Folder:
Contains Jupyter notebooks used in the study:
- `change_point_detection.ipynb`: Notebook that validates the etymology extraction using Google n-gram and change point detection algorithms.
- `semantic-snowball-model.ipynb`: Main notebook containing the cultural evolutionary model of the Semantic Snowball Effect.

#### `src/` Folder:
Contains Python scripts for processing data:
- `etymology_extraction.py`: Python script that extracts etymology information for words.
- `wiki_tokenizer.py`: Script to tokenize the Wiki40b dataset.
- `wikitionnary_preprocessing.py`: Python script for preprocessing the Wiktionary dump.

#### Additional Files:
- **requirements.txt**: Lists the necessary Python packages to run the project.
- **run_preprocessing.sh**: Bash script for preprocessing of the dataset.

### Instructions to Run the Software:

All the analyses were run on R version 4.2.2 (2022-10-31) and Python 3.11.2.

#### Pre-requisites:
Before running the project, ensure the following resources are available:
1. Download the Wiki40b French dataset using the [wiki-tokenizer](https://github.com/tpimentelms/wiki-tokenizer) tool. Place it in the `data/wiki-corpus/` folder.
2. Download the French Wiktionary dump from [WiktionaryX](http://redac.univ-tlse2.fr/lexiques/wiktionaryx.html) and place the unpacked files in the `data/WikitionaryX` folder.

#### Workflow:

1. **Install Dependencies:**
   Install the required Python packages by running:
   ```bash
   pip install -r requirements.txt
   ```

2. **Preprocess Data:**
   - Make the `run_preprocessing.sh` script executable:
   ```bash
   chmod +x run_preprocessing.sh
   ```
   - Run the script to preprocess the Wiki40b dataset and compute word ages and meanings:
   ```bash
   ./run_preprocessing.sh
   ```
   The results will be saved in `data/age_estimations.csv`.

3. **Validity Check:**
   Use the `change_point_detection.ipynb` notebook to validate the etymology extraction method and obtain the cross-validated dataset.

4. **Analyze the Results:**
   Use the `analysis.R` script to reproduce the statistical analysis and generate the figures.
