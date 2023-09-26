import pickle
import re

import pandas as pd
import requests
from bs4 import BeautifulSoup

with open('../data/definitions_dict.pickle', 'rb') as f:
    definitions_dict = pickle.load(f)

pos_mapping = {'NOUN': 'N', 'VERB': 'V', 'ADJ': 'Adj', 'ADV': 'Adv'}

# date_pattern = r'\b(?:Ca|ca|Ca\.\s|)(\d{4}|\d{4}(?=\s*[-–])|
# (\d{4})\s*[-–]\s*\d{2,4}|[IVXLCDM]+(?=\s*[-–])|[IVXLCDM]+|
# \b[xX]{1,2}[iIvV]{0,3}(?:es)?\b|\b[xX]{1,2}[iIvV]{0,3}es\.\b)(?![^\[]*])(?![^\(]*\))\b'

date_pattern = re.compile(r'''
                \b
                (?:
                    Ca|ca|Ca\.\s|
                )
                (
                    \d{4}|
                    \d{4}(?=\s*[-–])|
                    (\d{4})\s*[-–]\s*\d{2,4}|
                    [IVXLCDM]+(?=\s*[-–])|
                    [IVXLCDM]+|
                    \b[xX]{1,2}[iIvV]{0,3}(?:es)?\b|
                    \b[xX]{1,2}[iIvV]{0,3}es\.\b
                )
                (?![^\[]*])
                (?![^\(]*\))
                \b
                ''', re.VERBOSE)


def get_number_of_m(pos, lemma):
    '''Get the number of meanings for a given word and pos'''
    return len(definitions_dict[f'{pos_mapping[pos]}-{lemma}'])

def convert_roman_to_arabic(roman):
    '''Convert roman numerals to arabic'''
    roman_numerals = {
        'I': 1, 'II': 2, 'III': 3, 'IV': 4, 'V': 5, 
        'VI': 6, 'VII': 7, 'VIII': 8, 'IX': 9, 'X': 10, 
        'XI': 11, 'XII': 12, 'XIII': 13, 'XIV': 14, 'XV': 15,
        'XVI': 16, 'XVII': 17, 'XVIII': 18, 'XIX': 19, 'XX': 20,
        'XXI': 21
    }

    if roman in roman_numerals:
        return roman_numerals[roman] * 100

    return None

def get_etymology(word, pos, dp=date_pattern):
    '''Get the etymology for a given word and pos'''
    url = f'https://www.cnrtl.fr/etymologie/{word}/{pos}'
    page = requests.get(url, timeout=5)
    soup = BeautifulSoup(page.content, 'html.parser')
    etymology_div = soup.find('div', {'id': 'art'})
    if etymology_div:
        etymology_text = etymology_div.get_text()

        matches = re.findall(dp, etymology_text)
        matches = [tuple([i.replace('es', '') for i in match]) for match in matches]
        if matches:
            valid_years = []
            for match in matches:
                for year in match:
                    if year.isdigit():
                        valid_years.append(int(year))
                    elif convert_roman_to_arabic(year.upper()):
                        valid_years.append(int(convert_roman_to_arabic(year.upper())) - 50)
            valid_years = [year for year in valid_years if 700 <= year <= 2020]
            if valid_years:
                return min(valid_years)

if __name__ == '__main__':
    ### Get the etymologies
    with open('lemma_freq.pkl', 'rb') as f:
        lemma_freq = pickle.load(f)
    # remove elements with count < 1000
    lemma_freq_ = {k: v for k, v in lemma_freq.items() if v['count'] > 100}
    # convert to a dataframe
    df = pd.DataFrame.from_dict(lemma_freq_, orient='index')
    df.reset_index(inplace=True)
    df.columns = ['lemma_', 'count', 'pos', 'lemma', 'freq']
    # match NOUN, VERB and ADJ to substantif, verbe and adjectif in a dict
    # match NOUN, VERB and ADJ to substantif, verbe and adjectif in a dict
    pos_dict = {'NOUN': 'substantif', 'VERB': 'verbe', 'ADJ': 'adjectif', 'ADV': 'adverbe'}
    # apply pos_dict to df['pos'] to get the correct pos for the url
    df['pos_fr'] = df['pos'].apply(lambda x: pos_dict[x] if x in pos_dict else None)
    df = df.query('pos_fr == pos_fr')
    # apply get_number_of_m to df['pos'] and df['lemma']
    df['number_of_meanings'] = df.apply(lambda x: get_number_of_m(x['pos'], x['lemma']), axis=1)
    # df where number_of_meanings is not null
    df = df.query('number_of_meanings > 0')
    # get etymology for each word
    df['etymology'] = df.progress_apply(lambda x: get_etymology(x['lemma'], x['pos_fr']), axis=1)
    # df where etymology is not null
    df = df[df['etymology'] == df['etymology']]
    df = df[df['etymology'] > 1500]
    # compute age by subtracting etymology from 2020
    df['age'] = 2020 - df['etymology']
    # save to csv
    df.to_csv('../data/age_estimations.csv', index=False)
