import pandas as pd
import requests
from bs4 import BeautifulSoup
import re   
import pickle 

### Get the etymologies
def convert_roman_to_arabic(roman):
    roman_numerals = {
        'I': 1, 'II': 2, 'III': 3, 'IV': 4, 'V': 5, 'VI': 6, 'VII': 7, 'VIII': 8, 'IX': 9,
        'X': 10, 'XI': 11, 'XII': 12, 'XIII': 13, 'XIV': 14, 'XV': 15, 'XVI': 16, 'XVII': 17, 'XVIII': 18, 'XIX': 19,
        'XX': 20, 'XXI': 21
    }

    if roman in roman_numerals:
        return roman_numerals[roman] * 100

    return None

def get_etymology(word, pos):
    url = f'https://www.cnrtl.fr/etymologie/{word}/{pos}'
    page = requests.get(url)
    soup = BeautifulSoup(page.content, 'html.parser')
    etymology_div = soup.find('div', {'id': 'art'})
    if etymology_div:
        etymology_text = etymology_div.get_text()
        date_pattern = r'\b(?:Ca|ca|Ca\.\s|)(\d{4}|\d{4}(?=\s*[-–])|(\d{4})\s*[-–]\s*\d{2,4}|[IVXLCDM]+(?=\s*[-–])|[IVXLCDM]+|\b[xX]{1,2}[iIvV]{0,3}(?:es)?\b|\b[xX]{1,2}[iIvV]{0,3}es\.\b)(?![^\[]*])(?![^\(]*\))\b'
        matches = re.findall(date_pattern, etymology_text)
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


### Get the dictionary
dic_ = pd.read_csv('dico.csv')
# set column names to word and definition
dic_.columns = ['word', 'definition']
# lower everything
dic_['word'] = dic_['word'].str.lower()
# definition is a list, so count number of elements in list by converting to list and applying len
dic_['definition'] = dic_['definition'].apply(lambda x: len(eval(x)))
# convert to a dictionary
dic = dic_.set_index('word').T.to_dict('list')
# convrt values to integers
dic = {k: int(v[0]) for k, v in dic.items()}


### Get the etymologies
with open('lemma_freq.pkl', 'rb') as f:
    lemma_freq = pickle.load(f)
# remove elements with count < 1000
lemma_freq_ = {k: v for k, v in lemma_freq.items() if v['count'] > 100}
# convert to a dataframe
df = pd.DataFrame.from_dict(lemma_freq_, orient='index')
df.reset_index(inplace=True)
df.columns = ['lemma_', 'count', 'pos', 'lemma', 'freq']
# map lemmas to dic to get definition length
df['number_of_meanings'] = df['lemma'].map(dic)
# match NOUN, VERB and ADJ to substantif, verbe and adjectif in a dict
pos_dict = {'NOUN': 'substantif', 'VERB': 'verbe', 'ADJ': 'adjectif', 'ADV': 'adverbe'}
# apply pos_dict to df['pos'] to get the correct pos for the url
df['pos_fr'] = df['pos'].apply(lambda x: pos_dict[x] if x in pos_dict else None)
# remove closed-class POS
df_ = df.query('number_of_meanings > 0 and pos_fr == pos_fr')  
# get etymology for each word
df_['etymology'] = df_.progress_apply(lambda x: get_etymology(x['lemma'], x['pos_fr']), axis=1)
# df_ where etymology is not null
df__ = df_[df_['etymology'] == df_['etymology']]
df__ = df__[df_['etymology'] > 1500]
# compute age by subtracting etymology from 2020
df__['age'] = 2020 - df__['etymology']
# save to csv
df__.to_csv('age_estimations.csv', index=False)
