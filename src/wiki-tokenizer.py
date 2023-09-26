import os
from collections import defaultdict
import spacy
import pickle

nlp = spacy.load('fr_core_news_sm', disable=['ner'])

def unite_files():
    '''Unite all files in the current directory into one file'''

    path = os.getcwd()

    files = os.listdir(path)

    with open('all_data.txt', 'w') as outfile:
        for fname in files:
            if fname.endswith('.txt'):
                with open(fname) as infile:
                    for line in infile:
                        outfile.write(line)


def tokenize(t):
    '''Tokenize a list of texts'''

    lemma_freq = defaultdict(lambda: {'count': 0, 'pos': '', 'lemma': ''})

    for doc in nlp.pipe(t, batch_size=2000, n_process=-1):
        for token in doc:
            if not token.is_punct and not token.is_space:
                lemma = token.lemma_
                pos = token.pos_

                key = f'{lemma}@{pos}'
                if key not in lemma_freq:
                    lemma_freq[key] = {'count': 0, 'pos': pos, 'lemma': lemma}
                lemma_freq[key]['count'] += 1

    total = sum([lemma_freq[key]['count'] for key in lemma_freq])
    
    for key in lemma_freq:
        lemma_freq[key]['freq'] = lemma_freq[key]['count'] / total

    return lemma_freq


if __name__ == '__main__':
    unite_files()

    with open('all_data.txt', 'r', encoding='utf-8') as f:
        text = f.read()
        text = text.split('\n')

    lemma_freq = dict(tokenize(text))

    with open('../data/lemma_freq.pkl', 'wb') as f:
        pickle.dump(lemma_freq, f)
