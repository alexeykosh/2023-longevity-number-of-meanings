import xml.etree.ElementTree as ET
from collections import defaultdict
import pickle

if __name__ == '__main__':

    tree = ET.parse('../data/wiktionaryXfr2010.xml')

    pos = []

    for elem in tree.iter(tag='lexeme'):
        if elem.attrib['pos'] not in pos:
            pos.append(elem.attrib['pos'])

    root = tree.getroot()

    definitions_dict = defaultdict(list)

    for entry in root.findall('.//entry'):
        entry_form = entry.get('form')
        lexemes = entry.findall('.//lexeme')
        
        for lexeme in lexemes:
            pos = lexeme.get('pos')
            
            definitions = []
            for definition in lexeme.findall('.//toplevel-def'):
                gloss_element = definition.find('gloss')
                if gloss_element is not None:
                    gloss = gloss_element.text
                else:
                    gloss = None
                examples = [example.text for example in definition.findall('example')]
                definitions.append({'gloss': gloss, 'examples': examples})
            # lower entry_form and remove accents
            entry_form = entry_form.lower()
            key = f"{pos}-{entry_form}"
            definitions_dict[key].extend(definitions)

    with open('../data/definitions_dict.pickle', 'wb') as handle:
        pickle.dump(definitions_dict, handle, protocol=pickle.HIGHEST_PROTOCOL)
