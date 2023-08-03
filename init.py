import json
import wn
from simplemma.simplemma import lemmatize

de = wn.Wordnet('odenet')

import json

def woerterbuch_definitions_and_synonyms(word, part_of_speech=False):
    """
    Get definitions and synonyms for a given word.

    Args:
        word (str): The word to lookup.
        part_of_speech (str or False, optional): If provided, filter results
            to show only words with the specified part of speech. Supported
            values are 'n' for noun, 'a' for adjective/adverb, and 'v' for verb.
            Defaults to False.

    Returns:
        str: A JSON-formatted string containing definitions and synonyms
        of the word. If multiple parts of speech are found it returns a list with
        the parts of speech possibilities. If no words are found it returns False.

    Example:
        woerterbuch_definitions_and_synonyms('draussen')
        woerterbuch_definitions_and_synonyms('orange', 'a')
    """
    words = de.words(word)

    # If no word was found get the lemma and try again.
    if not words:
        lemma = lemmatize(word, lang='de')
        if lemma != word:
            words = de.words(word)
        if not words:
            return False

    # If the array holds more then one word return the possibilities.
    # If the optional argument pos is provided select the part of speech
    # accordingly.
    if len(words) > 1:
        parts_of_speech = ['parts of speech']
        for w in words:
            pos = w.pos
            if part_of_speech and part_of_speech == pos:
                word = w
                break
            else:
                parts_of_speech.append(pos)
        return json.dumps(parts_of_speech)
    else:
        word = words[0]

    # Get the synsets which is a group of words with the same definition.
    synsets = word.synsets()
    definitions_and_synonyms = []
    for synset in synsets:
        definition = synset.definition()
        synonyms = synset.lemmas()

        # Remove the word itself.
        word_lemma = word.lemma()
        if word_lemma in synonyms:
            synonyms.remove(word_lemma)

        # Add the defintion and it synonyms (as children).
        definitions_and_synonyms.append([definition, synonyms])
    return json.dumps(definitions_and_synonyms)

def get_definitions(word):
    word = de.words(word)[0]
    synsets = word.synsets()
    definitions = []
    for synset in synsets:
        definitions.append(synset.definition())
    return definitions

# TODO
def get_synonyms(word):
    # lemmas = sorted(lemmas, key=str.casefold)
    return word

