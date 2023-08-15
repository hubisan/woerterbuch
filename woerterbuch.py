from typing import Optional, List, Dict, Type
import json

import wn
from simplemma.simplemma import lemmatize

import json


def get_word_objects(word: str, wordnet: wn.Wordnet) -> Optional[List[wn.Word]]:
    """
    Get a list of Word objects for the given word.

    Args:
        word (str): The word to look up.
        wordnet (Wordnet): The Wordnet object for querying.

    Returns:
        list: A list of Word objects or None.
    """
    word_objects = wordnet.words(word)
    if word_objects:
        return word_objects
    else:
        return None


def lemmatize_word(word: str, lang: str) -> Optional[str]:
    """
    Lemmatize a word using the specified language.

    Args:
        word (str): The word to lemmatize.
        lang (str, optional): The language for lemmatization.

    Returns:
        str: The lemmatized form of the word or None.
    """
    lemma = lemmatize(word, lang=lang)
    if lemma != word:
        return lemma
    else:
        return None

def filter_by_part_of_speech( word_objects: List[wn.Word], part_of_speech: str,) -> Optional[wn.Word]:
    """
    Filter a list of Word objects by a specific part of speech.

    Args:
        word_objects (List[wn.Word]): A list of Word objects.
        part_of_speech (str): The desired part of speech.

    Returns:
        Optional[wn.Word]: The first word object matching the specified part of
          speech or None if no match is found.
    """
    for word_object in word_objects:
        pos = word_object.pos
        if part_of_speech and part_of_speech == pos:
            return word_object
    return None

def get_parts_of_speech(word_objects: List[wn.Word]) -> Optional[str]:
    """
    Get a JSON string of parts of speech from a list of Word objects.

    Args:
        word_objects (List[wn.Word]): A list of Word objects.

    Returns:
        Optional[str]: A JSON string containing a list of parts of speech
            extracted from the Word objects. Returns None if the parts of speech
            list is empty.
    """
    parts_of_speech = []
    for word_object in word_objects:
        parts_of_speech.append(word_object.pos)
    if parts_of_speech:
        return json.dumps(parts_of_speech, ensure_ascii=False)
    else:
        return None


def get_definitions_and_synonyms(word_object: wn.Word) -> List[Dict[str, List[str]]]:
    """
    Get definitions and synonyms for a Word object.

    Args:
        word_object (Word): A Word object.

    Returns:
        list: A list of dictionaries containing definitions and synonyms.
    """
    synsets = word_object.synsets()
    definitions_and_synonyms = []
    for synset in synsets:
        definition = synset.definition()
        synonyms = synset.lemmas()
        # Remove the word itself.
        word_lemma = word_object.lemma()
        if word_lemma in synonyms:
            synonyms.remove(word_lemma)
        definitions_and_synonyms.append(
            {"definition": definition, "synonyms": synonyms}
        )
    return definitions_and_synonyms

def woerterbuch_definitions_and_synonyms(word: str, part_of_speech: str = None, wordnet_name: str = "odenet", lemma_lang: str = "de"):
    """
    Get definitions and synonyms for a given word.

    Args:
        word (str): The word to look up.
        part_of_speech (str or None, optional): If provided, filter results
            to show only words with the specified part of speech. Supported
            values are 'n' for noun, 'a' for adjective/adverb, and 'v' for verb.
            Defaults to None.
        wordnet_name (str, optional): The name of the WordNet database to use.
            Defaults to "odenet".
        lemma_lang (str, optional): The language for lemmatization. Defaults to "de".

    Returns:
        str: A JSON-formatted string containing definitions and synonyms
        of the word. If multiple parts of speech are found, it returns a list with
        the possible parts of speech. If no words are found, it returns None.

    Example:
        >>> woerterbuch_definitions_and_synonyms('draussen')
        >>> woerterbuch_definitions_and_synonyms('orange', 'a')
    """

    wordnet: wn.Wordnet = wn.Wordnet(wordnet_name)
    word_objects: Optional[List[wn.Word]] = None
    word_object: Optional[wn.Word] = None
    word_lemma: Optional[str] = None

    word_objects = get_word_objects(word, wordnet)

    # If no word was found get the lemma and try again.
    if not word_objects:
        word_lemma = lemmatize_word(word, lemma_lang)
        if word_lemma:
            word_objects = get_word_objects(word_lemma, wordnet)
        if not word_objects:
            return None

    # If multiple objects are found then return the parts of speech to choose
    # from.
    if len(word_objects) > 1:
        # If part of speech is set filter by it.
        if part_of_speech:
            word_object = filter_by_part_of_speech(word_objects, part_of_speech)
            if not word_object:
                return None
        # Else return a list of part of speeches.
        else:
            return get_parts_of_speech(word_objects)
    else:
        word_object = word_objects[0]

    definitions_and_synonyms = get_definitions_and_synonyms(word_object)

    if definitions_and_synonyms:
        # The german used in Switzerland doesn't use the Eszett. During
        # lemmatization double s might be converted to Eszett. In this case the
        # lemma should be None.
        if word_lemma and word == word_lemma.replace("ß", "ss"):
            word_lemma = None
        # The return value includes the word used as parameter, the lemma (might be
        # different to the param and the defintions with the synonyms for each
        # definition.
        output = {
            "word-param": word,
            "word-lemma": word_lemma,
            "definitions": definitions_and_synonyms,
        }
        return json.dumps(output, ensure_ascii=False)
    else:
        return None

def process_command(command: str, args: str) -> Optional[str]:
    """
    Process a command and its arguments using registered functions.

    Args:
        command (str): The command to be executed.
        args (str): The comma-separated arguments for the command.

    Returns:
        str: The result of the executed command or an "Invalid command" message.
          The command returns a string or None.
    """

    functions = {
        "woerterbuch_definitions_and_synonyms": woerterbuch_definitions_and_synonyms,
        # "get_synonyms": get_synonyms,
        # "get_definitions": get_definitions,
    }

    if command in functions:
        func = functions[command]
        parsed_args = [arg.strip() for arg in args.split(",")]

        return func(*parsed_args)
    else:
        return "Invalid command"


while True:
    # Wait input from the user (read a string from standard input)
    line = input()

    # Split the user input into command and arguments using the comma as a separator.
    command, args = line.split(",", 1)

    # Call the 'process_command' function with the extracted command and arguments.
    result = process_command(command, args)

    # Print the result of the executed command or an error message.
    print(result)
