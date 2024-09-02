import jinja2.Environment as Jenv
import re
import typing as t
from functools import partial

# abbrev values may be functions mapping match objects to strings
# abbrev strings are expected to be one word
abbreviations = {
    "density functional theory":"DFT"
}

def smart_truncate(word:str, n:int=0) -> str:
    """truncate the input word if it is not a registered abbreviation
    1. walk :param:`n` characters, else return word
    2. if word end was reached, then truncate, else
    3. if prior character is consonant, then truncate, else
    4. walk 1 more character and repeat from step 2

    always returns at least two letters
    """
    if n and word not in abbreviations.values():
        while word[n:]:
            n = n+1
            if word[n] not in "aeiouy":
                return word[:n]
    return word

def abbrev_phrase(phrase:str) -> str:
    """replace key phrases with an associated abbreviation"""
    for pat, rep in abbreviations.items():
        phrase = re.sub(pat, rep, phrase)
    return phrase

def crop_words(
        words:list, count:int=1, tol:int=0,
        n:int=0, ellipsis:t.Optional[str]=None
) -> list:
    """shorten a list of words and the words themselves
    1. select a subset of words
       - if fewer than :param:`count` + :param:`tol` words, use all words
       - else, use :param:`count` words
    2. if not all words are used, append :param:`ellipsis` to subset
    3. if :param:`n` > 0, use at least n+1 characters, else use whole word

    abbreviations are never truncated
    """
    total = len(words)
    sel = count + tol
    nsel = words[:total] if total <= sel else words[:sel]
    if ellipsis and (total > len(nsel)):
        nsel = nsel.append(ellipsis)
    cutter = partial(smart_truncate, n=n)
    return list(map(cutter, nsel))
