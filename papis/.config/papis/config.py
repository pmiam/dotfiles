import os
import papis.config
from papis.format import Jinja2Formatter as F
# no need to init now, just modify class variable

import json
import re
import typing as t
from functools import partial

# abbrev values may be functions mapping match objects to strings
# abbrev strings are expected to be one word
with open(os.path.join(
        papis.config.get_config_folder(), "patterns.json"
)) as fd:
    patterns = json.loads(fd.read())
abbreviations = patterns['abbreviations']
ignores = patterns['ignores']

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
            if word[n] not in "aeiouy":
                return word[:n+1]
            n = n+1
    return word

def abbrev_phrase(phrase:str) -> str:
    """replace key phrases from :data:`abbreviations` with values"""
    for pat, rep in abbreviations.items():
        phrase = re.sub(pat, rep, phrase, flags=re.IGNORECASE)
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
    nsel = words[:total] if total <= sel else words[:count]
    if ellipsis and (total > len(nsel)):
        nsel.append(ellipsis)
    cutter = partial(smart_truncate, n=n)
    return list(map(cutter, nsel))

def abridge_sequence(
        seq:str,
        prologue_delim:t.Optional[str]=r"([:;!?]|--|-)",
        epilogue_delim:t.Optional[str]=r"[.!?]",
) -> str:
    """abridge a sentence-like sequence
    1. strip off words preceding chars matching :param:`prologue_delim`
    2. strip off words following chars matching :param:`epilogue_delim`
    3. abbreviate key phrases
    4. drop words matching :data:`ignores` regexps
    """
    ipat = re.compile("(" + "|".join(ignores) + ")", flags=re.IGNORECASE)
    ppat = re.compile(prologue_delim) if prologue_delim else re.compile("^")
    epat = re.compile(epilogue_delim) if epilogue_delim else re.compile("$")
    # not very smart, but plays well with most well formed sequences
    seq = re.split(ppat, seq, maxsplit=1)[-1].strip()
    seq = re.split(epat, seq, maxsplit=1)[0].strip()
    seq = abbrev_phrase(seq.lower())
    seq = re.sub(ipat, "", seq)
    return seq

def do_abridge_names(
        names:list, count:int=1, tol:int=0,
        n:int=0, ellipsis:str|None=None
) -> list:
    names = abbrev_phrase(" ".join(names).lower()).split()
    return crop_words(names, count, tol, n, ellipsis)

def do_abridge_title(
        title:str, count:int=5, tol:int=2,
        n:int=4, ellipsis:str|None=None
) -> list:
    title = abridge_sequence(title)
    return crop_words(title.split(), count, tol, n, ellipsis)

F.env.filters['abridge_names'] = do_abridge_names
F.env.filters['abridge_title'] = do_abridge_title
