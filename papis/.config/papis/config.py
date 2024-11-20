import os
import papis.config
from papis.format import Jinja2Formatter as F
import papis.document

# no need to init now, just modify class variable
env = F.get_environment()

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

def abbrev_phrase(phrases:list[str]) -> list[str]:
    """replace key phrases from :data:`abbreviations` with values"""
    rephrases = []
    for phrase in phrases:
        for pat, rep in abbreviations.items():
             rephrases.append(re.sub(pat, rep, phrase, flags=re.IGNORECASE))
    return rephrases

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
    wsel = words[:total] if total <= sel else words[:count]
    cutter = partial(smart_truncate, n=n)
    wcut = list(map(cutter, wsel))
    if ellipsis and (total > len(wsel)):
        wcut.append(ellipsis)
    return wcut

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
    seq = abbrev_phrase([seq])[0]
    seq = re.sub(ipat, "", seq)
    return seq

from datetime import date, datetime

def do_pubtime(doc:papis.document.Document) -> str:
    if doc['date']:
        return datetime.strptime(doc['date'], '%Y%m%d').strftime('%Y%m%d')
    elif doc['year']:
        return date(
            int(doc.get("year", 1)), int(doc.get("month", 1)), int(doc.get("day", 1))
        ).strftime('%Y%m%d')
    else:
        return "00000000"

def get_names(doc:papis.document.Document) -> list[str]:
    if doc["author_list"] or doc["author"]:
        if doc["author_list"]:
            return [d["family"] for d in doc["author_list"]]
        else:
            return [d["family"] for d in papis.document.split_authors_name(doc["author"])]
    elif doc["editor_list"] or doc["editor"]:
        if doc["editor_list"]:
            return [d["family"] for d in doc["editor_list"]]
        else:
            return [d["family"] for d in papis.document.split_authors_name(doc["editor"])]
    else:
        return ["anonymous"]

def do_abridge_names(
        doc:papis.document.Document, count:int=1, tol:int=0,
        n:int=0, ellipsis:str|None=None
) -> str:
    names = get_names(doc)
    names = abbrev_phrase(names)
    return "-".join(crop_words(names, count, tol, n, ellipsis))

def do_abridge_title(
        doc:papis.document.Document, count:int=5, tol:int=2,
        n:int=4, ellipsis:str|None=None
) -> str:
    title = doc["shorttitle"] or doc["title"]
    title = abridge_sequence(title)
    return "-".join(crop_words(title.split(), count, tol, n, ellipsis))

env.filters['tpub'] = do_pubtime
env.filters['abridge_names'] = do_abridge_names
env.filters['abridge_title'] = do_abridge_title
