# Hakyll-Search prototype

hHere is the repo containing what I hacked in a few days after learning Haskell. The goal was to learn a bit by writing real software, instead of Yet Another Hello World.

This Hakyll website contains a search form (in search.html) using only client side code. The Javascript code downloads a JSON file containing a mapping word -> array of indexes, and another JSON file containing an array of URLs. When searching for a word, we look for the indexes, and get the URLs corresponding to these indexes in the second file.

The Haskell code may be a bit ugly, but I had lots of fun writing it :)
