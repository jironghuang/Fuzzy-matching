# Fuzzy-matching: correct(word, taxanomies_dat, perc_char_diff)
Swiss army knife for fuzzy matching
- Find the closest approximate word with x % char difference
- word is the word to 'look-up'
- taxanomies_dat is the list of words that you want to fuzzy match to
- perc_char_diff is the % of character difference threshold 

Example:
- How to return closes matched word in taxanomy with at most 10% character differences
correct("AB Sugar","taxo",0.1)
