#example on adist
# (adist(c("kit", "sit","kitten"), "kitten"))


correct <- function(word, taxanomies_dat) {
  
  #convert sorted_words to a data frame by using assigne
  sorted_words = as.character(get(taxanomies_dat))
  
  # Calculate the edit distance between the word and all other words in sorted_words.
  edit_dist <- adist(sorted_words,word)
  # Calculate the minimum edit distance to find a word that exists in big.txt 
  # with a limit of 20% of the length of word
  min_edit_dist <- min(edit_dist, nchar(word)*0.20)
  # Generate a vector with all words with this minimum edit distance.
  # Since sorted_words is ordered from most common to least common, the resulting
  # vector will have the most common / probable match first.
  proposals_by_prob <- c(sorted_words[ edit_dist <= min_edit_dist])
  # In case proposals_by_prob would be empty we append the word to be corrected...
  # proposals_by_prob <- c(proposals_by_prob, word)
  # ... and return the first / most probable word in the vector.
  ret_wrd = proposals_by_prob[1]
  ret_wrd = ifelse(is.na(ret_wrd),"",ret_wrd); return(ret_wrd)
}



