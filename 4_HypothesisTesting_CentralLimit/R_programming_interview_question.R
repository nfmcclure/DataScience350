#------------------------------------------
#
# Programming question in R
#
#------------------------------------------

# Reverse words in a string

# First function, the long way.
reverse_long = function(my_string){
  separated_string = strsplit(my_string, "")[[1]]
  
  reversed_word = c()
  for (i in length(separated_string):1){
    reversed_word = c(reversed_word, separated_string[i])
  }
  
  return(paste(reversed_word, collapse=""))
}


# Second function, the short way
?rev  # Shortcut

reverse = function(my_string){
  separated_string = strsplit(my_string, "")[[1]]
  rev_string = rev(separated_string)
  return(paste(rev_string, collapse=""))
}

reverse("statistics")
reverse("tacocat")=="tacocat" # Test for palindromes

# Reverse each word in a sentence:
sentence = "Newton's Principia describes the axioms on the laws of motion"

reverse_sentence = function(sentence){
  separated_words = strsplit(sentence, " ")[[1]]
  rev_words = sapply(separated_words, reverse)
  return(paste(rev_words, collapse=" "))
}
