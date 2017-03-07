##--------------------------------------------
##
## Lecture 9 R methods
##
## Class: PCE Data Science Methods Class
##
## Subject:  Natural Langauge Processing
##
##--------------------------------------------

setwd('E:/Work/Teaching/PCE_Data_Science/9_NLP')

##-----Load Libraries-----
library(tm)
library(SnowballC)
library(wordcloud)
library(e1071)
library(caret)
library(RTextTools)
library(topicmodels)
library(slam)
library(RKEA)
library(broom)
library(stringdist)
library(tidytext)
library(dplyr)

# If you cannot install stringdist, download the zip here:
# https://cran.rstudio.com/bin/windows/contrib/3.2/stringdist_0.9.3.zip

library(textir)
library(openNLP)
library(openNLPdata)
library(qdap)

##----Measuring Text Distance------
word1 = "beer"
word2 = "bear"

phrase1 = "Open the pod bay doors, Hal"
phrase2 = "I'm your huckleberry"

# Hamming distance
stringdist(word1, word2, method = "hamming")
stringdist(phrase1, phrase2, method = "hamming") # Have to be of same length

# Levenshtein distance
stringdist(word1, word2, method = "lv")
stringdist(phrase1, phrase2, method = "lv")

# Jaccard distance
stringdist(word1, word2, method = "jaccard")
stringdist(phrase1, phrase2, method = "jaccard")

# Weighted Jaccard
w_jaccard = function(s1, s2){
  elements = union(strsplit(s1,"")[[1]], strsplit(s2,"")[[1]])
  min_e = sapply(elements, function(x){
    min( nchar(s1) - nchar(gsub(x,"",s1)) , nchar(s2) - nchar(gsub(x,"",s2)) )
  })
  max_e = sapply(elements, function(x){
    max( nchar(s1) - nchar(gsub(x,"",s1)) , nchar(s2) - nchar(gsub(x,"",s2)) )
  })
  return(1 - ( sum(min_e) / sum(max_e) ))
}

w_jaccard(word1, word2)
w_jaccard(phrase1, phrase2)

##-----Load Data Sets-----
# Load email list
texts = read.csv("text_messages.csv", stringsAsFactors = FALSE)

# Change to lower case:
texts$Message = tolower(texts$Message)

# Remove punctuation
texts$Message = sapply(texts$Message, function(x) gsub("'", "", x))
# Now the rest of the punctuation
texts$Message = sapply(texts$Message, function(x) gsub("[[:punct:]]", " ", x))

# Remove numbers
texts$Message = sapply(texts$Message, function(x) gsub("\\d","",x))

# Remove extra white space, so we can split words by spaces
texts$Message = sapply(texts$Message, function(x) gsub("[ ]+"," ",x))

# Remove non-ascii
texts$Message = iconv(texts$Message, from="latin1", to="ASCII", sub="")

# remove stopwords # Careful doing this in the right order
stopwords()
my_stops = as.character(sapply(stopwords(), function(x) gsub("'","",x)))
texts$Message = sapply(texts$Message, function(x){
  paste(setdiff(strsplit(x," ")[[1]],stopwords()),collapse=" ")
})

# Remove extra white space again:
texts$Message = sapply(texts$Message, function(x) gsub("[ ]+"," ",x))

# Stem words:
texts$message_stem = sapply(texts$Message, function(x){
  paste(setdiff(wordStem(strsplit(x," ")[[1]]),""),collapse=" ")
})


# Create a Corpus (matrix of frequent terms)
##-----Text Corpus-----
# We have to tell R that our collection of reviews is really a corpus.
text_corpus = Corpus(VectorSource(texts$message_stem))

# Build a Document Term Matrix
# Terms        Docs
#            ... 32 33 34 35 36 37 38 39 ...
# about       0  0  1  1  1  0  1  2  1  0
# wut         0  0  0  0  0  0  1  0  0  0
# lol         0  0  0  1  0  0  0  0  0  0
# u           0  0  0  0  0  0  0  0  0  0
# rofl        0  0  1  0  0  0  0  0  0  0
# ...

text_term_matrix = DocumentTermMatrix(text_corpus)

dim(text_term_matrix)

# Save Matrix (This is mostly empty)
# NOTE BE CAREFUL DOING THIS WITH LARGER DATA SETS!!!!!!
text_corpus_mat = as.matrix(text_term_matrix)
dim(text_corpus_mat)

# Convert to Data Frame
text_frame = as.data.frame(text_corpus_mat)

# Use only the most common terms
which_cols_to_use = which(colSums(text_corpus_mat) > 10)

text_frame = text_frame[,which_cols_to_use]

# Convert to factors:
text_frame = as.data.frame(lapply(text_frame, as.factor))

# Add the response
text_frame$type = texts$Type

# Split into train/test set
train_ind = sample(1:nrow(text_frame), round(0.8*nrow(text_frame)))
train_set = text_frame[train_ind,]
test_set = text_frame[-train_ind,]

# Compute Naive Bayes Model
text_nb = naiveBayes(as.factor(type) ~ ., data = train_set)
test_predictions = predict(text_nb, newdata = test_set, type="class")

# Look at outcomes:
confusionMatrix(test_predictions, as.factor(test_set$type))

# Do a prediction
important_words = setdiff(names(text_frame), "type")

sample_text = "Please call asap for free consultation!"

# Define text cleaning function:
clean_text = function(text){
  text = tolower(text)
  text = gsub("'", "", text)
  text = gsub("[[:punct:]]", " ", text)
  text = gsub("\\d","",text)
  text = gsub("[ ]+"," ",text)
  text = iconv(text, from="latin1", to="ASCII", sub="")
  text = gsub("[ ]+"," ",text)
  text = paste(setdiff(wordStem(strsplit(text," ")[[1]]),""),collapse=" ")
  return(text)
}

sample_text = clean_text(sample_text)

# Create occurence vector of important words
sample_occurences = sapply(important_words, function(x){
  return(as.numeric(x%in%strsplit(sample_text," ")[[1]]))
})

sample_data = as.data.frame(t(sample_occurences))
sample_prediction = predict(text_nb, newdata = sample_data, type = "class")

##--------BEER REVIEW DATA-----
# Load scraped beer reviews
reviews = read.csv("beer_reviews.csv", stringsAsFactors = FALSE)
reviews$date = as.Date(reviews$date, format = "%Y-%m-%d")

str(reviews)
range(reviews$date)

# Normalize Data:
# Change to lower case:
reviews$review = tolower(reviews$review)

# Remove punctuation
# Better to take care of the apostrophe first
reviews$review = sapply(reviews$review, function(x) gsub("'", "", x))
# Now the rest of the punctuation
reviews$review = sapply(reviews$review, function(x) gsub("[[:punct:]]", " ", x))

# Remove numbers
reviews$review = sapply(reviews$review, function(x) gsub("\\d","",x))

# Remove extra white space, so we can split words by spaces
reviews$review = sapply(reviews$review, function(x) gsub("[ ]+"," ",x))

# Remove non-ascii
reviews$review = iconv(reviews$review, from="latin1", to="ASCII", sub="")

# remove stopwords
stopwords()
my_stops = as.character(sapply(stopwords(), function(x) gsub("'","",x)))
my_stops = c(my_stops, "beer", "pour")

# Maybe more stop words to use?

reviews$review = sapply(reviews$review, function(x){
  paste(setdiff(strsplit(x," ")[[1]],my_stops),collapse=" ")
})# Wait a minute for this to complete

# Remove extra white space again:
reviews$review = sapply(reviews$review, function(x) gsub("[ ]+"," ",x))

# Stem words:
reviews$review_stem = sapply(reviews$review, function(x){
  paste(setdiff(wordStem(strsplit(x," ")[[1]]),""),collapse=" ")
})

# Remove empty/short reviews:
sum(nchar(reviews$review_stem)<15)
reviews = reviews[nchar(reviews$review_stem)>15,]



##-----Text Corpus-----
# We have to tell R that our collection of reviews is really a corpus.
review_corpus = Corpus(VectorSource(reviews$review_stem))

# Build a Document Term Matrix
# Terms        Docs
#            ... 32 33 34 35 36 37 38 39 ...
# drink        0  0  1  1  1  0  1  2  1  0
# balanc       0  0  0  0  0  0  1  0  0  0
# exept        0  0  0  1  0  0  0  0  0  0
# hop          0  0  0  0  0  0  0  0  0  0
# sweet        0  0  1  0  0  0  0  0  0  0
# ...

# The following takes another minute to run.
review_document_matrix = TermDocumentMatrix(review_corpus)
review_term_matrix = DocumentTermMatrix(review_corpus)

# These two matrices are transposes of each other
dim(review_term_matrix)
dim(review_document_matrix)

# Too large to write out, so look at a part of it
inspect(review_term_matrix[1:5,1:5])

# Too large and too sparse, so we remove sparse terms:
review_term_small = removeSparseTerms(review_term_matrix, 0.995) # Play with the % criteria, start low and work up
dim(review_term_small)
inspect(review_term_small[1:5,1:5])

# Look at frequencies of words across all documents
word_freq = sort(colSums(as.matrix(review_term_small)))
hist(word_freq[word_freq<10000], breaks=35)

# Most common:
tail(word_freq, n=10)

# Least Common:
head(word_freq, n=10)


##-----Word Clouds------
set.seed(42)
words_for_cloud = tail(word_freq, n= 50)

wordcloud(names(words_for_cloud), words_for_cloud, colors=brewer.pal(6, "Dark2"))


#----TF-IDF in R------
tf_idf_reviews = DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))

# Too large and too sparse, so we remove sparse terms:
tf_idf_small = removeSparseTerms(tf_idf_reviews, 0.99) # Play with the % criteria, start low and work up
dim(tf_idf_small)

# Look at frequencies of words across all documents
tfidf_word_freq = sort(colSums(as.matrix(tf_idf_small)))
hist(tfidf_word_freq[tfidf_word_freq<2000], breaks=30)

# Most common:
tail(tfidf_word_freq, n=10)

# Least Common:
head(tfidf_word_freq, n=10)

##-------Apply this for different styles and compare------
# let's apply this for specific styles!
hoppy_styles = c("American Double / Imperial IPA",
                 "American IPA",
                 "American Pale Ale (APA)")
sum(reviews$style%in% hoppy_styles)
reviews$hoppy_indicator = reviews$style%in% hoppy_styles

# Calucate most common IDF words for hoppy styles:
#--------------------------------------------------
# First recreate corpus for hoppy styles
review_corpus_hoppy = Corpus(VectorSource(reviews$review_stem[reviews$hoppy_indicator==1]))
# Then create Document Term Matrix
review_term_matrix_hoppy = DocumentTermMatrix(review_corpus_hoppy, control = list(weighting = weightTfIdf))
# Then remove sparse terms
review_term_small_hoppy = removeSparseTerms(review_term_matrix_hoppy, 0.995)
# Check Dimensions
dim(review_term_small_hoppy)
# Find most common TF-IDF terms
tfidf_word_freq_hoppy = sort(colSums(as.matrix(review_term_small_hoppy)))

# Most common:
tail(tfidf_word_freq_hoppy, n=10)

# Least Common:
head(tfidf_word_freq_hoppy, n=10)

# Calucate most common IDF words for non-hoppy styles:
#----------------------------------------------------
# First recreate corpus for non-hoppy styles
review_corpus_nohoppy = Corpus(VectorSource(reviews$review_stem[reviews$hoppy_indicator==0]))
# Then create Document Term Matrix
review_term_matrix_nohoppy = DocumentTermMatrix(review_corpus_nohoppy, control = list(weighting = weightTfIdf))
# Then remove sparse terms
review_term_small_nohoppy = removeSparseTerms(review_term_matrix_nohoppy, 0.995)
# Check Dimensions
dim(review_term_small_nohoppy)
# Find most common TF-IDF terms
tfidf_word_freq_nohoppy = sort(colSums(as.matrix(review_term_small_nohoppy)))

# Most common:
tail(tfidf_word_freq_nohoppy, n=10)

# Least Common:
head(tfidf_word_freq_nohoppy, n=10)

# Find unique words in each:
hoppy_unique_tfidf = setdiff(names(rev(tfidf_word_freq_hoppy)), names(tfidf_word_freq_nohoppy))
non_hoppy_unique_tfidf = setdiff(names(rev(tfidf_word_freq_nohoppy)), names(tfidf_word_freq_hoppy))

##-------Sentiment Analysis--------------

# Filter out NA ratings
summary(reviews$rating)
reviews = reviews[!is.na(reviews$rating),]

# There are a few built in dictionaries of sentiments:
# 'afinn' = integer rating of word.
# 'bing' = positive/negative
# 'nrc' = emotion of word
bing_sentiments = get_sentiments('bing')
bing_sentiments$integer = ifelse(bing_sentiments$sentiment=='positive', 1, -1)

# We could do an inner join on words with the bing (+/-) sentiment set
avg_sentiment = sapply(reviews$review, function(x){
  # Here we pass in the review, x, as a string
  words = strsplit(x, ' ')[[1]]
  indices = unlist(sapply(words, function(w) which(bing_sentiments$word==w)))
  return(mean(bing_sentiments$integer[indices]))
})

# Replace NaN's with zero
reviews$avg_sentiment[is.nan(reviews$avg_sentiment)]=0

hist(avg_sentiment)
reviews$avg_sentiment = avg_sentiment

# Look at a plot of sentiment and rating
sentiment_sequence = seq(-1, 1, len=25)
avg_rating_seq = sapply(1:(length(sentiment_sequence)-1), function(x){
  min_sent = sentiment_sequence[x]
  max_sent = sentiment_sequence[x+1]
  return(mean(reviews$rating[reviews$avg_sentiment>=min_sent & reviews$avg_sentiment < max_sent], na.rm=T))
})

plot(sentiment_sequence[1:(length(sentiment_sequence)-1)], avg_rating_seq)

# Most common positive and negative terms:
all_review_text = paste(reviews$review, collapse = ' ')

bing_sentiments$frequency = sapply(bing_sentiments$word, function(w) {
  word_count = tryCatch({
    str_count(all_review_text, w)
  },
  error = function(e) {
    0
  })
  return(word_count)
})

# Most common positive/negative words
bing_sentiments = bing_sentiments[order(bing_sentiments$frequency, decreasing = T),]

head(bing_sentiments[bing_sentiments$integer==1,], n=10)
head(bing_sentiments[bing_sentiments$integer== -1,], n=10)


##-------Latent Dirichlet Allocation------
review_corpus_unstemmed = Corpus(VectorSource(reviews$review))
review_dtm = DocumentTermMatrix(review_corpus_unstemmed, control = list(wordLengths = c(4,10)))
dim(review_dtm)

# Remove sparse terms
review_term_small = removeSparseTerms(review_dtm, 0.999)
dim(review_term_small)

# Remove documents that no longer have interesting words
rowTotals = apply(review_term_small , 1, sum)
reviews = reviews[rowTotals > 0,]
review_term_small = review_term_small[rowTotals > 0, ]  

# Number of topics:
k = 4

# Perform LDA
lda = LDA(review_term_small, k)

lda_tidy = tidy(lda)
lda_tidy
# This produces a data frame of three columns
#   Topic: Integer label of topic
#   Term:  Word
#   Beta:  Probability of Word given topic
sum(lda_tidy$beta[lda_tidy$topic==1])

# Get list of top keywords for each topic:
beer_topics = terms(lda, 10)
# Plot probabilities for each topic:

beer_top_terms <- lda_tidy %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

beer_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

##-------Topic Modeling as a Feature in Regression------
beer_topics = topics(lda)
head(beer_topics)

# Get probabilities for each review:
beer_topic_probs = posterior(lda, review_term_small)
beer_topic_probs[[2]]
beer_topic_df = as.data.frame(beer_topic_probs[[2]])
colnames(beer_topic_df) = c('t1', 't2', 't3', 't4')
reviews = cbind(reviews, beer_topic_df)

# Can we predict hoppy style or not? (feature was 'hoppy_indicator')
train_ix = sample(1:nrow(reviews), size = round(0.8*nrow(reviews)))
test_ix = setdiff(1:nrow(reviews), train_ix)
review_train = reviews[train_ix,]
review_test = reviews[test_ix,]
hoppy_logistic = glm(hoppy_indicator ~ t1 + t2 + t3 + t4, data = review_train, family=binomial)

# Predictions on test set
hoppy_probs = predict(hoppy_logistic, newdata=review_test, type='response')
hoppy_preds = ifelse(hoppy_probs > 0.35,1,0)

# Create confusion matrix
confusionMatrix(data=hoppy_preds, reference = as.numeric(review_test$hoppy_indicator))
# Not very good.
# Improvement:  Try with more groups (increase k) and more words (leave more words in)!

##-----Part of Speech Tagging------
detach("package:caret", unload=TRUE)
detach("package:qdap", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
library(qdap)
x1 = "The customer ordered a beer."
x2 = "The customer got his order of beer."
word_token_annotator = Maxent_Word_Token_Annotator()

string_input = x1

char_annotate = Annotation(1L, "sentence", 1L, nchar(string_input)) # How to break up the string
word_annotate = annotate(string_input, word_token_annotator, char_annotate) # Tell R where the words are
part_of_speech = annotate(string_input, Maxent_POS_Tag_Annotator(), word_annotate) # Use HMM to tag words

part_of_speech
unlist(part_of_speech$features[2:length(part_of_speech$features)])