##--------------------------------------------
##
## Lecture 9 R methods
##
## Class: PCE Data Science Methods Class
##
## Subject:  Natural Langauge Processing
##
##--------------------------------------------

setwd('E:/Work/Teaching/PCE_Data_Science/8_NLP')

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

library(stringdist)

# If you cannot install stringdist, download the zip here:
# https://cran.rstudio.com/bin/windows/contrib/3.2/stringdist_0.9.3.zip

library(textir)
library(openNLP)
library(openNLPdata)

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

# What's another benefit about the weighted jaccard?
# Slightly more evenly distributed as well
#    Let's create random strings of random length and observe this
N = 1000
sample_words = sapply(1:100, function(x) {
  paste(sample(letters, sample(3:10,1), replace=TRUE), collapse="")
})

# Look at some words
sample_words[1:25]

# Calcluate weighted jaccard distances
sample_distances_weighted = sapply(1:5000, function(x){
  w_jaccard(sample(sample_words,1), sample(sample_words,1))
})

# Calculate regular jaccard distances
sample_distances_jaccard = sapply(1:5000, function(x){
  stringdist(sample(sample_words,1), sample(sample_words,1), method = "jaccard")
})

# Most of random distances are between 0.5 and 1
hist(sample_distances_weighted, breaks=50) # More regular
hist(sample_distances_jaccard, breaks=50) # Slightly irregular


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

# remove stopwords
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
sample_text = tolower(sample_text)
sample_text = gsub("'", "", sample_text)
sample_text = gsub("[[:punct:]]", " ", sample_text)
sample_text = gsub("\\d","",sample_text)
sample_text = gsub("[ ]+"," ",sample_text)
sample_text = iconv(sample_text, from="latin1", to="ASCII", sub="")
sample_text = gsub("[ ]+"," ",sample_text)
sample_text = paste(setdiff(wordStem(strsplit(sample_text," ")[[1]]),""),collapse=" ")

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

# Maybe more stop words to use?.... leave for homework data exploration

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

# Look at frequencies of words across all documents
word_freq = sort(colSums(as.matrix(review_term_small)))

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

##-------Latent Dirichlet Allocation------
review_corpus_unstemmed = Corpus(VectorSource(reviews$review))
review_dtm = DocumentTermMatrix(review_corpus_unstemmed, control = list(wordLengths = c(4,10)))
dim(review_dtm)

term_tfidf = tapply(review_dtm$v/row_sums(review_dtm)[review_dtm$i], review_dtm$j, mean) *
  log2(nDocs(review_dtm)/col_sums(review_dtm > 0))

# Filter out lower terms in tf-idf
review_dtm = review_dtm[,term_tfidf >= 0.1]
# filter out reviews that have no interesting terms
review_dtm = review_dtm[row_sums(review_dtm) > 0,]

dim(review_dtm)

# Number of topics:
k = 4

# Perform LDA
lda = LDA(review_dtm, k)

# Get list of top keywords for each topic:
beer_topics = terms(lda, 10)

##-----Part of Speech Tagging------
x1 = "The customer ordered a beer."
x2 = "The customer got his order of beer."
word_token_annotator = Maxent_Word_Token_Annotator()

string_input = x1

char_annotate = Annotation(1L, "sentence", 1L, nchar(string_input)) # How to break up the string
word_annotate = annotate(string_input, word_token_annotator, char_annotate) # Tell R where the words are
part_of_speech = annotate(string_input, Maxent_POS_Tag_Annotator(), word_annotate) # Use HMM to tag words

##-----Word2Vec-----
# Need 'devtools' to install
library(devtools)
install_github("bmschmidt/wordVectors")
# Wait to compile...
library(wordVectors)

cookbook_file = 'epib.txt'

# Train word2vec:
model = train_word2vec(cookbook_file, output="cookbook_vectors.bin",
                       threads = 3, vectors = 100, window=12)

# Read in prior trained model:
# read.vectors("cookbook_vectors.bin")

# What is the top 10 nearest word vectors to 'fish'?
nearest_to(model,model[["fish"]])

# What is the top 50 nearest word vectors to a series of words?
nearest_to(model,model[[c("fish","salmon","trout","shad","flounder","carp","roe","eels")]],50)

##-----Sentiment Analysis-----

