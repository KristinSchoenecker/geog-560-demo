# In this project, we will see how people have been responding to tweets from official 'Adopt a Drain' twitter accounts.
# We will make a word cloud of words used at least twice by the tweeters, minus common words.
# I have already downloaded tweet responses from March 2020 - March 2023 and put them in the CSV file 'Replies.csv'

# Load Necessary Packages
library("NLP")
library("tm")
library("qdap")
library("qdapRegex")
library("wordcloud")

# Import tweet data from Replies.csv
AaD_Twitter_Reply_Data <- read.csv("Replies.csv", stringsAsFactors = FALSE)

# View the structure of data to find the correct column
str(AaD_Twitter_Reply_Data)

# Isolate replies from tweets
AaD_Replies <- AaD_Twitter_Reply_Data$Reply

# Remove URLs and special characters from replies (except for apostrophes)
AaD_Replies <- rm_twitter_url(AaD_Replies)
AaD_Replies <- gsub("[^A-Za-z]", " ", AaD_Replies)
AaD_Replies <- gsub("can t", "cannot", AaD_Replies)
AaD_Replies <- gsub("won t", "will not", AaD_Replies)
AaD_Replies <- gsub("isn t", "is not", AaD_Replies)
AaD_Replies <- gsub("I m", "I am", AaD_Replies)
AaD_Replies <- gsub("It s", "It is", AaD_Replies)

# Make a vector source from AaD_Messages
Reply_source <- VectorSource(AaD_Replies)

# Make a volatile corpus from AaD_source so that changes only affect data stored in R memory
Reply_corpus <- VCorpus(Reply_source)

# Create a function to clean text
clean_corpus <- function(corpus) {
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Remove unnecessary words
  corpus <- tm_map(corpus, removeWords, words = stops)
  return(corpus)
}

# Run AaD_corpus through clean_corpus
Reply_Clean <- clean_corpus(Reply_corpus)

# Create a term-document matrix from the corpus to count the number of unique words in each string
Reply_tdm <- TermDocumentMatrix(Reply_Clean)

# Convert to matrix
Reply_m <- as.matrix(Reply_tdm)

# Calculate the row sums of AaD_m
term_frequency <- rowSums(Reply_m)

# Sort term_frequency in decreasing order
term_frequency <- sort(term_frequency, decreasing = TRUE)

# Create a vector of terms
terms_vec <- names(term_frequency)

# Create a wordcloud of all words used twice or more
wordcloud(terms_vec, term_frequency, min.freq = 2, colors = c("blue4", "cyan4", "deepskyblue2"))