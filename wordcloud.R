# Install packages
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

# Load packages
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# set file directory
setwd("/Users/gandes.goldestan/Downloads/wordcloud")

#read dataset text "finance.csv"
berita <- read.csv("finance.csv", header = TRUE,sep=";")
View(berita)

#load the content as corpus
docs <- Corpus(VectorSource(berita[1:5000,1]))
inspect(docs)

# transforming text with tm_map
# replace "/", "@", dan "|" with space
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ",x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "https")
docs <- tm_map(docs, toSpace, "com")

# converting text to lowercase
docs  <- tm_map(docs, content_transformer(tolower))

# removing numbers
docs <- tm_map(docs, removeNumbers)
# removing stopwords (bahasa Indonesia)
stopwords_id <- readLines(file.choose())
typeof(stopwords_id)
docs <- tm_map(docs, removeWords, stopwords_id)
# removing punctuations
docs <- tm_map(docs, removePunctuation)
# eliminating white space
docs <- tm_map(docs, stripWhitespace)


# Create Term-Document Matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 15)

# create wordcloud
set.seed(12)
X11()
wordcloud(words=d$word, freq = d$freq, min.freq = 100,
          max.words = 100, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

# plotting most frequent words into bar graph
windows()
barplot(d[1:10,]$freq, las=2, names.arg = d[1:10,]$word, 
        col = "lightblue", main="Most frequent words", ylab = "word frequencies")
