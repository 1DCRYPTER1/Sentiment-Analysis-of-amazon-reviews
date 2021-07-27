library(tm)
library(wordcloud)
library(syuzhet)
library(plotrix)


reviews <- read.csv(file.choose(),header=T)

View(reviews)

mean(reviews$rating)
mean(reviews$rating, trim=0.5)

median(reviews$rating)

quantile(reviews$rating)

head(reviews)

str(reviews)

dim(reviews)

corpus <- iconv(reviews$text)
corpus <- Corpus(VectorSource(corpus))

inspect(corpus[1:5])

corpus <- tm_map(corpus,tolower)

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, removeWords, c("given","bought","read","book","important","flow","ikigai","find","meaning"))

corpus <- tm_map(corpus, stripWhitespace)

inspect(corpus[1])

#inspect(corpus[1:5])

reviews_final <- corpus

#Creating a Term Document
tdm <- TermDocumentMatrix(reviews_final)
tdm <- as.matrix(tdm)
tdm[1:10,1:5]

#Barplots ke liye
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w, las = 2, col = "blue")

#Wordcloud ke liye 
w <- sort(rowSums(tdm), decreasing = T)
set.seed(2000)
wordcloud(words = names(w),
          freq = w,
          max.words = 50,
          random.order = T,
          min.freq = 5,
          colors = brewer.pal(25,"Dark2"),
          scale = c(3,0.3))


sentiment_data <- iconv(reviews$text)
s <- get_nrc_sentiment(sentiment_data)
s[1:10,]

s$score <- s$positive - s$negative
s[1:10,]

#storing scores in csv file
write.csv(x = s, file = "C:/Users/Shaun Dsilva/Desktop/SEM 8/R/Amazon Reviews/Final_score_ikigai.csv")


#check overall sentiments
review_score <- colSums(s[,])
print(review_score)


#Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment on Reviews')


neg_score <- colSums(s[9])
pos_score <- colSums(s[10])
anger_score <- colSums(s[1])
fear_score <- colSums(s[4])
sadness_score <- colSums(s[6])
joy_score <- colSums(s[5])



x <- c(neg_score,pos_score,anger_score,fear_score,sadness_score,joy_score)
labels <- c("Negative", "Positive", "Anger", "Fear", "Sadness", "Joy")
#pie(x,labels,main = "Pie Chart Of Sentiments", col = rainbow(length(x)))


#just try
pie3D(x,labels = labels,explode = 0.1, main = "Pie Chart of Sentiments ")


