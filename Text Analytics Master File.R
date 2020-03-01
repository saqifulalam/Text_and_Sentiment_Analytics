#This is a r script for analysis of textual data. The script works with 2 data files, the reviews of amazon
# employees in their workplace, and the review of google employees about their work place

#This project is using a very structured data, so this might look easy, but in reality coming to the clean part
# is the bigger challenge

#lets start by loading the basic libraries

library(tidytext)
library(tidyverse)
library(tm)

#loading the data set - name the data set amzn, and also do not import with strings as factors, here 
#strings need to be strings

#creating a list of reviews 

amzn <- na.omit(amzn)

amzn$rev_id <- 1:nrow(amzn)

amzn_pros <- select(amzn, rev_id, pros)
amzn_cons <- select(amzn, rev_id, cons)

              #### WORD FREQUENCY ANALYSIS AND VISUALIZATIONS ####

#Simple way of finding out the word frequencies. 

pros_frequency <- amzn_pros %>% unnest_tokens(output = word, input = pros) %>% anti_join(stop_words) %>% count(word) %>% filter(!is.na(word)) %>% arrange(desc(n))
cons_frequency <- amzn_cons %>% unnest_tokens(output = word, input = cons) %>% anti_join(stop_words) %>% count(word) %>% filter(!is.na(word)) %>% arrange(desc(n))

#saving these frequencies in csv files for future reporting
write.csv(cons_frequency, "cons_frequency.csv")
write.csv(pros_frequency, "pros_frequency.csv")

#instead of showing the frequencies in table, we can develop simple wordclouds, where the size of the word is
# the frequency

library(wordcloud)
wordcloud(words = pros_frequency$word, freq = pros_frequency$n, min.freq = 20, colors = "blue")
wordcloud(words = cons_frequency$word, freq = cons_frequency$n, max.words = 20, colors = "red")

#there are other forms of wordclouds which are also very useful. But for this we need to create something called
# document term matrix or term document matrix. Please google and read a brief intro on what they are

# Useful read
#https://www.darrinbishop.com/blog/2017/10/text-analytics-document-term-matrix/

# Useful video
# https://www.youtube.com/watch?v=dE10fBCDWQc


# Another good read
# https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf

# The brief steps are to create VCorpus (also read online) and then create the dtm

# First we will look into something called commonality cloud, where we will see what are common between amazon
# pros and cons

#joining all pros comments
all_pros <- paste(amzn_pros$pros, collapse = " ")

#joining all cons comments
al_cons <- paste(amzn$cons, collapse = " ")

#creating a vector with all pros and all cons
all <- c(all_pros, al_cons)

#converting this vector into a corpus

all_corpus <- VCorpus(VectorSource(all))

# the next step will be to clean the corpus. we define a function here, usingthe code tm_map and built in
# tmfunctions. tm_map basically applies the fucntion inside to the corpus.

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  return(corpus)}

# now applying the function

all_corpus_clean <- clean_corpus(all_corpus)

# the next step is to create the dtm

all_dtm <- TermDocumentMatrix(all_corpus_clean)
colnames(all_dtm) <- c("pros", "cons")

# for finding out the common words, we will create a commonality cloud, which requres a matrix input

all_m <- as.matrix(all_dtm)

# and now the commonality cloud

commonality.cloud(all_m, max.words = 30, colors = "steelblue1")

# similarly we can create a comparison which shows the words that are different in the two sets

comparison.cloud(all_m, colors = c("green", "red"), max.words = 30,title.size = 1)

#besides commonality cloud and comparison cloud, another useful area of analysis is that of word clustering and
# word similarities and pyramid plots and word associations

# First we will look into Pyramid plots, where we will see how many times the common terms in two different
# docs appeared. For this we will need to create a dataframe of the number of terms we want to check for
# For us, lets find out top 20

# 1. Convert into data frame using dplyr, use the rownames to get a column of words. 2. Use filter_all to 
# filter for all values in a row to be greater than 0. 3. Find out the difference of each word count in 2
# groups. 4. Choose top 20 based on the difference

top20df <- all_m %>% as_data_frame(rownames = "word") %>% filter_all(all_vars(. > 0)) %>% mutate(diff = pros - cons) %>% top_n(20, wt = diff) %>% arrange(desc(diff))

# Next step is to create the pyramid plot using the plotrix package

library(plotrix)

pyramid.plot(top20df$pros, top20df$cons, labels = top20df$word, top.labels = c("Pros", "Words", "Coffee"), main = "Words in Common", unit = NULL, gap = 8)

# Word association plots lets you check the connection of one word with the others in a text. There are two
# functions available in the qdap package to carry out these visualizations

library(qdap)

word_associate(text.var = amzn_cons$cons, match.string = "benefit", stopwords = Top200Words, network.plot = TRUE, cloud.colors = c("blue", "red"))

#in order to do heirarchical clustering and dendograms, we need to create sparse TDMs. If you look at a TDM matrxo
# you will see most of the entries are zeros, as it is natural that not all term are in all documents. So if
# we create less sparse TDM, then we will be removing terms that dont appear a lot of times

#lets create 2 separate tdms for pros and cons and then reduce sparcity

pros_corpus <- VCorpus(VectorSource(amzn_pros$pros))

pros_corpus <- clean_corpus(pros_corpus)

pros_tdm <- TermDocumentMatrix(pros_corpus)

pros_tdm

#The sparsity is 99%. We can reduce this sparsity to 95% without removing the critical connections

pros_tdm2 <- removeSparseTerms(pros_tdm, sparse = 0.95)

pros_tdm2

#lets do the same thing for cons

cons_tdm <- TermDocumentMatrix(clean_corpus(VCorpus(VectorSource(amzn_cons$cons))))
cons_tdm

cons_tdm2 <- removeSparseTerms(cons_tdm, sparse = 0.95)
cons_tdm2

#next we need to form a matrix format and then convert to a dataframe to create a dendogram

pros_df <- data.frame(as.matrix(pros_tdm2))

#now the heirarchical clustering leading to dendogram

pros_hc <- hclust(dist(pros_df))

plot(pros_hc)

# doing the same thing for cons

cons_df <- data.frame(as.matrix(cons_tdm2))
cons_hc <- hclust(dist(cons_df))

plot(cons_hc)


#Next we will look into how we can find out word associations between different words, and plot them.
# SUppose we want to know which words are connected to "benefits". We have to mention the level of association
# it is between 0 to 1. 1 means that the words are constantly appearing together. Lets set bar for 0.3

assocs <- findAssocs(pros_tdm, "benefit", 0.3)

#converting the associations to a data frame

assoc_df <- list_vect2df(assocs, col2 = "word", col3 = "score")

#developing a association visualization

library(ggplot2)

ggplot(assoc_df, aes(score, word)) + 
  geom_point(size = 3)

#doing the same thing for the word "stress"

assocs <- findAssocs(cons_tdm, "stress", 0.3)

#converting the associations to a data frame

assoc_df <- list_vect2df(assocs, col2 = "word", col3 = "score")

#developing a association visualization

ggplot(assoc_df, aes(score, word)) + 
  geom_point(size = 3)

# So far, we have only worked with frequencies and associations with a single term in a document. But what if
# we need to look for more than 1 term together? This is the concept of N-Gram toketization, and for that we
# will need a library called RWeka

library(RWeka)

#First we will create a tokenizer function

tokenizer <- function(x){NGramTokenizer(x, Weka_control(min = 2, max = 2))}

# Weka control cannot handle null values, so we need to remove the null values

amzn_pros <- na.omit(amzn_pros)
amzn_cons <- na.omit(amzn_cons)

pros_corpus <- clean_corpus(VCorpus(VectorSource(amzn_pros$pros)))
cons_corpus <- clean_corpus(VCorpus(VectorSource(amzn_cons$cons)))

pros_dtm_2t <- DocumentTermMatrix(pros_corpus, control = list(tokenize = tokenizer))
cons_dtm_2t <- DocumentTermMatrix(cons_corpus, control = list(tokenize = tokenizer))

#Now to create wordclouds and check what else can we see

pros_2t_m <- as.matrix(pros_dtm_2t)

freq <- colSums(pros_2t_m)
t2_terms <- names(freq)

wordcloud(t2_terms, freq, max.words = 20)

cons_2t_m <- as.matrix(cons_dtm_2t)

freq <- colSums(cons_2t_m)
t2_terms <- names(freq)

wordcloud(t2_terms, freq, max.words = 20)

# This concludes the word frequency analysis. Next we will check Sentiment Analysis

                    #### Sentiment Analysis ####

# A very important use of text analytics is to identify the sentiment of the text document or parts of the
# text document. By sentiment we mean whether the text had a positive connotation or a negative connotation
# A very popular use of sentiment analysis is to track news and then see how the news sentiment is affecting
# stock performance in the stock market. For our instructions, we will still use the amazon workplace reviews
# and see what is the sentiment of these reviews. Although we know the positive and the negative reviews 
# that are separated, for our case we can merge them all together, and shuffle, in order to create a data
# with mixed reviews

names(amzn_cons) <- c("id", "reviews")
names(amzn_pros) <- c("id", "reviews")

amzn_revs <- rbind(amzn_cons, amzn_pros)

#removing NA's

amzn_revs <- na.omit(amzn_revs)

# shuffling rows

r_rows <- sample(1:nrow(amzn_revs), nrow(amzn_revs))

amzn_revs <- amzn_revs[r_rows, ]

#now we are ready to carry out our sentiment analysis. The first library we will use is the qdap, and find out
# the polarity of the whole document

library(qdap)
library(magrittr)

#generating overall polarity

amzn_revs %$% polarity(text.var = reviews)

# now generating polarity per review

rev_polarity <- amzn_revs %$% polarity(reviews, id)

# we can see that a list has been created. Now to get a dataframe with polarity scores

rev_polarity_df <- rev_polarity$all

plot(rev_polarity)

# although the polarity function in qdap library is quite powerful, we could do more and easily do it using
# the tidyverse package tidytext. So next we will look into how to work with sentiment analysis using tidytext

#first we need to create a Document Term Matrix

library(tm)
library(tidyverse)
library(tidytext)

rev_dtm <- DocumentTermMatrix(clean_corpus(VCorpus(VectorSource(amzn_revs$reviews))))

#converting to tidy format

rev_tidy <- tidy(rev_dtm)

# a look at the rev_tidy will show how this can be very useful for processing

# Similar to the library used for the polarity funciton in qdap is the bing library in tm

bing <- get_sentiments("bing")

rev_bing_words <- rev_tidy %>% inner_join(bing, c("term" = "word"))

#This will give us a list of positive and negative terms in each of the documents

#Counting total sentiment words

rev_bing_words %>% count(sentiment)

# number of positive and negative words in each document

count_per_index <- rev_bing_words %>% mutate(index = as.numeric(document)) %>% count(sentiment, index)

# from here using dplyr commands we can easily find out which docs have max pos and neg words

# this is a tall format file. We can make it more readable by spreading it

rev_words_wide <- count_per_index %>% spread(sentiment, n, fill = 0)

#using this format we can create a viz which shows the net sentiment over the documents, in the form of a
# time series. This will help if the document had sequential text information like news, and see how the
# sentiment has varied over time

rev_words_wide %>% mutate(polarity = positive - negative) %>% ggplot(aes(x = index, y = polarity)) + geom_smooth()

# Bing only records words are positive or negative. But we know that different words has different pos and neg
# strength. There is another library that measures whether each word is negative or positive and also the strenth
# of pos and neg

afinn <- get_sentiments("afinn")

# a df showing count of scores for each document
rev_afinn <- rev_tidy %>% inner_join(afinn, c("term" = "word")) %>% count(value, document)

# getting the sentiment score for each document

rev_afinn_score <- rev_afinn %>% group_by(document) %>% summarize(total_score = sum(value*n))

#plotting the data

ggplot(rev_afinn_score, aes(x = as.numeric(document), y = total_score)) + geom_smooth()

#Now that we have looked in to positive and negative sentiments, we can go further. There is an idea of Plutchik
# wheel of emotions that illustrates that any emotion conveyed can have 8 different emotions' mix. These 
# separate emotions are stored in the NRC dictionary in tidytext

nrc <- get_sentiments("nrc")

rev_nrc_agg <- rev_tidy %>% inner_join(nrc, c("term" = "word")) %>% group_by(sentiment) %>% summarise(total_count = sum(count))

# am added step could be to remove the words that are only labeled pos and neg, as they dont show the emotions

rev_nrc_agg <- rev_nrc_agg %>% filter(!sentiment %in% c("positive", "negative"))

# now time for a viz

ggplot(rev_nrc_agg, aes(x = sentiment, y = total_count, fill = sentiment)) + geom_col()

#another useful visualization can be checking which terms in a document are contributing to what lever of polarity
# to do this, we will merge the tidy DTM with bing, count by id and sentiment, spread, calculate polarity,
# take those with high polarity, and then plot

rev_bing_pol <- rev_tidy %>% inner_join(bing, c("term" = "word")) %>% count(term, sentiment) %>% spread(sentiment, n, fill= 0) %>% mutate(polarity = positive - negative) %>% filter(abs(polarity) > 20) %>% mutate(pos_or_neg = ifelse(polarity > 0, "positive", "negative")) %>% arrange(polarity)

ggplot(rev_bing_pol, aes(x = reorder(term, polarity), y = polarity, fill = pos_or_neg)) + geom_col()

#We have looked into creating word clouds from term frequencies. What about creating wordclouds involving sentiments?

library(wordcloud)

rev_bing_wc <- rev_tidy %>% inner_join(bing, c("term" = "word")) %>% count(term, sentiment) %>% spread(sentiment, n, fill= 0) %>% data.frame(row.names = "term")

comparison.cloud(rev_bing_wc, max.words = 50, colors = c("red", "blue"))

rev_nrc_wc <- rev_tidy %>% inner_join(nrc, c("term" = "word")) %>% count(term, sentiment) %>% filter(!sentiment %in% c("positive", "negative")) %>% spread(sentiment, n, fill= 0) %>% data.frame(row.names = "term")

comparison.cloud(rev_nrc_wc, max.words = 50, title.size = 1)

# Another useful chart for NRC is the radar chart for each emotion

rev_nrc_radar <- rev_tidy %>% inner_join(nrc, c("term" = "word")) %>%  filter(!sentiment %in% c("positive", "negative")) %>% count(sentiment)

chartJSRadar(rev_nrc_radar)

# another shortcut to a polar chart to repeat the column nrc chart with an added code

ggplot(rev_nrc_agg, aes(x = sentiment, y = total_count, fill = sentiment)) + geom_col() + coord_polar()



                  #### Topic Modeling ####

# Now that we have looked into word frequncy analysis and sentiment analysis, we are ready to work with topic
# modeling. This area of text analytics is still very fresh in development and requires a lot of trial and 
# errors before generating proper insight. The intuition of topic models is simple - every word document
# has the possibility of talking about a certain topic. If we get a collection of word documents and we want
# to see which topic each of these documents belong and what are the topics they are talking about, then we
# need to carry out topic modeling. The problem is it is a unsupervised algorithm, and cannot find the optimum
# number of topics on its own. As such we need to iterate over the number of topics.

# For any topic modeling, we need a DTM, so lets clean and create our dtm again

library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)

pros_dtm <- DocumentTermMatrix(clean_corpus(VCorpus(VectorSource(amzn$pros))))
cons_dtm <- DocumentTermMatrix(clean_corpus(VCorpus(VectorSource(amzn$cons))))

#First we will assume that both  the pros and the cons are talking about 3 topics. So we will fit topic modeling
# for 3 topics for both the dtm. We will use a Markov Chain Algorithm known as Gibbs

pros_mod_3 <- LDA(pros_dtm, k = 3, method = "Gibbs")
cons_mod_3 <- LDA(cons_dtm, k = 3, method = "Gibbs")

# Figuring out which are the top 5 words in each topic

terms(pros_mod_3, k=5)

# we can also use tidyverse to get a dataset of words and their probabilities in each of the documents

topic_terms <- tidy(pros_mod_3, matrix = "beta") %>% spread(topic, beta)

# similarly we can also see the probabilities of documents belonging to each of these topics

topic_documents <- tidy(pros_mod_3, matrix = "gamma") %>% spread(topic, gamma, fill = 0)

# You can see that using this last data, we can divide our reviews into 3 groups, and to our frequency and 
# sentiment analysis

# Now, we still have not discussed that we have just used an arbitrary number of topics (k = 3). But what 
# is the optimum number of topics? There are two steps. First we will look at some numberic outcomes - 
# perplexity (a measure of complication and error, and the more the less accurate the model) and log likelihood
# which is a negative value and the larger the value the better is the fitting of the topics

#to see the log likelihood of the model:

logLik(pros_mod_3)

# and to see the perplexity

perplexity(pros_mod_3, pros_dtm)

# Now lets compare these results for a number of models. So first create vectors for likelihood and perplexity
# and then iterate over k between 2 to 9 and store each of the models likelihood and perplexity


#So using a list and a loop, we have run 9 topic models and stored them on the list. Now we will create a
# Log Likelihood plot and a Perplexity plot

likelihood <- c()
perplx <- c()

for (x in 2:10) {
  mod <- LDA(pros_dtm, method = "Gibbs", k = x)
  likelihood[x-1] <- logLik(mod)
  perplx[x-1] <- perplexity(mod, pros_dtm)
}

k <- 2:10

# now lets plot k versus likelihood and perplexity

plot(x = k, y = perplx)
plot(x = k, y = likelihood)

#if we see both the charts, it seems clear that the most improvement comes from k = 4, and so for the positive
# reviews, k = 4. Now lets go check the topics being created qualitatively and see whether they are actually
# very different in a common sense appeal. If we see even two themes to be similar, then we should try a lower k

pros_mod_4 <- LDA(pros_dtm, method = "Gibbs", k = 4)

#looking at the top 10 terms

terms(pros_mod_4, k = 10)

#looks like there are 4 topics coming out of the reviews - good benefits, great people, fast environment and
# career with amazon!

#lets do a wordcloud with the terms for each of the topics

library(wordcloud)

for (j in 1:4){
  word_freq <- tidy(pros_mod_4, matrix = "beta") %>% mutate(n = trunc(beta * 1000)) %>% filter(topic == j)
  wordcloud(words = word_freq$term, freq = word_freq$n, max.words = 20, colors = c("DarkOrange", "CornflowerBlue", "DarkRed"))
}

# remember the trick here is to convert the word probability (beta) into a integer number by
# multiplying with 1000

#Now if we want to see how the words in the topics are connected to other words, we can create a simple
# netwrok chart connecting the words using a library called semnet. Its not in the CRAN repository,
# so we have install it from github

install.packages("devtools")
library(devtools)
install_github("kasperwelbers/semnet")

library(semnet)
top10terms <- unique(terms(pros_mod_4, k=3))
cooc <- coOccurenceNetwork(pros_dtm[, top10terms])
cooc <- simplify(cooc)
plot(cooc, vertex.size=V(cooc)$freq / 20, edge.arrow.size=0.5)

#The final master step is to create a LDA visualization html, using LDAvis package

library(LDAvis)


json <- createJSON(phi = posterior(pros_mod_4)$terms,
                   theta = posterior(pros_mod_4)$topics,
                   doc.length = row_sums(pros_dtm),
                   vocab = colnames(pros_dtm),
                   term.frequency = col_sums(pros_dtm))

serVis(json)

# Enjoy going through the visualization!! :D
