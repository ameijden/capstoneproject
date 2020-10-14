#### Code for building model 

con <- file("C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/en_US.news2.txt", "r") 
news <- readLines(con, 1010242) ## Read all the lines from the news text
close(con)

tail(news)
## Remove [] in the news text, so changed the source code in news2.text


library(quanteda)
newstokenized <- tokenize_word(news)

newstokenized2 <- unlist(newstokenized)

newstokenized3 <- as.data.frame(newstokenized2)

colnames(newstokenized3) <- c("word")

patternprofandsigns <- "[^0-9A-Za-z\' ]|ahole|arsehole|asshole|bimbo|bitch|blowjob|bollock|bullshit|cunt|dipshit|ejaculate|faggot|fuck|jackass|nigga|nigger|penis|pussy|slut|whore"

newsclean <- gsub('[^0-9A-Za-z\' ]','',newstokenized3$word) ## Remove all profanity and everything not being a letter, number, space or '


rm(news) ## We don't use these variable anymore (about 270 mb)
rm(newstokenized) ## We don't use these variable anymore (is about 2,6 Gb)
rm(newstokenized2) ## (615 Mb)
rm(newstokenized3) ## (74316338 obs. of 1 variable)

library(dplyr)
newstogetngram <- as_tibble(newsclean)

colnames(newstogetngram) <- c("word")

rm(newsclean)

## Create a lot of n-grams 
library(tidytext)


newsngram1  <- unnest_ngrams(newstogetngram, words, word, n=1) ## (1 grams)
newsngram2 <- unnest_ngrams(newstogetngram, words, word, n=2) ##  (2 grams)
newsngram3 <- unnest_ngrams(newstogetngram, words, word) ##  (3 grams)
newsngram4 <- unnest_ngrams(newstogetngram, words, word, n=4)
newsngram5 <- unnest_ngrams(newstogetngram, words, word, n=5)
newsngram6 <- unnest_ngrams(newstogetngram, words, word, n=6)
newsngram7 <- unnest_ngrams(newstogetngram, words, word, n=7)

## Two benefits of n-gram models (and algorithms that use them) are simplicity and scalability – with larger n, 
## a model can store more context with a well-understood space–time tradeoff, enabling small experiments 
## to scale up efficiently. https://en.wikipedia.org/wiki/N-gram

### Task 3 - Modelling
## Task 1: build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.
## Task 2: Build a model to handle unseen n-grams 

## Next step, count the number of occurrences
newsngram1count2 <- as.data.frame(table(newsngram1)) 
## newsngram1count3 <- as_tibble(newsngram1count2)


newsngram2count <- as.data.frame(table(newsngram2)) 
head(newsngram2count)

newsngram2countorded <- newsngram2count[order(newsngram2count$Freq, decreasing=TRUE),]
head(newsngram2countorded)
## How can I remove all other instances of the first word so to have only the word with the highest frequency?

rm(newstogetngram)
rm(newsngram4)
rm(newsngram3)
rm(newsngram2)
rm(newsngram1)

library(tidyr)
newsngram2countordedsplitted <- separate(data=newsngram2countorded, col=newsngram2, into=c("first","second"), sep = " ")
head(newsngram2countordedsplitted)




newsngram3count <- as.data.frame(table(newsngram3)) 

newsngram4count <- as.data.frame(table(newsngram4)) 

## Creating a function to predict the next word based on the previous 1, 2, or 3 words
PredictNextWord <- function(Word1) {
  
  nextword <- newsngram2count[Word1,]
  print(nextword)
}


rm(newsngram1)
rm(newsngram2)
rm(newsngram3)
rm(newsngram4)

## If we do this for blog we get:

## 899288 en_US.blogs.txt lines
con2 <- file("C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/en_US.blogs.txt", "r") 
blogs <- readLines(con2, 899288) ## Read all the lines from the blogs text
close(con2)

blogstokenized <- tokenize_word(blogs)

blogstokenized2 <- unlist(blogstokenized)

blogstokenized3 <- as.data.frame(blogstokenized2)

profinblogs2 <- grep(patternprof6, blogstokenized2, value = TRUE, ignore.case = TRUE) ## about 7436
unique(profinblogs2) ## 388 unique words. 

rm(blogs) ## We don't use these variable anymore
rm(blogstokenized) ## We don't use these variable anymore (is about 2,6 Gb)

## If we do this for twitter we get:
## wc -l en_US.twitter.txt

con3 <- file("C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/en_US.twitter.txt", "r") 
twitter <- readLines(con3, 2360148) ## Read all the lines from the blogs text
close(con3)

twittertokenized <- tokenize_word(twitter)

twittertokenized2 <- unlist(twittertokenized)

twittertokenized3 <- as.data.frame(twittertokenized2)

profintwitter2 <- grep(patternprof6, twittertokenized2, value = TRUE, ignore.case = TRUE) ## about 51306
unique(profintwitter2) ## 1565 unique words. 

rm(twitter) ## We don't use these variable anymore
rm(twittertokenized) ## We don't use these variable anymore (is about 2,7 Gb)

## Even the profanity is spelled weird 

## Looking for spelling mistakes in the text
## ((source: https://www.learnenglish.de/spelling/commonspellingmistakes.html))
patterncommonmistakes <- "adress|alot|definately|enviroment|expresso|forteen"

misspelledinnews <- grep(patterncommonmistakes, newstokenized2, value = TRUE, ignore.case = TRUE)
unique(misspelledinnews)

misspelledinnblog <- grep(patterncommonmistakes, blogstokenized2, value = TRUE, ignore.case = TRUE) 
unique(misspelledinnblog)

misspelledintwitter <- grep(patterncommonmistakes, twittertokenized2, value = TRUE, ignore.case = TRUE) 
unique(misspelledintwitter)

### Some plots

library(ggplot2)
library(dplyr)
newstokenized3 %>%
  count(newstokenized2, sort = TRUE) %>%
  filter(n < 5000) %>%  ## to filter out space, is, the, it, not, you, etc. 
  filter(n > 2000) %>%
  ggplot(aes(newstokenized2, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

## Or a treemap with the most used words
Mostusedwordsinnews <- newstokenized3 %>%
  count(newstokenized2, sort = TRUE) %>%
  filter(n < 50000) %>%
  filter(n > 10000)

Mostusedwordsinnews2 <- newstokenized3 %>%
  count(newstokenized2, sort = TRUE) %>%
  filter(n > 50000)

blogstokenized3 %>%
  count(blogstokenized2, sort = TRUE) %>%
  filter(n < 850000) %>%  ## to filter out space, is, the, it, not, you, etc. 
  filter(n > 200000) %>%
  ggplot(aes(blogstokenized2, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


Mostusedwordsinblogs <- blogstokenized3 %>%
  count(blogstokenized2, sort = TRUE) %>%
  filter(n < 850000) %>%
  filter(n > 200000)


Mostusedwordsinblogs2 <- blogstokenized3 %>%
  count(blogstokenized2, sort = TRUE) %>%
  filter(n > 300000)


Mostusedwordsintwitter2 <- twittertokenized3 %>%
  count(twittertokenized2, sort = TRUE) %>%
  filter(n > 450000)

Mostusedwordsintwitter <- twittertokenized3 %>%
  count(twittertokenized2, sort = TRUE) %>%
  filter(n < 450000) %>%
  filter(n > 200000)

twittertokenized3 %>%
  count(twittertokenized2, sort = TRUE) %>%
  filter(n < 450000) %>%  
  filter(n > 200000) %>%
  ggplot(aes(twittertokenized2, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()




ggplot(aes(Mostusedwordsinnews, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


library(treemap)
treemap(Mostusedwordsinnews,
        index=c("newstokenized2"),
        vSize = "n",
        type = "index",
        palette = "Reds",
        title="Most used words in news",
        fontsize.title = 14)



library(treemap)
treemap(Mostusedwordsinblogs,
        index=c("blogstokenized2"),
        vSize = "n",
        type = "index",
        palette = "Blues",
        title="Most used words in blogs",
        fontsize.title = 14)








newstogetngram <- as_tibble(newstokenized3)
str(test10tokenizedquan4)
head(newstogetngram)

colnames(newstogetngram) <- c("word")

## Or / And then
library(tidytext)
library(dplyr)


newsngram3 <- unnest_ngrams(newstogetngram, words, word) ## Seems to work (3 grams)
newsngram2 <- unnest_ngrams(newstogetngram, words, word, n=2) ## Seems to work (2 grams)
newsngram1  <- unnest_ngrams(newstogetngram, words, word, n=1) ## Seems to work (1 grams)
newsngram4 <- unnest_ngrams(newstogetngram, words, word, n=4)
newsngram5 <- unnest_ngrams(newstogetngram, words, word, n=5)
newsngram6 <- unnest_ngrams(newstogetngram, words, word, n=6)


newsngram3 %>%
  count(words, sort = TRUE) %>%
  filter(n > 250) %>%
  ggplot(aes(words, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

newsngram2 %>%
  count(words, sort = TRUE) %>%
  filter(n > 2500) %>%
  ggplot(aes(words, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


blogstogetngram <- as_tibble(blogstokenized3)
colnames(blogstogetngram) <- c("word")
blogsngram3 <- unnest_ngrams(blogstogetngram, words, word) ## Seems to work (3 grams)
blogsngram2 <- unnest_ngrams(blogstogetngram, words, word, n=2) ## Seems to work (2 grams)

blogsngram3 %>%
  count(words, sort = TRUE) %>%
  filter(n > 750) %>%
  ggplot(aes(words, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

blogsngram2 %>%
  count(words, sort = TRUE) %>%
  filter(n > 2500) %>%
  ggplot(aes(words, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


############ testfile

con10 <- file("C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/test10.txt", "r") 
test <- readLines(con10,10) ## Read all the lines from the test
close(con10)

library(quanteda)
testtokenized <- tokenize_word(test)

testtokenized2 <- unlist(testtokenized)

teststokenized3 <- as.data.frame(testtokenized2)

colnames(teststokenized3) <- c("word")


testclean <- gsub('[^0-9A-Za-z\' ]','',teststokenized3$word) ## DUurt te lang eerst even op test doen


patternprof1 <- "the|years|[^0-9A-Za-z\' ]"
testclean3 <- gsub(patternprof1,'',teststokenized3$word)
