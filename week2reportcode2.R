#### Code for week 2 report

## Use git bash to see number of lines. 
###  cd "/c/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/"
### wc -l en_US.news.txt
### 1010242 lines in this file
### 34365905 words in this file

### The only decent work I could review: https://rstudio-pubs-static.s3.amazonaws.com/297058_7f72243dddd741eea84d5e405c46f5d0.html
### Second check, this is an old one made in 2017
## Nice analysis 
## https://robianson.github.io/cs-ds-capstone/Week-2-Milestone/  

## Input from Francesco
## https://rpubs.com/frncscm/585288

## INput from Axel Perruchoud
##  https://rpubs.com/moya/206223

## Input from Greg
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/capstone-ngramComputerCapacity.md



#### To do this for all files (m=characters, w=words, l=lines)

## wc -mlw en_US.news.txt en_US.blogs.txt en_US.twitter.txt

### 1010242  34365905 205243643 en_US.news.txt
###  899288  37333958 208623085 en_US.blogs.txt
### 2360148  30357171 166843164 en_US.twitter.txt
### 4269678 102057034 580709892 total


con <- file("C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/en_US.news.txt", "r") 
news <- readLines(con, 1010242) ## Read all the lines from the news text
close(con)

library(quanteda)
newstokenized <- tokenize_word(news)

newstokenized2 <- unlist(newstokenized)

newstokenized3 <- as.data.frame(newstokenized2)

patternprof6 <- "ahole|arsehole|asshole|bimbo|bitch|blowjob|bollock|bullshit|cunt|dipshit|ejaculate|faggot|fuck|jackass|nigga|nigger|penis|pussy|slut|whore"

profinnews2 <- grep(patternprof6, newstokenized2, value = TRUE, ignore.case = TRUE)
unique(profinnews2) ## only 15 words

## So we have some Bitch and Bitches, slut, whore, penis but 
## also a lot of Analysists, Dick (probably a name), Arsenal, tetanus
## So we remove anus, anal, arse, dick, shit
## There are now 21 words in the entire text we need to remove

rm(news) ## We don't use these variable anymore
rm(newstokenized) ## We don't use these variable anymore (is about 200 Mb)

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
