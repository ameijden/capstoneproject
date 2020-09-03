
## Importing txt files (English)

## directory <- "C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final"

## Enlishblogs <- read.table(paste0(directory,"/en-US/en_US.blogs.txt"), quote="\"", comment.char="")
## Englishblogs <- read.table(paste0(directory,"/en-US/en_USblogs.txt"))


en_US.blogs <- read.delim("C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/en_US.blogs.txt", header=FALSE)


en_US.news <- read.delim("C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/en_US.news.txt")


en_US.twitter <- read.delim("C:/Users/ameij/OneDrive/Bureaublad/Coursera/R/courseR/Statistical Inference/capstoneproject/en_US/en_US.twitter.txt")


## Example from Week 1, task 1
con <- file("C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/en_US.twitter.txt", "r") 
readLines(con, 1) ## Read the first line of text 
readLines(con, 1) ## Read the next line of text 
readLines(con, 5) ## Read in the next 5 lines of text 
close(con) ## It's important to close the connection when you are done. See the connections help page for more information.

## Task 1
## Tasks to accomplish

####11 Tokenization - identifying appropriate tokens such as words, punctuation, and numbers.
####    Writing a function that takes a file as input and returns a tokenized version of it.

####12  Profanity filtering - removing profanity and other words you do not want to predict.

## Search for a word in a text (like fuck)

?grep

a <- grep("[Ff][Uu][Cc][Kk]", en_US.blogs)
b <- grep("[Ff][Uu][Cc][Kk]", en_US.news)
c <- grep("[Ff][Uu][Cc][Kk]", en_US.twitter)
d <- grep("[Ff].[Cc][Kk]", en_US.blogs)
e <- grep("[Ff].[Cc][Kk]", en_US.news)
f <- grep("[Ff].[Cc][Kk]", en_US.twitter)

g <- grep("asshole", en_US.blogs)
h <- grep("bitch", en_US.blogs)
i <- grep("shit", en_US.blogs)
j <- grep("shit", en_US.twitter)