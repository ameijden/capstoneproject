
## Importing txt files (English)

## directory <- "C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final"

## Enlishblogs <- read.table(paste0(directory,"/en-US/en_US.blogs.txt"), quote="\"", comment.char="")
## Englishblogs <- read.table(paste0(directory,"/en-US/en_USblogs.txt"))


en_US.blogs <- read.delim("C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/en_US.blogs.txt", header=FALSE)


en_US.news <- read.delim("C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/en_US.news.txt")


en_US.twitter <- read.delim("C:/Users/ameij/OneDrive/Bureaublad/Coursera/R/courseR/Statistical Inference/capstoneproject/en_US/en_US.twitter.txt")


## Example from Week 1, task 1
con <- file("C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/en_US.twitter.txt", "r") 
a <- readLines(con, 1) ## Read the first line of text 
b <- readLines(con, 1) ## Read the next line of text 
c <- readLines(con, 5) ## Read in the next 5 lines of text 
d <- readLines(con, 100) ## Read in the next 5 lines of text 
e <- readLines(con, 500)
f <- readLines(con, 899000)
close(con) ## It's important to close the connection when you are done. See the connections help page for more information.

## Task 1
## Tasks to accomplish

####11 Tokenization - identifying appropriate tokens such as words, punctuation, and numbers.
####    Writing a function that takes a file as input and returns a tokenized version of it.
## https://www.rdocumentation.org/packages/quanteda/versions/0.99.12/topics/tokenize

library(quanteda)
enblogstokenized <- tokenize_word(en_US.blogs)

tokenizeda <- tokenize_word(a)
tokenizedb <- tokenize_word(b)
tokenizedc <- tokenize_word(c)
tokenizedd <- tokenize_word(d)
tokenizede <- tokenize_word(e)
tokenizedf <- tokenize_word(f)


### 111  Sampling.You can use the rbinom function to "flip a biased coin" to determine whether 
####      you sample a line of text or not.

### Sampling, To reiterate, to build models you don't need to load in and use all of the data.
### use the rbinom function to "flip a biased coin" to determine whether you sample a line of text or not

## Input -> line number (how do we get this number?)
## Function -> decide if we want to include this line (flip a coin) , success
## Output -> Readline or not


Takelineornot <- function(number) {
 takeline <-  rbinom(number, 1, 0.5) ## Biased coin with number of success on 50%
 return(takeline)
}

Takelineornot(1) ## test -> it works
## input is the next line 

## output of takelineornot -> 0, don't take line if 1 then readLine

con2 <- file("C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/en_US.twitter.txt", "r") 
c2 <- readLines(con2, Takelineornot(1))
x <- 1
c3 <- readLines(con2, lapply(Takelineornot(x)))
c4 <- sapply(readLines (con2, Takelineornot(x)))
close(con2)


####### Example (https://stackoverflow.com/questions/37923041/how-to-read-certain-lines-of-a-data-file-into-r)
con3 <- file("test1.txt", "r")
lines <- c()
while(TRUE) {
  line = readLines(con3, 1)
  if(length(line) == 0) break
  else if(grepl("^\\s*F{1}", line) && grepl("(0,0)", line, fixed = TRUE)) lines <- c(lines, line)
}


lines
close(con3)



#### This works!!!  Keep in mind that the line numbers don't match the orginal line numbers
con5 <- file("C:/Users/ameij/OneDrive/Bureaublad/Capstone project/Coursera-SwiftKey/final/en_US/test10.txt", "r") 
lines <- c()
while(TRUE) {
  line = readLines(con5, x)
  if(length(line) == 0) break
  else if(Takelineornot(x) == 1) ## So only read the line if the coin is 1 (and not null)
  lines <- c(lines, line)
 
}
lines

close(con5)

### 

?rbinom

patternb <- "you"
testtokenb <- grep(patternb, tokenizeda, value = TRUE)

str(tokenizeda)

tokenizeda2 <- as.character(tokenizeda)
testtokenb2 <- grep(patternb, tokenizeda2, value = TRUE)
str(tokenizeda2)

tokenizeda3 <- as.vector(tokenizeda)
testtokenb3 <- grep(patternb, tokenizeda3, value = TRUE)
str(tokenizeda3)

tokenizeda4 <- unlist(tokenizeda)
testtokenb4 <- grep(patternb, tokenizeda4, value = TRUE) ## success!!!
str(tokenizeda4)

tokenizedc1 <- unlist(tokenizedc)
testtokenc1 <- grep("it", tokenizedc1, value = TRUE) 

tokenizedd1 <- unlist(tokenizedd)
tokenizede1 <- unlist(tokenizede)
tokenizedf1 <- unlist(tokenizedf)


patternprof1 <- "[Ff][Uu][Cc][Kk]"
fwordinblog <- grep(patternprof1, tokenizedf1, value = TRUE) ## success!!!

## Profanity filtering - removing profanity and other words you do not want to predict.
## Next step is changing these words or replacing them with something else.
## Step1 test on small text
replaceyou <- gsub("you", "replaced", texta) ## seems to work

## How to replace Fuck? with nothing leaving mothafa or ing or in or with --- or with ???
replacefuck <- gsub( "[Ff][Uu][Cc][Kk]", "----", tokenizedf1)


str(tokenizedc1)
summary(tokenizedc1)

summary(enblogstokenized2)
summary(enblogstokenized3)
summary(enblogstokenized4)
summary(enblogstokenized5)
summary(enblogstokenized6)


head(tokenizedc1)
head(enblogstokenized2)
head(enblogstokenized3)

enblogstokenized2 <- unlist(enblogstokenized)
str(enblogstokenized2)
enblogstokenized3 <- as.character(enblogstokenized2)
str(enblogstokenized3)

enblogstokenized4 <- as.character(enblogstokenized)
str(enblogstokenized4)
enblogstokenized5 <- unlist(enblogstokenized4)
str(enblogstokenized5)
enblogstokenized6 <- toString(enblogstokenized5)
str(enblogstokenized6)



patternprof1 <- "[Ff][Uu][Cc][Kk]"
fwordinblog <- grep(patternb, enblogstokenized3, value = TRUE)
youinblog <- grep(patternb , enblogstokenized2, value = TRUE)

patternblog <- "most"
mostinblog <- grep(patternblog, enblogstokenized2, value = TRUE)

fwordinblog <- grep(patternprof1 , enblogstokenized3, value = TRUE)

############### history ########################################



vectortokenizeda <- as.vector(tokenizeda)
dftokenizeda <- as.data.frame(tokenizeda)

library(quanteda)
myCorpus <- corpus(data_char_ukimmig2010)
summary(myCorpus)

# extract 1-grams from first document
ngram1 <- tokens(myCorpus[1],n=1)
ngramTable <- as.data.frame(table(ngram1))
# count the number of distinct 1-grams, matching Types for BNP text
nrow(ngramTable)
# count total number of 1-grams (i.e. sum the frequencies), matching Tokens
sum(ngramTable$Freq)
# print most frequent 1-grams
tail(ngramTable[order(ngramTable$Freq),])
# count the sentences
sentences <- tokens(myCorpus[1],what = "sentence")
length(sentences[[1]])

####12  Profanity filtering - removing profanity and other words you do not want to predict.

## Search for a word in a text (like fuck)

texta <- c("you", "you're", "your", "youth", "you", "You", "yOu", "yoU")
str(texta)

patternb <- "you"
patternb2 <- "[Yy][Oo][Uu]"


testtexta <- grep(patternb, texta)
testtexta 
testtextb <- grep(patternb, texta, value = TRUE)
testtextc <- grep(patternb2, texta, value = TRUE)

testb <-  grep(patternb, b, value = TRUE) 
testb2 <-  grep(patternb, tokenizedb, value = TRUE) 

tokenizedbstring <- toString(tokenizedb)
tokenizedbstring2 <- as.character(tokenizedbstring)


testb3 <-  grep(patternb, tokenizedbstring2 , value = TRUE) 


## Are the backslashes a problem? Trying to remove them
testb4 <- sub("\\\\", "", "testb2")

testb4 <- replace(\\, tokenizedstring2, "")

str(testb3)

?grep
pattern <- "[Ff][Uu][Cc][Kk]"
patterna <- "fuck" 
patternb <- "you"
patternc <- "^[Ff][Uu][Cc][Kk]"


a2<- grep(patterna, en_US.blogs, value = TRUE)
a3<- grepl(patterna, en_US.blogs)
a21
a22 <- grep(patternb, b)
a23 <- grep(patternb, enblogstokenized, value = TRUE)

a2<- grep(patterna, enblogstokenized , value = TRUE)

a5 <- regexpr(patterna, en_US.blogs)

a <- grep("[Ff][Uu][Cc][Kk]", en_US.blogs)
b <- grep("[Ff][Uu][Cc][Kk]", en_US.news)
c <- grep("[Ff][Uu][Cc][Kk]", en_US.twitter)
d <- grep("[Ff].[Cc][Kk]", en_US.blogs)
e <- grep("[Ff].[Cc][Kk]", en_US.news)
f <- grep("[Ff].[Cc][Kk]", en_US.twitter)

ax <- grep("fuck", en_US.blogs, value = TRUE)
ax3 <-grep('fuck', en_US.blogs, value = TRUE)
ax4 <- grep("you", dftokenizeda , value = TRUE)
ax1 <- grep(patternc, enblogstokenized, value = TRUE)
ax1 <- grep(patternc, en_US.blogs, value = TRUE)

g <- grep("asshole", en_US.blogs)
h <- grep("bitch", en_US.blogs)
i <- grep("shit", en_US.blogs)
j <- grep("shit", en_US.twitter)