library(dplyr)
library(tidytext)
library(ggplot2)
library(tm)
library(dtplyr)
library("data.table")

df1 <- readChar(con = "doc1.txt",nchars = 100)
df2 <- readChar(con = "doc2.txt",nchars = 100)
df3 <- readChar(con = "doc3.txt",nchars = 100)
df4 <- readChar(con = "doc4.txt",nchars = 100)

df1 <- cbind("doc1.txt",df1,"1")
df2 <- cbind("doc2.txt",df2,"2")
df3 <- cbind("doc3.txt",df3,"3")
df4 <- cbind("doc4.txt",df4,"4")

myDataset <- data.frame(stringsAsFactors = FALSE)

datasetTemp <- as.data.frame(rbind(df1,df2,df3,df4),stringsAsFactors = F)
names(datasetTemp) <- c("id","text","class")
datasetTemp
ct <- 0         # counter to read files
for(ind in 1:length(datasetTemp$id)) {
        texto <- datasetTemp$text[ind]
        texto <- paste(texto,collapse = " ")
        texto <- gsub("<.*?>", "", texto)
        documents <- Corpus(VectorSource(texto))
        documents = tm_map(documents, tolower)
        documents = tm_map(documents, removePunctuation)
        texto  = tm_map(documents, removeNumbers)$content
        #texto <- tm_map(documents, removeWords,stopwords("en"))$content
        myDataset <- rbind(myDataset,c(datasetTemp$id[ind],datasetTemp$class[ind], 
                                       t(texto)),deparse.level = 0, stringsAsFactors =  FALSE)
        
}

myDataset <- data.frame(lapply(myDataset,as.character), stringsAsFactors = FALSE)
colnames(myDataset) <- c("file","class","text")
book_words <- myDataset %>%
        unnest_tokens(word, text,to_lower = TRUE) %>%
        count(file,word, sort = TRUE) %>% ungroup() %>% as.data.frame()


book_words <- book_words[order(book_words$file),]

names(book_words) <- c("file","word","f")
conta <- function(x) {
        y <- 0
        for(i in 1:length(x)) {
              y <- y+1  
        }
        return(y)
}

bind_tfidf <- function(files,f,n) {
        nfiles <- length(unique(book_words$file))
        nfiles <- rep(nfiles,length(book_words$file))
        tf <- 1+log2(f)
        idf <- log2(nfiles/n)
        tf_idf <- tf * idf
        book_words$df <- tf
        book_words$idf <- idf
        book_words$tf_idf <- tf_idf
}

total_file <- book_words
total_file <- aggregate(total_file,by = list(total_file$word),conta)

total_file <- total_file[,-2:-3]
names(total_file) <- c("word","f")
book_words <- left_join(book_words, total_file, by = "word")
names(book_words) <- c("file","word","n","f")
head(book_words)


# Create matrix with TF-IDF
book_words$tf_idf <- bind_tfidf(book_words$file,book_words$f,book_words$n)
print(book_words)


