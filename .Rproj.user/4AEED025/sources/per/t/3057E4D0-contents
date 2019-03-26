library(pdftools)
library(dplyr)
library(tidytext)
library(tm)
library("stringr")

classes <- data.frame(class=c(list.files("data",recursive = F)),stringsAsFactors = F)
docs <- data.frame()
doc <- data.frame()
for(myClass in classes$class) {
        files <- list.files(paste0("data/",myClass),recursive = F)
        docs <- sapply(paste0("data/",myClass,"/",files),pdf_text)
        for(i in 1:length(docs)) {
                docLn <- toString(docs[[i]])
                doc <- rbind(doc,data.frame(text=docLn,class=myClass))
                doc$text <- as.character(doc$text)
                doc$class <- as.character(doc$class)
        }
}


doc <- data.frame(id=1:length(doc$class),text=doc$text,class=doc$class,stringsAsFactors = F)
myDataset <- data.frame(stringsAsFactors = FALSE)

datasetTemp <- data.frame(doc,stringsAsFactors = F)
datasetTemp$text <- as.character(datasetTemp$text)
names(datasetTemp) <- c("id","text","class")
head(datasetTemp)
ct <- 0         # counter to read files
for(ind in 1:length(datasetTemp$id)) {
        texto <- datasetTemp$text[ind]
        texto <- paste(texto,collapse = " ")
        texto <- gsub("<.*?>", "", texto)
        documents <- Corpus(VectorSource(texto))
        documents = tm_map(documents, tolower)
        documents = tm_map(documents, removePunctuation)
        texto  = tm_map(documents, removeNumbers)$content
        texto <- tm_map(documents, removeWords,stopwords("en"))$content
        myDataset <- rbind(myDataset,c(datasetTemp$id[ind],datasetTemp$class[ind], 
                                       t(texto)), stringsAsFactors =  FALSE)
        
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
head(book_words)
tail(book_words)



