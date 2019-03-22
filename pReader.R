library(pdftools)
library(dplyr)
library(tidytext)
library(tm)
library("string")

files <- list.files("data",recursive = T)

classes <- strsplit(files[[1]][1],"/")
        

doc <- sapply(paste0("data/",files),pdf_text)
myDataset <- data.frame(stringsAsFactors = FALSE)

datasetTemp <- data.frame(id <- 1:length(filename)[1], text=filename, class=str_split(doc),stringsAsFactors = F)
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
        texto <- tm_map(documents, removeWords,stopwords("pt"))$content
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


