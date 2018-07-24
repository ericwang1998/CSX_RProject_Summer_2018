library(tm)
library(magrittr)
library(tidytext)
library(Matrix)
library(factoextra)
library(stringr)
library(readr)
library(tidyr)
setwd("~/GitHub/CSX_RProject_Summer_2018/week_3/songs_data")
files_list <- list.files(path = ".", recursive = TRUE,
                         pattern = "*.txt$", full.names = TRUE)
#load in the txt files
songs <- lapply(files_list, read_file) %>%
  unlist(.)

#change data into corpus
df <- as.data.frame(songs)
corpus_song <- Corpus(VectorSource(df$songs))

#some slight cleaning
song_words <- corpus_song %>%
  tm_map(removePunctuation)  %>%
  tm_map(removeNumbers)

#obtain DTM
dtm <- DocumentTermMatrix(song_words)
inspect(dtm)

#create matrix of words and their count in the different texts
doc_tf <- apply(as.matrix(dtm), 2, function(word) {word/sum(word) })

#create idf function
idf <- function(doc) {
  return(log2( length(doc) + 1 / nnzero(doc)) )
}
#apply function, calculating idf using tdm
doc_idf <- apply(as.matrix(dtm), 1, idf)

docs_tfidf <- doc_tf * doc_idf
head(docs_tfidf)

## PCA
docs_pca <- prcomp(docs_tfidf, scale = T)


##Drawing 2
fviz_eig(docs_pca)
fviz_pca_ind(docs_pca, geom.ind = c("point"), col.ind = "cos2")
fviz_pca_var(docs_pca, col.var = "contrib")
fviz_pca_biplot(docs_pca, geom.ind = "point")

docs_eig <- get_eig(docs_pca)
docs_var <- get_pca_var(docs_pca)
docs_ind <- get_pca_ind(docs_pca)

#kmeans
ind_coord2 <- docs_ind$coord[,1:2]
wss <- c()
for (i in 1:10) {
  wss[i] <- kmeans(ind_coord2, i)$tot.withinss
}
plot(wss, type = "b")

#clustering
km <- kmeans(ind_coord2, 3)
plot(ind_coord2, col = km$cluster)
points(km$centers, col = 1:3, pch = 8, cex = 2)