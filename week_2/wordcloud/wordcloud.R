source('pttTestFunction.R')
#create vector containing 1 to 10
id = c(4430:4434)

#defining the URL to capture the words
URL = paste0("https://www.ptt.cc/bbs/Stock/index", id,".html")

#degine the name of the file to be saved in.
filename = paste0(id, ".txt")

#Run the variables above into the function
pttTestFunction(URL[1], filename[1])
#Using mapply to run the loop.
mapply(pttTestFunction, 
       URL = URL, filename = filename)

#What is this removing?
rm(list = ls(all.names = TRUE))
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)

#creating a list of character vector of the names of files. In the working directory.
filenames <- list.files(getwd(), pattern = "*.txt")

files <- lapply(filenames, readLines)
docs <- Corpus(VectorSource(files))

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
docs <- tm_map(docs, toSpace, ",")
docs

mixseg = worker()
jieba_tokenizer = function(d){
  unlist(segment(d[1], mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[order(freqFrame$Freq, decreasing = TRUE), ]
library(knitr)
kable(head(freqFrame, format = "markdown"))