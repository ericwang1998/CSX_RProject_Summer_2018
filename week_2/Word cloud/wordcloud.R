#This worker initializes jiebaR workers.. what are they?
library(jiebaR)
library(wordcloud)
library(magrittr)
wk <- worker()

#import text
text <- readLines("Mayunspeech1.txt", encoding = "UTF-8")

#place all the characters we don't want into a vector
stopwords <- c("我", "的", "在", "和", "都", "也", "不", "就", "上", "为", "我们", "这", "从","是", "会", "个","有","像","把","其实")
#make them into one whole string separated by '|'
stopwords.pattern <- paste0(stopwords, sep = "|", collapse = "") %>%
  substr(1, nchar(.) -1)
# the one above takes them out. 

#Pull the trigger. So if the text any of the phrases in the string(since they have the "or" sign), they will be removed.
text <- gsub(stopwords.pattern, " ", text)

#breaks apart the words.
speech <- wk[text]

#Arrange characters into table and counts into a data frame 
speech <- as.data.frame(table(speech))

#Ordering the phrases in terms of frequencies in decreasing order
freq <- speech[order(speech$Freq, decreasing = T),]

#Drawing the wordcloud
wordcloud(freq$speech, freq$Freq,
          min.freq = 2, random.order = F,
          color = brewer.pal(8, "Dark2"))
