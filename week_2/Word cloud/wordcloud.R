#This worker initializes jiebaR workers.. what are they?
library(jiebaR)
library(wordcloud2)
library(magrittr)
Sys.setlocale("LC_ALL", locale = "Chinese")
wk <- worker()

#import text
text <- readLines("Mayunspeech1.txt", encoding = "UTF-8")

#place all the characters we don't want into a vector
stopwords <- c("我","的","在","和","都","也","不","就","上","为","我们","你们","他们","这","从","是",
               "会","个","有","像","把","其实","了", "谁","所","但", "更", "很","什么", "一","因", "让",
               "好", "每", "多", "得", "当", "将","座", "对", "没", "你","到")
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
str(freq)

#Drawing the wordcloud
wordcloud2(freq, size = 1.5, color = "random-light", backgroundColor = "grey")
