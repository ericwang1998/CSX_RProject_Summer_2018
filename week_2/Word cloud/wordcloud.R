#This worker initializes jiebaR workers.. what are they?
library(jiebaR)
wk <- worker()

#import text
text <- readLines("Mayunspeech1.txt", encoding = "UTF-8")

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