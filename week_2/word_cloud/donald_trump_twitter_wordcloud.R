library(twitteR)
library(ROAuth)
library(magrittr)
library(wordcloud2)

tweets <- searchTwitter(searchString = "realDonaldTrump", n = 100)
tweets.df <- twListToDF(tweets)
tweets.char <- tweets.df[,"text"] %>%
            list(.) %>%
            paste(., sep = "") %>%
            gsub('[[:punct:]]+',' ',.) %>%
            strsplit(., " ")

djt.df <- as.data.frame(table(tweets.char))
djt.dude <- djt.df[order(djt.df$Freq, decreasing = TRUE),]

trashwords <- c('https', 'RT', 't', 'and' , 'co', 'is','what', 'on',
                'you', 'not', 'has', 'Dear', 'do', 'It', 'about', 'The',
                'have', 'an', 'U', 'to', 'the', ' of', 'n', 's', 'I', 
                'our', 'for', 'are', 'He', 'he', 'of', 'his', 'a', 'So',
                "It's", 'theres', 'as', 'this', 'in', 'one','all','one',
                'been', 'part', 'When','may', 'that', 'or', 'me','K', "by",
                'now', 'You', 'off', 'A', 'so', 'was','ha', 'know', 'Because',
                'would', 'just', 'him', 'On', 'will', 'your', 'no', 'out',)

trashwords.pattern <- apply(djt.dude, 1, function(x) !any(x %in% trashwords))
djt.final <- subset(djt.dude, trashwords.pattern)
wordcloud2(djt.final, size = 20, minSize = 2, color = "random-light", backgroundColor = "grey")

