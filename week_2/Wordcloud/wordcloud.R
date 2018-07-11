source('pttTestFunction.R')
id = c(4032:4034)
URL = paste0("https://www.ptt.cc/bbs/Boy-Girl/index", id, ".html")
filename = paste0(id, ".txt")
pttTestFunction(URL[1], filename[1])
mapply(pttTestFunction, 
       URL = URL, filename = filename)