
processFile = function(filepath) {
  con = file(filepath, "r")
  word = c()
  freq = c()
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    else{
        words = strsplit(line,"\t")
        word = c(word,words[[1]][1])
        freq = c(freq,words[[1]][2])
    }
    #print(line)
  }
  close(con)
  word_cloud <- data.frame(word,freq)
  return(word_cloud)
}

word_cloud <- processFile("part-00000")

#line = strsplit("abc\tdef","\t")
#line
#line[[1]][1]
#line[[1]][2]
names(word_cloud)
str(word_cloud)
word_cloud$word <- as.character(word_cloud$word)
word_cloud$freq <- as.numeric(word_cloud$freq)
str(word_cloud)

library("wordcloud")
library("RColorBrewer")

#word_cloud$freq
#min(word_cloud$freq)
#max(word_cloud$freq)

top_10 <- word_cloud[order(word_cloud$freq, decreasing = T),]
top_10 <- top_10[1:10,]
top_10
write.csv(top_10,"NYT_top.csv",row.names = F)

ordered <- as.vector(word_cloud$freq)
#class(ordered)
ordered <- unique(ordered)
ordered <- sort(ordered, decreasing = TRUE)
#ordered
max_freq <- ordered[1]
min_freq <- ordered[10]

#set.seed(1234)
wordcloud(words = word_cloud$word, freq = word_cloud$freq, min.freq = min_freq,
          max.words=10, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


