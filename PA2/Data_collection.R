rm(list = ls())
setwd("~/git_workspace/DIC/PA2")


collect_tweet <- function(){
  API_key <- "qbbabJxE8MlLrmtsNFmb8HZuI"
  API_secret_key <- "LNrxtOmM9IjepyNvJgRLQDbTb2umK2zEcz1DyoyFBLzCF9Yev0"
  Access_token <- "1038144228596039680-55CYAYC8HDAeyNInyx3W8sTMwcF8SY"
  Access_token_secret <- "KJADO9al58jGnRmb6ktPUGDk8BchnWPXqMGG6pDmW9b0e"
  
  #install.packages('rtweet')
  library(rtweet)
  
  create_token("IR Project 1 : Indexing",API_key,API_secret_key,Access_token,Access_token_secret)
  #usa <- lookup_coords("usa", "AIzaSyAZaXDX8K_WBKBRi8G0Dkw-bK2RWV_bSeI")
  
  count = 0
  m = NULL
  
  print(count)  
#  rt <- search_tweets("#GOT ", n = 20000, include_rts = FALSE, geocode = usa, retryonratelimit = T, max_id = m)
  rt <- search_tweets("#GOT #gameofthrones", n = 20000, include_rts = FALSE, retryonratelimit = T, max_id = m)
  print(dim(rt))
  count = count + dim(rt)[1]
  m <- min(as.numeric(rt$status_id))
  #print(m)
  
  while (count<10000){
    print(count)  
    #print(dim(rt))
    #t <- search_tweets("#Flu OR #flushot OR #fluvirus OR #influenza", n = 10000, include_rts = FALSE, geocode = usa, retryonratelimit = T, max_id = m)
    t <- search_tweets("#GOT #gameofthrones", n = 20000, include_rts = FALSE, retryonratelimit = T, max_id = m)
    print(dim(t))
    rt <- rbind(rt,t)
    count = count + dim(t)[1]
    m <- min(as.numeric(rt$status_id))
    #print(m)
  }
  
  count
  dim(rt)
  
  # t <- data.frame(c(1,2), c(3,4))
  # names(t) <- c('a','b')
  # t
  # t1 <- data.frame(c(5,6),c(7,8))
  # names(t1) <- c('a','b')
  # t1
  # 
  # t <- rbind(t,t1)
  # t
  # dim(t)
  
  save_as_csv(rt, file_name = "GOT.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
  #View(unique(rt$location))
}

collect_tweet()

GOT <- read.csv("GOT.csv",header = TRUE)
class(GOT)

