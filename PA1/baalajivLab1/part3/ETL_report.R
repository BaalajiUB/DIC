#https://www.rdocumentation.org/packages/twitteR/versions/1.1.9
# library(twitteR)
# setup_twitter_oauth(API_key, API_secret_key, Access_token, Access_token_secret)
# tweet_data <- searchTwitter('flu', n=2000)
# tweet_df <- twListToDF(tweet_data)
# write.csv(tweet_df, file = "flu.csv")
# dim(tweet_df)
rm(list = ls())
#setwd("~/git_workspace/DIC/PA1")


collect_tweet <- function(){
API_key <- "qbbabJxE8MlLrmtsNFmb8HZuI"
API_secret_key <- "LNrxtOmM9IjepyNvJgRLQDbTb2umK2zEcz1DyoyFBLzCF9Yev0"
Access_token <- "1038144228596039680-55CYAYC8HDAeyNInyx3W8sTMwcF8SY"
Access_token_secret <- "KJADO9al58jGnRmb6ktPUGDk8BchnWPXqMGG6pDmW9b0e"

#install.packages('rtweet')
library(rtweet)

create_token("IR Project 1 : Indexing",API_key,API_secret_key,Access_token,Access_token_secret)
usa <- lookup_coords("usa", "AIzaSyAZaXDX8K_WBKBRi8G0Dkw-bK2RWV_bSeI")

count = 0
m = NULL

print(count)  
rt <- search_tweets("#Flu OR #flushot OR #fluvirus OR #influenza OR epidemic", n = 10000, include_rts = FALSE, geocode = usa, retryonratelimit = T, max_id = m)
print(dim(rt))
count = count + dim(rt)[1]
m <- min(as.numeric(rt$status_id))
#print(m)

while (count<10000){
print(count)  
#print(dim(rt))
t <- search_tweets("#Flu OR #flushot OR #fluvirus OR #influenza", n = 10000, include_rts = FALSE, geocode = usa, retryonratelimit = T, max_id = m)
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

save_as_csv(rt, file_name = "flu_18k_1.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
#View(unique(rt$location))
}

##ETL
flu1 <- read.csv('flu_18k_1.csv', header = T, stringsAsFactors = F)
dim(flu1)
#flu1 <- rt
  
locations <- flu1$location
#View(table(locations))
#View(unique(locations))

temp1 <- as.vector(locations)
#https://stat545.com/block022_regular-expression.html
temp1 <- temp1[grepl(', [A-Z]{2}$',temp1)]
#View(temp1)

temp2 <- c() #appending to a list
for (i in temp1){
temp2<-c(temp2,strsplit(i, ',')[[1]][2])
}

temp2
length(temp2)

tbl <- table(temp2)
population <- as.data.frame(tbl, stringsAsFactors = F)
population$temp2 <- trimws(population$temp2) #trimws works on entire column

View(population)

tmp <- as.vector(locations)

temp3 <- c() #appending to a list
for (i in tmp){
  if (length(strsplit(i, ',')[[1]]) == 2){
    x <- strsplit(i, ',')[[1]][2]
    if ("USA" == trimws(x)){
      temp3<-c(temp3,strsplit(i, ',')[[1]][1])}
  }
}

tbl1 <- table(temp3)
population1 <- as.data.frame(tbl1, stringsAsFactors = F)
population1$temp3 <- trimws(population1$temp3) #trimws works on entire column


library(tidyverse)
library(usmap)

req_data <- merge(x = population, y = statepop, x.by = temp2, y.by = abbr, x.all = TRUE)
req_data <- filter(req_data, temp2 == abbr)
#View(req_data)
req_data <- req_data[, c(2,3,4,5)]
names(req_data)

req_data1 <- merge(x = population1, y = statepop, x.by = temp3, y.by = full, x.all = TRUE)
req_data1 <- filter(req_data1, temp3 == full)
#req_data <- req_data[, c(2,3,4,5)]
req_data1 <- req_data1[, c(2,3,4,5)]
names(req_data1) <- c('Freq_1', 'fips', 'abbr', 'full')

res <- merge(x = req_data,y = req_data1, all = T)
res$Freq[is.na(res$Freq)] <- 0
res$Freq_1[is.na(res$Freq_1)] <- 0
res$frequency <- res$Freq + res$Freq_1
names(res)
res <- res[, c(1,2,3,6)]
#nchar(req_data$temp2)
#nchar(trimws(req_data$temp2))
#View(res)
# heatmap <- plot_usmap(data = res, values = "frequency", lines = "black") + 
#   #scale_fill_manual(values = c( "#CC0000", "#D73800", "#E27100", "#EEAA00", "#F9E200", "#E2F800", "#AAEA00", "#71DD00", "#38CF00", "#00C200")) +
#   theme(legend.position = "right") +
#   labs(fill = "Population")
# 
# heatmap
##############################################################################################################################
#library(tidyverse)
df <- read.csv("2011_us_ag_exports.csv", header = T)
head(df)
#write.csv(df, '2011_us_ag_exports.csv')
#head(df)
df <- df[, c(2,3,4,5)]
names(df)
#chart5_data <- read.csv(file = 'StateDataforMap_2018-19week8.csv', header = T, stringsAsFactors = F)
#head(chart5_data)
chart5_data <- res
#names(res)
#chart5_data <- chart5_data[,-c(2,3)]

#https://stackoverflow.com/questions/42286072/extracting-numeric-values-from-mixed-variable-column-in-r
#chart5_data$ACTIVITY.LEVEL <- sapply(strsplit(chart5_data$ACTIVITY.LEVEL, ' '), function(x) as.numeric(x[[2]]))

req_data <- left_join(chart5_data, df, by = c("abbr"="code"))

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)

# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
names(req_data)
req_data$hover <- with(req_data, frequency)

chart5 <- plot_geo(req_data, locationmode = 'USA-states') %>%
  add_trace(
    z = ~frequency,text = ~hover,  locations = ~abbr,
    color = ~frequency, colors = 'Reds'
  ) %>%
  colorbar(title = "ACTIVITY LEVEL") %>%
  layout(
    title = 'heatmap',
    geo = g
  )

chart5
rm( req_data, chart5_data, df, l, g)
