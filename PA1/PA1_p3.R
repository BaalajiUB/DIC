#https://www.rdocumentation.org/packages/twitteR/versions/1.1.9
library(twitteR)
setwd("~/git_workspace/DIC/PA1")
API_key <- "qbbabJxE8MlLrmtsNFmb8HZuI"
API_secret_key <- "LNrxtOmM9IjepyNvJgRLQDbTb2umK2zEcz1DyoyFBLzCF9Yev0"
Access_token <- "1038144228596039680-55CYAYC8HDAeyNInyx3W8sTMwcF8SY"
Access_token_secret <- "KJADO9al58jGnRmb6ktPUGDk8BchnWPXqMGG6pDmW9b0e"
setup_twitter_oauth(API_key, API_secret_key, Access_token, Access_token_secret)
tweet_data <- searchTwitter('flu', n=2000)
tweet_df <- twListToDF(tweet_data)
write.csv(tweet_df, file = "flu.csv")
dim(tweet_df)
