library(plyr)
library(tidyverse)
library(twitteR)
library(streamR)
library(ROAuth)

#setup 
api_key <- "Tb8Vmv5dcBD8A2Sh0aTJZUbMw"
api_secret <- "fSu5lAs2u6WP5DZK1uSfhTp2FCzm6w7TTfQfAUquUrNSHMhKlM"
token <- "379528859-oFZhHNJP90w65THxI9RtbgKpDAijgOdqXNEgQ0ul"
token_secret <- "gk3bxSPmUUyQGueMKUYnitFtX2It0v2a8wbwhpL9nF5SA"

setup_twitter_oauth(api_key, api_secret, token, token_secret)

# get data
msi1 <- searchTwitter("#MadeinStatenIsland", n = 10000, lang = "en", since = "2019-01-20", until = "2019-01-22")
msi2 <- searchTwitter("made+ in+ Staten + Island", n = 10000, lang = "en", since = "2019-01-20", until = "2019-01-22")

# list to dataframe 
msi1 <- lapply(msi1, as.data.frame)
msi1 <- bind_rows(msi1)
msi2 <- lapply(msi2, as.data.frame)
msi2 <- bind_rows(msi2)
msi_final <- rbind(msi1,msi2)

# remove retweets
nrow(msi_final[msi_final$isRetweet==TRUE,]) #3518 are RTs 
msi_final <- filter(msi_final, isRetweet != T)

# check for duplicates
sum(duplicated(msi_final)) #411 are duplicated...
msi_final <- msi_final[!duplicated(msi_final),]
write.csv(msi_final, "msitweets2.csv")
