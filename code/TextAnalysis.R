library(tidyverse)
library(tidytext)
library(stringr)
library(stringi)
library(ggplot2)
library(reshape2)
library(DataCombine)
library(purrr)
library(wordcloud)
library(emojifont)

# read data 
msi  <- read.csv("MadeinSI.csv")
msi2 <- read.csv("msitweets2.csv")
msi  <- msi[-1]
msi2 <- msi2[-1]
msi  <- bind_rows(msi, msi2)
rm(msi2)

# add a custom stop word to list (Staten, Island)
custom_stop_words <- bind_rows(data_frame(word = c("Staten", "Island", "#madeinstatenisland", "staten", "island", "mtvs", "mtv"), 
                                          lexicon = c("custom")), 
                               stop_words)



                                    ##### Word Counts #####

# remove stop words, ampersands this and that 
remove_reg <- "&amp;|&lt;|&gt;" #from tidytext textbook 
rm_emojis <-  '[\U{1F300}-\U{1F6FF}]' #from https://stackoverflow.com/questions/43359066/how-can-i-match-emoji-with-an-r-regex

msi_words <- msi %>% 
  mutate(text = str_remove_all(text, "http.*")) %>% #remove links
  mutate(text = str_remove_all(text, "https.*")) %>% #remove links
  mutate(text = str_remove_all(text, remove_reg)) %>% #ampersands 
  mutate(text = str_remove_all(text, "@\\w+")) %>% #remove @s 
  mutate(text = str_remove_all(text, '[\U{1F300}-\U{1F6FF}]')) %>% #removes "most" emojis 
  unnest_tokens(word, text, token = "tweets") %>% #unest tokens 
  filter(!word %in% custom_stop_words$word,
         !word %in% str_remove_all(custom_stop_words$word, "'"), #remove stop words 
         str_detect(word, "[a-z]")) %>%
  select(id, word) %>%
  mutate(word = str_remove(word, "^#")) %>% #remove from words list those tweets that start with hastags
  filter(!grepl("staten", word, ignore.case = T)) #filter our tricking staten island words 

# top 50 words 

msi_words %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  slice(1:50) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  guides(fill=FALSE)

                                  ##### Most Mentioned Cast Member #####
cast <- msi %>%
  mutate(kayla = ifelse(grepl("kayla", text, ignore.case = T),1,0)) %>%
  mutate(paulie = ifelse(grepl("paul", text, ignore.case = T),1,0)) %>%
  mutate(cp = ifelse(grepl("cp|christian", text, ignore.case = T),1,0)) %>% #add nickname
  mutate(karina = ifelse(grepl("karina|kiki", text,ignore.case = T),1,0)) %>% # add nickname 
  mutate(dennie = ifelse(grepl("dennie", text,ignore.case = T),1,0)) %>% 
  select(kayla,paulie,cp,karina,dennie) %>%
  colSums() %>%
  data.frame()

colnames(cast)[1] <- "count"
cast$member <- rownames(cast)

ggplot(aes(x = member, y = count, fill = member), data = cast) +
  geom_bar(stat = 'identity') 

rm(cast)

                                    ##### Sentiment Analysis #####

# do tweets have more positive or negative words 
msi_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  ggplot(aes(x=sentiment, y = n)) +
  geom_bar(stat="identity", color="orange", fill="white") +
  ggtitle("Sentiment of Tweets")

Negative_Words_perTweet <- msi_words %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(id) %>%
  summarise(SumNegative = sum(sentiment == "negative"), SumPositive = sum(sentiment == "positive")) %>%
  count(SumNegative) 
 
Positive_Words_perTweet <- msi_words %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(id) %>%
  summarise(SumNegative = sum(sentiment == "negative"), SumPositive = sum(sentiment == "positive")) %>%
  count(SumPositive)  

# more tweets overly positive or overly negative or tie 
 msi_words %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(id) %>%
  summarise(SumNegative = sum(sentiment == "negative"), SumPositive = sum(sentiment == "positive")) %>%
  mutate(OverallSentiment = case_when(
    SumNegative > SumPositive ~ "More Negative",
    SumPositive > SumNegative ~ "More Positive",
    SumPositive == SumNegative ~ "Tie"
  )) %>% 
  count(OverallSentiment) %>%
  ggplot(aes(x=OverallSentiment, y= n)) +
    geom_bar(stat = "identity", color = "orange", fill = "white") +
    ggtitle("Tweet Categorization") +
    labs( y = "number of tweets", x = "sentiment") 

# compare negative and positive words 
msi_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


# word cloud 

msi_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("indianred3","lightsteelblue3"),
                   max.words = 100, random.order = F, title.size=2.5)




                                    ##### Examination of Hastags #####
msi$text <- as.character(msi$text)
hashtags <- str_extract_all(msi$text, "#\\S+")
hashtags <- data.frame(hashtags = unlist(hashtags)) 

hashtags <- hashtags %>%
  mutate(hashtags = str_remove_all(hashtags, '[\U{1F300}-\U{1F6FF}]')) %>% #removes "most" emojis 
  mutate(hashtags =  str_remove_all(hashtags,"[^#][[:punct:]]")) %>% #remove punctuation but # 
  mutate(hashtags = tolower(hashtags)) %>%
  count(hashtags) %>%
  arrange(desc(n))

rm(hashtags, Negative_Words_perTweet, Positive_Words_perTweet, TweetCategorization)

                                  ##### Emoji Analysis #####

emoji_find <- str_extract_all(string = msi$text, pattern = '[^\\w\\s[[:punct:]]]')
names(emoji_find) <- 1:nrow(msi)
emoji_find <- map_df(emoji_find, ~as.data.frame(.x), .id="id")

#emoji_find <- lapply(emoji_find, function (x) paste(x, collapse = ""))
#emoji_find <- data.frame(non_words = unlist(emoji_find))

unicode_1 <-  c("\\u2640", "\\u2642")
unicode_2 <-  c("\\U0001f3fb", "\\U0001f3fc", "\\U0001f3fd","\\U0001f3fe", "\\U0001f3ff")
unicode_3 <-  c("\\U0001f1ee", "\\U0001f1f9")
unicode_4 <- c("\\U0001f926", "\\U0001f937")

emoji_find <- emoji_find %>%
  mutate(escape = stringi::stri_escape_unicode(.x)) %>%
  mutate(special_character = ifelse(escape %in% unicode_1 | escape %in% unicode_2,1,0)) %>%
  mutate(italian_flag = ifelse(escape %in% unicode_3,1,0)) %>%
  group_by(id) %>%
  mutate(combine = ifelse(special_character==1 & lag(special_character) ==1, 
                          paste(dplyr::lag(escape,2),dplyr::lag(escape,1),"\\u200d",escape, sep = ""), "")) %>%
  mutate(combine = ifelse(special_character==1 & lag(special_character)==0 & lead(special_character, default = 0)==0,
                          paste(dplyr::lag(escape,1),escape, sep = ""), combine)) %>%
  mutate(combine = ifelse(special_character==1 & lag(special_character)==0& 
                            lag(escape,1) %in% unicode_4 & 
                            lead(special_character, default = 0)==0,paste(dplyr::lag(escape,1),
                            "\\u200d",escape, sep = ""), combine)) %>%
  mutate(combine = ifelse(italian_flag==1 & lag(italian_flag)==1,
                          paste(dplyr::lag(escape,1),escape, sep = ""), combine))
  

combined_emojis <- filter(emoji_find, combine != "") %>% select(id,combine) %>% 
                          mutate(combine = ifelse(stri_detect_fixed(combine, "2640")|
                                                  stri_detect_fixed(combine, "2642"), 
                                                  paste(combine,"\\ufe0f", sep = ""),combine))
names(combined_emojis)[2] <- "utf-8"

nc_emojis <- filter(emoji_find, combine == "") %>% select(id, escape) 
names(nc_emojis)[2] <- "utf-8"

emoji_FINAL <- bind_rows(nc_emojis, combined_emojis)

rm(combined_emojis, nc_emojis)

##credit goes to ista: 
## https://stackoverflow.com/questions/47675990/reading-in-unicode-emoji-correctly-into-r ## 

## read emoji info and get rid of documentation lines

readLines("https://unicode.org/Public/emoji/12.0/emoji-test.txt",# replaced with latest version 
          encoding="UTF-8") %>%
  stri_subset_regex(pattern = "^[^#]") %>%
  stri_subset_regex(pattern = ".+") -> emoji

emoji <- stringi::stri_escape_unicode(emoji)

## get the emoji characters and clean them up
emoji %>%
  stri_extract_all_regex(pattern = "#\\s+\\S+") %>%
  stri_replace_all_fixed(pattern = c("*", "#"),
                         replacement = "",
                         vectorize_all=FALSE) %>%
  stri_trim_both() -> emoji.chars

## get the emoji character descriptions
emoji %>%
  stri_extract_all_regex(pattern = "#.*$") %>%
  stri_replace_all_regex(pattern = "#\\s+\\S+",
                         replacement = "") %>%
  stri_trim_both() -> emoji.descriptions

#create dataframe
emoji_ref <-data.frame(emoji.chars, emoji.descriptions, stringsAsFactors = F)
emoji_ref$emojis <- stringi::stri_unescape_unicode(emoji_ref$emoji.chars)

rm(emoji.chars, emoji.descriptions)

#match emojis from tweets with emoji descriptions 

emoji_FINAL$Desc <- emoji_ref$emoji.descriptions[match(emoji_FINAL$`utf-8`,emoji_ref$emoji.chars)]
emoji_FINAL <- filter(emoji_FINAL, !is.na(Desc))  # only drop 14 

top_8<-emoji_FINAL %>%
  ungroup() %>%
  mutate(unescaped = stringi::stri_unescape_unicode(`utf-8`)) %>%
  count(Desc,unescaped) %>%
  arrange(desc(n)) %>%
  top_n(8)

ggplot(aes(x=Desc, y = n), top_8) +
  geom_bar(stat = 'identity') + 
  scale_x_discrete(breaks = Desc, labels = unescaped) +
  coord_flip()






  
  





