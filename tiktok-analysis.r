library(readr)
library(dplyr)

tiktok_posts <- read_csv("INSERT_FILE_NAME_HERE") # Namen der entsprechenden Datei (inkl. Dateiendung) einfügen

names(tiktok_posts)

glimpse(tiktok_posts)

tiktok_posts %>% 
  count(author_full) %>% 
  arrange(desc(n)) %>% 
  head(10)

tiktok_comments <- read_csv("INSERT_FILE_NAME_HERE") # Namen der entsprechenden Datei (inkl. Dateiendung) einfügen

names(tiktok_comments)

glimpse(tiktok_comments)

library(quanteda)

tiktok_videos_corpus <- tiktok_posts %>% 
  select(id, author, author_full,
         timestamp, unix_timestamp,
         likes, comments, shares, plays,
         tiktok_url,
         body) %>% 
  corpus(docid_field = "id",
         text_field = "body")

tokens_tiktok_videos <- tokens(tiktok_videos_corpus,
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_numbers = TRUE,
                         remove_url = TRUE)

tokens_tiktok_videos <- tokens_remove(tokens_tiktok_videos,
                                stopwords("de"))

tokens_tiktok_videos

dfm_tiktok_videos <- dfm(tokens_tiktok_videos)

dfm_tiktok_videos

library(quanteda.textstats)

dfm_tiktok_videos %>%
  dfm_remove(pattern = c("dass",
                         "@*", "#*")) %>% # ohne User Tags und Hashtags
  textstat_frequency(n = 20)

dfm_tag <- dfm_select(dfm_tiktok_videos, pattern = "#*")
toptag <- names(topfeatures(dfm_tag, 50))
head(toptag, 10)

dfm_users <- dfm_select(dfm_tiktok_videos, pattern = "@*")
topuser <- names(topfeatures(dfm_users, 50))
head(topuser, 10)

library(quanteda.textplots)

dfm_tiktok_videos %>% 
  dfm_remove(pattern = c("dass",
                         "@*", "#*")) %>% 
  dfm_trim(min_termfreq = 50) %>%
  textplot_wordcloud()

library(ggplot2)

tstat_freq <- dfm_tiktok_videos %>% 
  dfm_remove(pattern = c("dass",
                         "@*", "#*")) %>%  
  textstat_frequency(n = 20)

ggplot(tstat_freq, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_col() + 
  labs(x = "Frequency", y = "Feature") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))

tstat_key <- dfm_tiktok_videos %>% 
  dfm_remove(pattern = c("dass",
                         "@*", "#*")) %>%  
  textstat_keyness(target = dfm_tiktok_videos$author_full == "INSERT_ACCOUNT_NAME") # hier Namen des Accounts einfügen, der mit dem Rest des Corpus verglichen werden soll

textplot_keyness(tstat_key)

tiktok_comments_corpus <- tiktok_comments %>% 
  select(id, thread_id,
         author, author_full,
         timestamp, unix_timestamp,
         likes, replies,
         post_url,
         is_comment_on_comment,
         language_guess,
         post_body) %>% 
  corpus(docid_field = "id",
         text_field = "post_body")

tokens_tiktok_comments <- tokens(tiktok_comments_corpus,
                               remove_punct = TRUE,
                               remove_symbols = TRUE,
                               remove_numbers = TRUE,
                               remove_url = TRUE)

tokens_tiktok_comments <- tokens_remove(tokens_tiktok_comments,
                                      stopwords("de"))

dfm_tiktok_comments <- dfm(tokens_tiktok_comments)

dfm_tiktok_comments %>%
  dfm_remove(pattern = c("dass",
                         "@*", "#*")) %>% 
  textstat_frequency(n = 20)

dfm_users <- dfm_select(dfm_tiktok_comments, pattern = "@*")
topuser <- names(topfeatures(dfm_users, 50))
head(topuser, 10)

dfm_tiktok_comments %>% 
  dfm_remove(pattern = c("dass",
                         "@*", "#*")) %>% 
  dfm_trim(min_termfreq = 50) %>%
  textplot_wordcloud()

tstat_freq <- dfm_tiktok_comments %>% 
  dfm_remove(pattern = c("dass",
                         "@*", "#*")) %>%  
  textstat_frequency(n = 20)

ggplot(tstat_freq, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_col() + 
  labs(x = "Frequency", y = "Feature") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))
