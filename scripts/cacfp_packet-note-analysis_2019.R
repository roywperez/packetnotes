library(tidyverse)
library(tidytext)
library(ggplot2)


packetnotes <- read.csv("H:/2016 CACFP Packet Note Analysis/packetnotets_csv.csv", stringsAsFactors = FALSE, colClasses = c(NA,"NULL"))

data("stop_words")


packetnotes_bigrams <- packetnotes %>% 
                       unnest_tokens(bigram, COMMENT, token = "ngrams", n = 2)


bigrams_separated <- packetnotes_bigrams %>% 
                    separate(bigram, c("word1", "word2"), sep = " ")

stop_words2 <- c("tda","isd","i'm",
                 "texasagriculture.gov", "www.squaremeals.org", "www.squaremeals.gov", "www.texasagriculture.gov",
                 "ce","academy",
                 "cacfp",
                 "py",
                 "september",
                 "email",
                 "ces","day","care", "due", "dates", "quot",
                 "txunps",
                 "date",
                 "unps","conducted",
                 "site",
                 "tx", "houston",
                 "corey","fatima", "burnias","cuevas"
                )

bigrams_filtered <- bigrams_separated %>% 
                    filter(!word1 %in% stop_words$word,
                           !word1 %in% stop_words2,
                           !str_detect(word1, "\\_"),
                           !str_detect(word1, "[:digit:]")) %>% 
                    filter(!word2 %in% stop_words$word,
                           !word2 %in% stop_words2,
                           !str_detect(word2, "\\_"),
                           !str_detect(word2, "[:digit:]"))   

bigram_counts <- bigrams_filtered %>% 
                count(word1,word2, sort = TRUE)

bigram_counts


cleanedpacket <- packetnotes %>% 
                filter(!str_detect(word, "[:digit:]"),
                       !str_detect(word, "mailto*?"),
                       !str_detect(word, "\\_"),
                       !word %in% c("tda", "texasagriculture.gov", "ce", "cacfp", "py", "september", "email", "ces", "txunps", "date", "unps", "site", "tx"))

