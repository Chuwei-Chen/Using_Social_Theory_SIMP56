#####################

#Assignment Code

#####################

library(dplyr)


library(rtweet)

my_token <- create_token(   #Creating token
  app = "Social Theory - Twitter",
  consumer_key = "rkgRiDDc8mQcZBmIl6PHVLVK5",
  consumer_secret = "0Os46wJbL0bxjhaRUljKR621ydmkF5fKWZZwWjM1A8FmPpp5lK",
  access_token = "1590752936-4euzrjwhoZ8nw7RsJQmksDdvQ6Nrqj8fHSwxrFz",
  access_secret = "qpGK7HxDutUBGDMq8cg4GuibeZ1iHQkSjOMz3LJmq8Ia4",
  set_renv = TRUE
)

tweetsCriticalRace2 <- search_tweets(     #Extracting the tweets
  q = "'critical race theory' OR crt OR #crt OR #criticalracetheory",   
  n = 18000,            
  type = "recent",
  include_rts = FALSE,  
  geocode = NULL,
  max_id = NULL,
  parse = TRUE,
  token = my_token,
  retryonratelimit = FALSE,
  verbose = TRUE,
  lang = "en")

#Clearing data

CRTtweets <- tweetsCriticalRace2 %>%     #Only selecting the columns we need. 
  select(user_id, status_id, screen_name, created_at, text, 
         favorite_count, retweet_count, quote_count, reply_count, followers_count)
  
  
#Filtering out duplicate tweets
 
  distinct_CRTtweets <- CRTtweets %>%
    distinct(text, .keep_all = T) 
  
#Double-checking which the duplicated tweets were
  
CRTtweets %>%
    group_by(text) %>%
  count() %>%
filter(n > 1)

#sample 200 tweets out of the whole dataset
  
set.seed(9)
sample_CRTtweet <- distinct_CRTtweets %>%
  sample_n(200)


#Hypothesis: Engagement Ratio

#Replace NAs with 0 for calculating the 

options(scipen = 2)

sample_CRTtweet2 <- sample_CRTtweet %>%
  replace(is.na(.), 0) %>%
  mutate(total_engagement = favorite_count + retweet_count + quote_count + reply_count)

CRTwithEngagement <- sample_CRTtweet2 %>% 
  mutate(total_engagement = favorite_count + retweet_count + quote_count + reply_count)

CRTwithEngagementR <- CRTwithEngagement %>% 
  mutate(engagement_ratio = total_engagement/followers_count) 

#Export the dataframe

library(writexl)

write_xlsx(CRTwithEngagementR, "CRTwithEngagementR2.xlsx")


#Important tolerance scores into R 

CRTfinal <- CRTwithEngagementR2

#Perspective API - toxicity

library(peRspective)

CRTfinal2 <- CRTfinal %>%
  select(status_id, `Combined Intolerance`, text)

CRTfinal2 <- prsp_stream(
  .data = CRTfinal2,
  text = text,
  text_id = status_id,
  languages = "en",
  score_sentences = F,
  score_model = "TOXICITY", 
  doNotStore = F,
  key = "AIzaSyDDJszpgjmjE1APaTyplG1QM6eEsAZt9AA")

CRTfinal2 <- CRTfinal2 %>%
  rename(status_id = text_id)

CRTfinal3 <- inner_join(CRTfinal, CRTfinal2, by = "status_id")

#Perspective API - identity_attack and insult

CRTidentity <- CRTfinal %>%
  select(status_id, `Combined Intolerance`, text)

CRTidentity2 <- prsp_stream(
  .data = CRTidentity,
  text = text,
  text_id = status_id,
  languages = "en",
  score_sentences = F,
  score_model = c("IDENTITY_ATTACK", "INSULT"),
  doNotStore = F,
  key = "AIzaSyDDJszpgjmjE1APaTyplG1QM6eEsAZt9AA")

CRTidentity2 <- CRTidentity2 %>%
  rename(status_id = text_id)

CRTfinal3 <- inner_join(CRTfinal3, CRTidentity2, by = "status_id")


#Data analysis part

CRTfinal3 %>% 
  group_by(`Combined Intolerance`) %>%   #How many Intolerant vs tolerant
  count()


CRTfinal3 %>%
  group_by(`Combined Intolerance`) %>%     #Mean engagement ratio per each group
  summarize(Mean = mean(engagement_ratio))


CRTfinal3 %>%
  group_by(`Combined Intolerance`) %>%      #Mean toxicity per each group
  summarize(Mean = mean(TOXICITY))

CRTfinal3 %>%
  group_by(`Combined Intolerance`) %>%      #Mean identity attack per each group
  summarize(Mean = mean(IDENTITY_ATTACK))


CRTfinal3 %>%     
  group_by(`Combined Intolerance`) %>%      #Mean insult per group
  summarize(Mean = mean(INSULT))
