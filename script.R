
# load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(wesanderson)
library(ggsignif)
library(ggpubr)
library(labelled)
library(patchwork)
library(boot)
library(statebins)
library(dtw)
library(rstatix)

# read in data ------------------------------------------------------------

df <- read_csv('data/unprocessed/df.csv', col_types = cols(  X1 = col_double(),
                                            user_id = col_character(),
                                            tweet_id = col_character(),
                                            sentiment = col_character(),
                                            created_at = col_character(),
                                            user_location = col_character(),
                                            coordinates = col_character(),
                                            place = col_character(),
                                            race = col_character(),
                                            age = col_double(),
                                            gender = col_character())) %>% 
  mutate(
    dttm = strptime(created_at, format = '%a %b %d %H:%M:%S +%OS %Y'),
    date = date(dttm),
    time = format(dttm, format = '%H:%M:%S')
  ) %>% 
  arrange(dttm) %>% 
  drop_na(race) %>% 
  # drop duplicates
  distinct(tweet_id, .keep_all = TRUE) %>% 
  mutate(sentiment_code = ifelse(sentiment == 'negative', 0, 
                                 ifelse(sentiment == 'neutral', 1, 2)))

# tweets with age gender available
df_sd <- df %>% 
  filter(!is.na(age))

#users with multiple race
df_sd %>% 
  group_by(user_id) %>% 
  summarise(n_race = n_distinct(race),
            races=paste(race, collapse='-')) %>% 
  filter(n_race > 1)
#replace the race of multiple race with the most prevalent
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
df_sd <- df_sd %>% 
  group_by(user_id) %>% 
  mutate(race = Mode(race))

#users with multiple gender
df_sd %>% 
  group_by(user_id) %>% 
  summarise(n_gender = n_distinct(gender),
            genders=paste(gender, collapse='-')) %>% 
  filter(n_gender > 1)

df_sd <- df_sd %>% 
  group_by(user_id) %>% 
  mutate(gender = Mode(gender))

# write out final data
write_csv(df_sd, 'data/processed/df_sd.csv')

daily_count <- df %>% 
  group_by(date) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

write_csv(daily_count, 'data/processed/daily_count.csv')

# visualization -----------------------------------------------------------
# daily counts + weekly pattern

p7 <- df %>% 
  # daily count
  group_by(date, sentiment) %>% 
  summarise(count = n()) %>% 
  mutate(sum = sum(count)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_area(aes(x = date,
                y = count,
                color = sentiment,
                fill = sentiment),
            alpha = 0.2,
            position = position_dodge(0.8)) +
  scale_color_manual(limits = c('negative', 'positive', 'neutral'),
                     values = wes_palette(n=3, 
                                          name="Darjeeling1")) +
  scale_fill_manual(limits = c('negative', 'positive', 'neutral'),
                    values = wes_palette(n=3, 
                                         name="Darjeeling1")) +
  labs(
    y = 'tweet count',
    color = '',
    fill = ''
  ) +
  ggtitle('Daily counts') +
  theme_minimal() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        plot.title = element_text(margin = margin(b = -20))) +
  scale_y_continuous(labels = scales::comma) 

p8 <- df %>% 
  # daily count
  group_by(date, sentiment) %>% 
  summarise(count = n()) %>% 
  mutate(sum = sum(count)) %>% 
  ungroup() %>% 
  # get weekly date
  mutate(week = as.Date(cut(date, breaks = 'week'))) %>% 
  group_by(week, sentiment) %>% 
  mutate(weekly_count = sum(count),
         weekly_avg = weekly_count / 7) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(x = week, 
                y = weekly_count,
                color = sentiment)) +
  scale_color_manual(limits = c('negative', 'positive', 'neutral'),
                     values = wes_palette(n=3, 
                                          name="Darjeeling1")) +
  scale_fill_manual(limits = c('negative', 'positive', 'neutral'),
                    values = wes_palette(n=3, 
                                         name="Darjeeling1")) +
  scale_y_continuous(breaks = c(0, 60000, 120000),
                     labels = scales::comma) +
  labs(
    y = 'tweet count'
  ) +
  theme_minimal() +
  ggtitle('Weely pattern') +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = 20),
        plot.title = element_text(margin = margin(t = 10, b = -20)))

p8/p7 +
  plot_layout(heights = unit(c(4,6), units = "cm"))

ggsave('figures/sentiments_over_time.png',
       dpi = 300,
       height = 8,
       width= 16,
       units = 'in')

# Week day patterns -------------------------------------------------------

df %>% 
  mutate(weekday = weekdays(as.Date(date)),
         weekday = factor(weekday, 
                          levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))) %>% 
  group_by(weekday, sentiment) %>% 
  summarise(weekday_count = n()) %>% 
  ggplot +
  geom_bar(aes(x = weekday, 
               y = weekday_count, 
               fill = sentiment),
           stat = 'identity',
           position = 'dodge') +
  scale_color_manual(limits = c('negative', 'positive', 'neutral'),
                     values = wes_palette(n=3, 
                                          name="Darjeeling1")) +
  scale_fill_manual(limits = c('negative', 'positive', 'neutral'),
                    values = wes_palette(n=3, 
                                         name="Darjeeling1")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    y = 'tweet count'
  )+
  theme_minimal()

ggsave('figures/weekday_patterns.png',
       dpi = 300,
       height = 8,
       width= 16,
       units = 'in')

# number of weekdays positive more than negative

n_pos_more_week <- df %>% 
  # on a weekday
  mutate(weekday = weekdays(as.Date(date)),
         weekday = factor(weekday, 
                          levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')),
         if_weekend = ifelse((weekday == 'Saturday'| weekday == 'Sunday'), 1, 0),
         week = floor_date(as.Date(date), unit="week", week_start = 1),
         n_weeks = n_distinct(week)) %>% 
  
  # daily count
  group_by(week, sentiment, weekday, n_weeks) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = sentiment, values_from = count) %>% 
  mutate(pos_more = ifelse(positive > negative, 1, 0)) %>% 
  group_by(weekday, n_weeks) %>% 
  summarise(n_pos_more = sum(pos_more)) 

write_csv(n_pos_more_week,"data/processed/n_pos_more_week.csv")

# sentient change only positive and negative ------------------------------

sentiment_chg_pn <- df %>% 
  filter(sentiment_code != 1) %>% 
  group_by(user_id) %>% 
  arrange(dttm) %>%
  mutate(chg = sentiment_code - lag(sentiment_code)) %>% 
  filter(!is.na(chg) & chg!=0) %>% 
  arrange(user_id, dttm)

sentiment_chg_daily_pn <- sentiment_chg_pn %>% 
  group_by(date) %>% 
  summarise(pos = sum(chg > 0),
            neg = sum(chg < 0)) %>% 
  mutate(neg = -neg)

# function for showing absolute value and common style together
abs_comma <- function (x, ...) {
  format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

ggplot(sentiment_chg_daily_pn, aes(x = date)) + 
  geom_area(aes(y = pos),
            color = 'darkgreen',
            fill = 'darkgreen',
            alpha = 0.2) +
  geom_area(aes(y = neg),
            color = 'red',
            fill = 'red',
            alpha = 0.2) +
  labs(
    y = "Count"
  ) +
  scale_y_continuous(labels=abs_comma) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 20))

ggsave("figures/sentiment_chg_daily_pn.png",
       dpi = 300,
       height = 4,
       width= 16,
       units = 'in')

write_csv(sentiment_chg_daily_pn, "data/processed/sentiment_chg_daily_pn.csv")



# proportion - race -------------------------------------------------------
stat_test <- df_sd %>% 
  group_by(race, sentiment) %>% 
  summarise(count = n()) %>% 
  group_by(race) %>% 
  mutate(all = sum(count),
         count_ = all - count,
         prop = count / all) %>% 
  group_by(race)

prop_test_rslt <- pairwise_prop_test(
  x = as.matrix(stat_test[,c('count', 'count_')]),
  conf.level = 0.05,
  p.adjust.method = "fdr") %>% 
  mutate(diff = abs(as.numeric(group1) - as.numeric(group2))) %>% 
  filter((diff == 3) | (diff == 6) | (diff == 9))

df_sd %>% 
  group_by(race, sentiment) %>%
  summarise(count = n()) %>%
  group_by(race) %>%
  mutate(all = sum(count),
         percent = count/all) %>%
  ggplot(aes(x = race, y = percent,
             color = sentiment, fill = sentiment)) +
  geom_bar(stat = 'identity',
           position = 'dodge',
           alpha = 0.6) +
  geom_signif(
    # inherit.aes = FALSE,
    data = data.frame(sentiment = c("negative","negative","negative","negative","negative","negative",
                                    "neutral","neutral","neutral","neutral","neutral","neutral",
                                    "positive", "positive","positive","positive","positive","positive"),
                      start = c("Asian", "Black", "Hispanic", "Asian", "Black", "Asian",
                                "Asian", "Black", "Hispanic", "Asian", "Black", "Asian",
                                "Asian", "Black", "Hispanic", "Asian", "Black", "Asian"),
                      end = c("Black", "Hispanic", "White", "Hispanic", "White", "White",
                              "Black", "Hispanic", "White", "Hispanic", "White", "White",
                              "Black", "Hispanic", "White", "Hispanic", "White", "White"),
                      y = c(0.4, 0.4, 0.4, 0.5, 0.6, 0.7,
                            0.7, 0.7, 0.7, 0.8, 0.9, 1,
                            0.4, 0.4, 0.4, 0.5, 0.6, 0.7),
                      label = prop_test_rslt$p.adj.signif[c(1,5,12,4,11,10,
                                                            2,7,15,6,14,13,
                                                            3,9,18,8,17,16)]),
    aes(xmin = start,
        xmax = end,
        annotations = label, 
        y_position = y),
    manual = TRUE
  )+
  
  facet_wrap(~sentiment, ncol = 3) +
  scale_color_manual(limits = c('negative', 'positive', 'neutral'),
                     values = wes_palette(n=3, 
                                          name="Darjeeling1")) +
  scale_fill_manual(limits = c('negative', 'positive', 'neutral'),
                    values = wes_palette(n=3, 
                                         name="Darjeeling1"))+
  theme_minimal() +
  geom_text(aes(label = paste0(round(percent*100, 1), '%'),
                y = percent + 0.06), size = 6) +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 20))  +
  ylim(NA, 1.1)

ggsave('figures/sentiment_proportions_by_race_sd.png',
       dpi = 300,
       height = 4,
       width= 16,
       units = 'in')

# proportion - gender -----------------------------------------------------

stat_test_gender <- df_sd %>% 
  group_by(gender, sentiment) %>% 
  summarise(count = n()) %>% 
  group_by(gender) %>% 
  mutate(all = sum(count),
         count_ = all - count,
         prop = count / all) %>% 
  group_by(gender)

prop_test_rslt_gender <- pairwise_prop_test(
  x = as.matrix(stat_test_gender[,c('count', 'count_')]),
  conf.level = 0.05,
  p.adjust.method = "fdr") %>% 
  mutate(diff = abs(as.numeric(group1) - as.numeric(group2))) %>% 
  filter((diff == 3) | (diff == 6) | (diff == 9))

df_sd %>% 
  group_by(gender, sentiment) %>%
  summarise(count = n()) %>%
  group_by(gender) %>%
  mutate(all = sum(count),
         percent = count/all) %>%
  ggplot(aes(x = gender, y = percent,
             color = sentiment, fill = sentiment)) +
  geom_bar(stat = 'identity',
           position = 'dodge',
           alpha = 0.6) +
  geom_signif(
    # inherit.aes = FALSE,
    data = data.frame(sentiment = c("negative",
                                    "neutral",
                                    "positive"),
                      start = c("Female",
                                "Female",
                                "Female"),
                      end = c("Male",
                              "Male",
                              "Male"),
                      y = c(0.45,0.65,0.4),
                      label = prop_test_rslt_gender$p.adj.signif[c(1,2,3)]),
    aes(xmin = start,
        xmax = end,
        annotations = label, 
        y_position = y),
    manual = TRUE
  )+
  
  facet_wrap(~sentiment, ncol = 3) +
  scale_color_manual(limits = c('negative', 'positive', 'neutral'),
                     values = wes_palette(n=3, 
                                          name="Darjeeling1")) +
  scale_fill_manual(limits = c('negative', 'positive', 'neutral'),
                    values = wes_palette(n=3, 
                                         name="Darjeeling1"))+
  theme_minimal() +
  geom_text(aes(label = paste0(round(percent*100, 1), '%'),
                y = percent + 0.06), size = 6) +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 20))  +
  ylim(NA, 1.1)

ggsave('figures/sentiment_proportions_by_gender_sd.png',
       dpi = 300,
       height = 4,
       width= 16,
       units = 'in')


# proportion test - age ---------------------------------------------------

age_labs <- c(paste(seq(0, 95, by = 20), seq(0 + 20 - 1, 100 - 1, by = 20),
                    sep = "-"), paste(100, "+", sep = ""))

stat_test_age <- df_sd %>% 
  mutate(age_group = cut(age, breaks = c(seq(0, 100, by = 20), Inf), labels = age_labs, right = FALSE)) %>% 
  group_by(age_group, sentiment) %>% 
  summarise(count = n()) %>% 
  group_by(age_group) %>% 
  mutate(all = sum(count),
         count_ = all - count,
         prop = count / all) %>% 
  group_by(age_group)

prop_test_rslt_age <- pairwise_prop_test(
  x = as.matrix(stat_test_age[,c('count', 'count_')]),
  conf.level = 0.05,
  p.adjust.method = "fdr") %>% 
  mutate(diff = abs(as.numeric(group1) - as.numeric(group2))) %>% 
  filter((diff == 3) | (diff == 6) | (diff == 9))

df_sd %>% 
  mutate(age_group = cut(age, breaks = c(seq(0, 100, by = 20), Inf), labels = age_labs, right = FALSE)) %>% 
  group_by(age_group, sentiment) %>%
  summarise(count = n()) %>%
  group_by(age_group) %>%
  mutate(all = sum(count),
         percent = count/all) %>%
  ggplot(aes(x = age_group, y = percent,
             color = sentiment, fill = sentiment)) +
  geom_bar(stat = 'identity',
           position = 'dodge',
           alpha = 0.6) +
  geom_signif(
    # inherit.aes = FALSE,
    data = data.frame(sentiment = c("negative","negative","negative","negative","negative","negative",
                                    "neutral","neutral","neutral","neutral","neutral","neutral",
                                    "positive","positive","positive","positive","positive","positive"),
                      start = c("0-19","20-39","40-59","0-19","20-39","0-19",
                                "0-19","20-39","40-59","0-19","20-39","0-19",
                                "0-19","20-39","40-59","0-19","20-39","0-19"),
                      end = c("20-39","40-59","60-79","40-59","60-79","60-79",
                              "20-39","40-59","60-79","40-59","60-79","60-79",
                              "20-39","40-59","60-79","40-59","60-79","60-79"),
                      y = c(0.45,0.45,0.45,0.55,0.65,0.75,
                            0.75,0.75,0.75,0.85,0.95,1.05,
                            0.5,0.5,0.5,0.6,0.7,0.8),
                      label = prop_test_rslt_age$p.adj.signif[c(1,5,12,4,11,10,
                                                                2,7,15,6,14,13,
                                                                3,9,18,8,17,16)]),
    aes(xmin = start,
        xmax = end,
        annotations = label, 
        y_position = y),
    manual = TRUE
  )+
  
  facet_wrap(~sentiment, ncol = 3) +
  scale_color_manual(limits = c('negative', 'positive', 'neutral'),
                     values = wes_palette(n=3, 
                                          name="Darjeeling1")) +
  scale_fill_manual(limits = c('negative', 'positive', 'neutral'),
                    values = wes_palette(n=3, 
                                         name="Darjeeling1"))+
  theme_minimal() +
  geom_text(aes(label = paste0(round(percent*100, 1), '%'),
                y = percent + 0.06), size = 6) +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 20))  +
  labs(
    x = "age group"
  )
ylim(NA, 1.1)

ggsave('figures/sentiment_proportions_by_age_sd.png',
       dpi = 300,
       height = 4,
       width= 16,
       units = 'in')
