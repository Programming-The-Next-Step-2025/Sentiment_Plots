library("rwhatsapp")
library("dplyr")
library("ggplot2"); theme_set(theme_minimal())
library("lubridate")
library("tidyr")
library("roxygen2")
library("stringr")
library("ggimage")
library("tidytext")



#history <- system.file("extdata", "sample.txt", package = "rwhatsapp")
#chat <- rwa_read(history)

chat <- rwa_read("_chat_demo.txt") %>%
  filter(!is.na(author)) # remove messages without author
chat

wordcloud_data <- chat %>%
  mutate(
    text = stringr::str_remove_all(text, "<a\\s+[^>]*>|</a>"),  # Remove <a> tags
    text = stringr::str_remove_all(text, "\\s*<[^>]+>"),        # Remove other HTML tags
    text = stringr::str_remove_all(text, "http\\S+|www\\S+")   # Remove URLs
  ) %>%
  unnest_tokens(output = "word", input = text) %>%  # Tokenize text into words
  filter(!is.na(word)) %>%  # Remove NA words
  filter(str_detect(word, "[a-z]+")) %>%  # Remove non-alphabetic words (e.g., numbers, punctuation)
  filter(!word %in% c(get_stopwords("en"), "media", "omitted", "deleted", "message", "you", "this")) %>%  # Remove stop words and common irrelevant terms
  count(author, word, sort = TRUE)  # Count word frequencies per author

wordcloud_data %>%
  group_by(author) %>%
  ggplot(aes(label = word, size = n, color = n)) +
  ggwordcloud::geom_text_wordcloud() +
  scale_size_area(max_size = 10) +
  scale_color_gradient(low = "grey", high = brewer.pal(8, "Dark2")[1]) +
  facet_wrap(~author, ncol = 3, scales = "free") +
  labs(title = "Word Clouds by Author") +
  theme_minimal()

# Step 3: Pre-process for sentiment analysis
weekly_sentiment_data <- chat %>%
  mutate(day = date(time)) %>%
  unnest_tokens(output = "word", input = text) %>%
  filter(!is.na(word)) %>%
  filter(str_detect(word, "[a-z]+")) %>%
  filter(!word %in% c(get_stopwords("en"), "media", "omitted", "deleted", "message", "you", "this")) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(author, day, sentiment) %>%  # Per day
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative) %>%  # Daily score
  mutate(week = lubridate::floor_date(day, "week")) %>%  # Convert day to week
  group_by(author, week) %>%
  summarise(
    positive = sum(positive, na.rm = TRUE),  # Sum positive counts per week
    negative = sum(negative, na.rm = TRUE),  # Sum negative counts per week
    sentiment_score = sum(sentiment_score, na.rm = TRUE)  # Net sentiment score per week (you can change to mean if preferred)
  ) %>%
  ungroup()

ggplot(weekly_sentiment_data, aes(x = week, y = sentiment_score, group = author, color = author)) +
  geom_line() +  # Line chart for fluctuations
  geom_point() +  # Add points for clarity
  labs(x = "Week", y = "Sentiment Score", title = "Weekly Sentiment Fluctuations by Author") +
  theme_minimal()

#
#Discriptive table===============
daysed <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
no_of_days_of_messages <- chat %>%
  mutate(day=date(time)) %>%
  summarise(no=length(unique(day))) %>%
  pull(no)
most_oldest_date <- chat %>%
  mutate(day=date(time))%>%
  arrange(day) %>%
  slice(1) %>%
  select(Oldest=day) %>%
  pull(Oldest)
most_recent_date <- chat %>%
  mutate(day=date(time))%>%
  arrange(desc(day)) %>%
  slice(1) %>%
  select(Newest=day) %>%
  pull(Newest)

#total no of days
total_no_of_days <- as.numeric(most_recent_date-most_oldest_date)
# total no of days with messages
no_of_days_of_messgaes <- as.numeric(total_no_of_days-no_of_days_of_messages)
# % days without msg
percent_days_without_messages <- round(no_of_days_of_messgaes/total_no_of_days*100,2)
#most active day
most_active_day <- chat %>%
  mutate(date = date(time)) %>%
  count(date) %>%
  top_n(1) %>%
  pull(date)
#most active day of week
most_active_day_of_week <- chat %>%
  mutate(day = wday(as.Date(time),week_start = 1)) %>%
  count(day) %>%
  top_n(1) %>%
  pull(day)
most_active_day_of_week <- daysed[most_active_day_of_week]
#total no of messages
total_no_of_messages <- chat %>% count()
# no of unique users
total_no_of_users <- n_distinct(chat$author)
# no of messages per day
messages_per_day <- as_tibble(total_no_of_messages/no_of_days_of_messgaes)
# no of deleted messages
deleted_messages <- chat %>% filter(text=="This message was deleted" | text=="You deleted this message") %>% count()

no_of_smiley <- chat %>% unnest(emoji) %>% count() #no of smileys
unique_smiley <- chat %>%  unnest(emoji) %>% count(emoji, sort = TRUE) %>% count()
no_of_links <- chat%>% filter(str_detect(text,"www.")| str_detect(text,"http:")|str_detect(text,"https:")|str_detect(text,"youtu.be")) %>% count() #no of links

#all these values can be easily combined into a table
# Create the table
summary_table <- tibble(
  Metric = c(
    "Number of Days with Messages",
    "Oldest Date",
    "Newest Date",
    "Total Number of Days",
    "Number of Days without Messages",
    "Percent Days without Messages",
    "Most Active Day",
    "Most Active Day of Week",
    "Total Number of Messages",
    "Total Number of Users",
    "Messages per Day",
    "Number of Deleted Messages",
    "Number of Smileys",
    "Number of Links"
  ),
  Value = unlist(c(
    no_of_days_of_messages,
    as.character(most_oldest_date),
    as.character(most_recent_date),
    total_no_of_days,
    no_of_days_of_messgaes,
    percent_days_without_messages,
    as.character(most_active_day),
    most_active_day_of_week,
    total_no_of_messages,
    total_no_of_users,
    messages_per_day,
    deleted_messages,
    no_of_smiley,
    unique_smiley,
    no_of_links
  ))
)

print(summary_table)

##  no. of msg by date ====
msg_date <- chat %>%
  mutate(day = date(time)) %>%
  count(day) %>% arrange(desc(n))
head(msg_date)

#' Creates a bar plot of message counts per day.
#'
#' This function takes a dataframe containing message data and generates a bar plot
#' showing the number of messages sent on each day.
#'
#' @param df A dataframe with a 'time' column (POSIXct or similar).
#'   The 'time' column should represent the timestamp of the message.
#' @return A ggplot object representing the bar plot.  This object can be further
#'   modified using ggplot2 functions (e.g., adding themes, changing colors).
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @export
#' @examples
#' # Assuming you have a dataframe called chat_data with a 'time' column:
#' # Generate the plot:
#' # daily_message_plot <- create_daily_message_count_plot(chat_data)
#'
#' # Display the plot:
#' # print(daily_message_plot)
#'
#' # Example with dummy data:
#' time <- seq(as.POSIXct("2023-01-01 00:00:00"), as.POSIXct("2023-01-05 00:00:00"), by = "hour")
#' chat_data <- data.frame(time = time)
#' daily_message_plot <- create_daily_message_count_plot(chat_data)
#' print(daily_message_plot)

create_daily_message_count_plot <- function(df) {
  if (!("time" %in% names(df))) {
    stop("Dataframe must contain a 'time' column.")
  }

  df %>%
    mutate(day = date(time)) %>%
    count(day) %>%
    ggplot(aes(x = day, y = n)) +
    geom_bar(stat = "identity") +
    ylab("") + xlab("") +
    ggtitle("Messages per day") +
    + theme_minimal()
}

create_daily_message_count_plot(chat)


# message counts by author =====
#'
#' This function takes a dataframe containing message data and generates a bar plot
#' showing the number of messages sent by each author.  The plot is horizontally
#' oriented for better readability of author names.
#'
#' @param df A dataframe with 'time' (POSIXct or similar) and 'author' columns.
#'   The 'time' column should represent the timestamp of the message, and the
#'   'author' column should contain the name or identifier of the message sender.
#' @return A ggplot object representing the bar plot.  This object can be further
#'   modified using ggplot2 functions (e.g., adding themes, changing colors).
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @export
#' @examples
#' # Assuming you have a dataframe called chat_data with 'time' and 'author' columns:
#' # Generate the plot:
#' # message_plot <- create_message_count_plot(chat_data)
#'
#' # Display the plot:
#' # print(message_plot)
#'
#' # Example with dummy data:
#' time <- seq(as.POSIXct("2023-01-01 00:00:00"), as.POSIXct("2023-01-01 00:00:10"), by = "sec")
#' author <- c("Alice", "Bob", "Alice", "Charlie", "Bob", "Alice", "Bob", "Charlie", "Alice", "Bob", "Alice")
#' chat_data <- data.frame(time = time, author = author)
#' message_plot <- create_message_count_plot(chat_data)
#' print(message_plot)

create_message_count_plot <- function(df) {
  if (!("time" %in% names(df))) {
    stop("Dataframe must contain a 'time' column.")
  }
  if (!("author" %in% names(df))) {
    stop("Dataframe must contain an 'author' column.")
  }

  df %>%
    mutate(day = date(time)) %>%
    count(author) %>%
    ggplot(aes(x = reorder(author, n), y = n)) +
    geom_bar(stat = "identity") +
    ylab("") + xlab("") +
    coord_flip() +
    ggtitle("Number of messages")
}

create_message_count_plot(chat)



roxygen2::roxygenize

## emoji ====

emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/",
                            tolower(hex_runes1), ".png"))


chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 3, n) %>%
  left_join(emoji_data, by = "emoji") %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 3, scales = "free_y") +
  ggtitle("Most often used emojis")

## common words =====

# under construction
chat_cleaned %>%
  select(word, author) %>%
  filter(!word %in% to_remove) %>%
  mutate(word = gsub(".com", "", word)) %>%
  mutate(word = gsub("^gag", "9gag", word)) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(term = word, document = author, n = n) %>%
  filter(n > 5) %>%
  group_by(author) %>%
  top_n(n = 4, tf_idf) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 3, scales = "free_y") +
  scale_x_reordered()+
  ggtitle("Common Words Used")

# word cloud
df<-chat %>% unnest_tokens(input = text, output = word) %>% filter(!word %in% to_remove) %>% count(word, sort = TRUE)
set.seed(1234) # for reproducibility
wordcloud(words = df$word, freq = df$n, min.freq = 5,
          max.words=250, random.order=FALSE, rot.per=0,
          colors=brewer.pal(8, "Dark2"))


## message time ====

# to find the hour when most messages are sent
title<-paste0("Most Messages happen at hour ",chat %>% mutate(hour = hour(time)) %>% count(hour) %>% top_n(1) %>% pull(hour))
chat %>%
  mutate(hour = hour(time)) %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_bar(stat = "identity",fill="steelblue") +
  ylab("") + xlab("Messages for every hour") +
  ggtitle(title)+
  scale_x_continuous(breaks = 0:23)

#to find which day of the week most messages are being sent
daysed<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
most_active_day_of_week<-chat %>% mutate(day = wday(as.Date(time),week_start = 2)) %>% count(day) %>% top_n(1) %>% pull(day)
most_active_day_of_week<-daysed[most_active_day_of_week]
title<-paste0("Most messages are sent on a ",most_active_day_of_week)
days<-c("Mon","Tue","Wed","Thu","Fri","Sat","Sun") # for axis labels

chat %>%
  mutate(day = wday(as.Date(time),week_start = 2)) %>%
  count(day)  %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity", fill="steelblue") +
  ylab("") + xlab("Messages Per Day of week") +
  ggtitle(title) +
  scale_x_continuous(breaks = 1:7,labels=days)+
  scale_x_continuous(breaks = 1:7,labels=days)


