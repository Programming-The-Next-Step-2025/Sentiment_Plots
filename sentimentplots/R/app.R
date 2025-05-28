
# app.R

# Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(rwhatsapp)
library(wordcloud)
library(ggwordcloud)
library(tidytext)
library(RColorBrewer)
library(ggimage)


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("WhatsApp Chat Analyzer"),

  sidebarLayout(
    sidebarPanel(
      h3("Upload Your Chat File"),
      fileInput("file", "Upload a WhatsApp chat file (e.g., .txt)",
                accept = c(".txt")),
      helpText("This is a visualisation tool for the chat histories that you are curious about :)"),

      # Privacy disclaimer below the upload box
      tags$div(
        tags$p(tags$small(
          style = "color: red; font-weight: bold;",
          "Privacy Disclaimer:
          Uploaded chat data is processed temporarily and is not stored persistently.
          If you have privacy concerns, feel free to download the code from the repository (https://github.com/Programming-The-Next-Step-2025/Sentiment_Plots) and run the analysis locally."
        ))
      ),

    ),

    mainPanel(
      tabsetPanel(
        tabPanel("How to upload chat",
                 h3("How to upload chat"),
                 img(src = "export.jpeg", width = "100%", alt = "Example Image")), #instructions
        tabPanel("Summary Table", tableOutput("summaryTable")),
        tabPanel("Daily Message Count Plot", plotOutput("dailyMessagePlot")),
        #tabPanel("Message Count by Author", plotOutput("messageCountPlot")),
        tabPanel("Message Time Analysis", plotOutput("messageHourPlot"), plotOutput("messageDayPlot")),
        tabPanel("Top Emojis", plotOutput("topEmojisPlot")),
        tabPanel("Word Cloud", plotOutput("wordCloudPlot"), width = "100%"),
        tabPanel("Sentimen Analysis", plotOutput("SentimentPlot"))
      )
    )
  )
)


# Server ------------------------------------------------------------------
server <- function(input, output) {
  # Reactive data processing
  chat_data <- reactive({
    req(input$file)  # Wait for file upload
    file_path <- input$file$datapath  # Get the uploaded file path
    chat <- rwa_read(file_path) %>% filter(!is.na(author))  # Load and filter data
    return(chat)
  })

  # Compute summary table (reactive)
  summary_table <- reactive({
    chat <- chat_data()  # Get the uploaded data

    # Your original computations
    daysed <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    no_of_days_of_messages <- chat %>% mutate(day = date(time)) %>% summarise(no = length(unique(day))) %>% pull(no)
    most_oldest_date <- chat %>% mutate(day = date(time)) %>% arrange(day) %>% slice(1) %>% select(Oldest = day) %>% pull(Oldest)
    most_recent_date <- chat %>% mutate(day = date(time)) %>% arrange(desc(day)) %>% slice(1) %>% select(Newest = day) %>% pull(Newest)
    total_no_of_days <- as.numeric(most_recent_date - most_oldest_date)
    no_of_days_of_messgaes <- as.numeric(total_no_of_days - no_of_days_of_messages)
    percent_days_without_messages <- round(no_of_days_of_messgaes / total_no_of_days * 100, 2)
    most_active_day <- chat %>% mutate(date = date(time)) %>% count(date) %>% top_n(1) %>% pull(date)
    most_active_day_of_week <- chat %>% mutate(day = wday(as.Date(time), week_start = 1)) %>% count(day) %>% top_n(1) %>% pull(day)
    most_active_day_of_week <- daysed[most_active_day_of_week]
    total_no_of_messages <- chat %>% count() %>% pull(n)
    total_no_of_users <- n_distinct(chat$author)
    messages_per_day <- (total_no_of_messages / no_of_days_of_messgaes)  # Fixed to a single value
    deleted_messages <- chat %>% filter(text == "This message was deleted" | text == "You deleted this message") %>% count() %>% pull(n)
    no_of_smiley <- chat %>% unnest(emoji) %>% count() %>% pull(n)
    unique_smiley <- chat %>% unnest(emoji) %>% count(emoji, sort = TRUE) %>% count() %>% pull(n)
    no_of_links <- chat %>% filter(str_detect(text, "www.") | str_detect(text, "http:") | str_detect(text, "https:") | str_detect(text, "youtu.be")) %>% count() %>% pull(n)

    # Create the table
    tibble(
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
        "Number of Unique Smileys",
        "Number of Links"
      ),
      Value = c(
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
      )
    )
  })

  output$summaryTable <- renderTable({ summary_table() })

  # Render plots
  output$dailyMessagePlot <- renderPlot({
    #create_daily_message_count_plot(chat_data())
    chat_data() %>%
      mutate(day = date(time)) %>%
      count(day) %>%
      ggplot(aes(x = day, y = n)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      ylab("") + xlab("") +
      ggtitle("Messages per day") +
      theme_minimal()
  })

  #output$messageCountPlot <- renderPlot({ create_message_count_plot(chat_data()) })

  # emoji
  output$topEmojisPlot <- renderPlot({
    emoji_data <- rwhatsapp::emojis %>% mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", tolower(hex_runes1), ".png"))
    chat_data() %>%
      unnest(emoji) %>%
      count(author, emoji, sort = TRUE) %>%
      group_by(author) %>%
      top_n(n = 6, n) %>%
      left_join(emoji_data, by = "emoji") %>%
      ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
      geom_col(show.legend = FALSE) +
      ylab("") +
      xlab("") +
      coord_flip() +
      geom_image(aes(y = n + 20, image = emoji_url)) +
      facet_wrap(~author, ncol = 3, scales = "free_y") +
      ggtitle("Most often used emojis") +
      theme_minimal() +
      scale_fill_manual(values = c("purple", "brown"))
  })

  output$messageHourPlot <- renderPlot({
    title <- paste0("Most Messages happen at hour ", chat_data() %>% mutate(hour = hour(time)) %>% count(hour) %>% top_n(1) %>% pull(hour))
    chat_data() %>% mutate(hour = hour(time)) %>% count(hour) %>% ggplot(aes(x = hour, y = n)) + geom_bar(stat = "identity", fill = "skyblue") + ylab("") + xlab("Messages for every hour") + ggtitle(title) + scale_x_continuous(breaks = 0:23) + theme_minimal()
  })


  output$messageDayPlot <- renderPlot({
    daysed <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    most_active_day_of_week <- chat_data() %>% mutate(day = wday(as.Date(time), week_start = 2)) %>% count(day) %>% top_n(1) %>% pull(day)
    most_active_day_of_week <- daysed[most_active_day_of_week]
    title <- paste0("Most messages are sent on a ", most_active_day_of_week)
    days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    chat_data() %>% mutate(day = wday(as.Date(time), week_start = 2)) %>% count(day) %>% ggplot(aes(x = day, y = n)) + geom_bar(stat = "identity", fill = "skyblue") + ylab("") + xlab("Messages Per Day of week") + ggtitle(title) + scale_x_continuous(breaks = 1:7, labels = days) + theme_minimal()
  })


output$wordCloudPlot <- renderPlot({
  wordcloud_data <- chat_data() %>%
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
    slice_max(n, n = 50, with_ties = FALSE) %>% # Select top 50 words per author
    ggplot(aes(label = word, size = n, color = n)) +
    ggwordcloud::geom_text_wordcloud() +
    scale_size_area(max_size = 10) +
    scale_color_gradient(low = "grey", high = brewer.pal(8, "Dark2")[1]) +
    facet_wrap(~author, ncol = 3, scales = "free") +
    labs(title = "Word Clouds by Author") +
    theme_minimal()

})


output$SentimentPlot <- renderPlot({
  weekly_sentiment_data <- chat_data() %>%
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
    theme_minimal() +
    scale_color_manual(values = c("purple", "brown"))
})



}
# Run the app
shinyApp(ui = ui, server = server)



