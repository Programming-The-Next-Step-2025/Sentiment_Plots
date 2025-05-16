library("rwhatsapp")
library("dplyr")
library("ggplot2"); theme_set(theme_minimal())
library("lubridate")
library("tidyr")
library("roxygen2")

#history <- system.file("extdata", "sample.txt", package = "rwhatsapp")
#chat <- rwa_read(history)

chat_Y <- rwa_read("_chat_coded_UTF-8.txt") %>%
  filter(!is.na(author)) # remove messages without author
chat_Y

# no. of msg by date
msg_date <- chat_Y %>%
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
    ggtitle("Messages per day")
}

create_daily_message_count_plot(chat_Y)


#' Creates a bar plot of message counts by author.
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

create_message_count_plot(chat_Y)


roxygen2::roxygenize

