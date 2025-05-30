\name{launch_sentimentplot}
\alias{launch_sentimentplot}
\title{Launch the Shiny App}
\description{
  This function launches the Shiny app included in the sentimentplot package, where you can upload your WhatsApp chat history for data analysis and visualisations. 
}
\usage{
launch_shinyapp(...)
}
\arguments{
  \item{...}{
    Arguments passed to \code{\link[shiny]{runApp}}. This allows
    you to control the app's behavior, such as the port it runs on.
  }
}
\details{
  The Shiny app is located in the \code{inst/shiny-examples/my_app} directory of the
  package.  This function uses \code{\link[shiny]{runApp}} to launch the app.
}
\examples{
\dontrun{
  # Launch the app with default settings
  launch_sentimentplot()

  # Launch the app on a specific port
  launch_shinyapp(port = 8080)
}
}

