library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(tidytext)
library(sentimentr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Sentiment & NPS/CSAT Analysis of Review Comments - Investor Insights"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File",
                accept = c("text/csv", ".csv")),
      uiOutput("column_ui"),
      uiOutput("analyze_ui"),
      hr(),
      helpText("Note: Upload a CSV with one column of review comments.")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Data Summary",
          h4("Preview of Uploaded Data"),
          tableOutput("data_preview")
        ),
        tabPanel("Plots",
          h4("Sentiment Score Distribution"),
          plotOutput("sentiment_plot")
        ),
        tabPanel("Results",
          h4("Sentiment Breakdown"),
          verbatimTextOutput("sentiment_breakdown"),
          fluidRow(
            column(6, h4("Top Positive Words"), verbatimTextOutput("positive_keywords")),
            column(6, h4("Top Negative Words"), verbatimTextOutput("negative_keywords"))
          ),
          hr(),
          h4("NPS Metrics"),
          verbatimTextOutput("nps_promoters"),
          verbatimTextOutput("nps_neutrals"),
          verbatimTextOutput("nps_detractors"),
          h4("NPS Score"), verbatimTextOutput("nps_score"),
          h4("CSAT Score"), verbatimTextOutput("csat_score")
        ),
        tabPanel("Interpretation",
          h4("What These Scores Mean:"),
          p("Sentiment Score: Numerical representation of sentence sentiment from -1 (negative) to +1 (positive)."),
          p("NPS Score: Net Promoter Score calculated as %Promoters - %Detractors, reflecting customer loyalty."),
          p("CSAT Score: Customer Satisfaction based on percentage of positive sentiments."),

          h4("Business Insights:"),
          verbatimTextOutput("business_insights")
        )
      )
    )
  )
)

# Server definition moved below to stay within this block
shinyApp(ui = ui, server = function(input, output, session) {
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })

  output$data_preview <- renderTable({
    req(data())
    head(data(), 10)
  })

  output$column_ui <- renderUI({
    req(input$file)
    selectInput("text_column", "Select Column with Review Comments:", choices = names(data()))
  })

  output$analyze_ui <- renderUI({
    req(input$text_column)
    actionButton("analyze", "Perform Analysis")
  })

  observeEvent(input$analyze, {
    req(input$text_column)
    review_data <- data()
    text_corpus <- review_data[[input$text_column]]

    sentiment_scores <- sentiment(text_corpus)
    sentiment_classified <- sentiment_scores %>%
      mutate(sentiment_category = case_when(
        sentiment > 0.2 ~ "Positive",
        sentiment < -0.2 ~ "Negative",
        TRUE ~ "Neutral"
      ))

    sentiment_counts <- table(sentiment_classified$sentiment_category)
    sentiment_percentages <- prop.table(sentiment_counts) * 100

    output$sentiment_plot <- renderPlot({
      ggplot(sentiment_scores, aes(x = sentiment)) +
        geom_histogram(binwidth = 0.2, fill = "steelblue", color = "black") +
        labs(title = "Sentiment Score Distribution", x = "Sentiment Score", y = "Frequency") +
        theme_minimal()
    })

    output$sentiment_breakdown <- renderPrint({
      cat("Positive:", round(sentiment_percentages["Positive"], 1), "%\n")
      cat("Neutral:", round(sentiment_percentages["Neutral"], 1), "%\n")
      cat("Negative:", round(sentiment_percentages["Negative"], 1), "%\n")
    })

    tidy_reviews <- review_data %>%
      select(text = !!sym(input$text_column)) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words, by = "word")

    positive_words <- tidy_reviews %>%
      inner_join(get_sentiments("bing") %>% filter(sentiment == "positive"), by = "word") %>%
      count(word, sort = TRUE) %>% head(10)

    negative_words <- tidy_reviews %>%
      inner_join(get_sentiments("bing") %>% filter(sentiment == "negative"), by = "word") %>%
      count(word, sort = TRUE) %>% head(10)

    output$positive_keywords <- renderPrint({
      if (nrow(positive_words) > 0) {
        cat(paste(positive_words$word, collapse = ", "))
      } else {
        cat("No strong positive signals.")
      }
    })

    output$negative_keywords <- renderPrint({
      if (nrow(negative_words) > 0) {
        cat(paste(negative_words$word, collapse = ", "))
      } else {
        cat("No strong negative signals.")
      }
    })

    promoter_keywords <- c("love", "great", "excellent", "recommend", "best", "amazing", "fantastic")
    detractor_keywords <- c("terrible", "awful", "disappointed", "worst", "horrible", "bad", "poor")

    promoter_count <- sum(str_detect(tolower(text_corpus), str_c(promoter_keywords, collapse = "|")))
    detractor_count <- sum(str_detect(tolower(text_corpus), str_c(detractor_keywords, collapse = "|")))
    total_reviews <- length(text_corpus)
    neutral_count <- total_reviews - promoter_count - detractor_count
    nps_score <- round(((promoter_count - detractor_count) / total_reviews) * 100, 1)
    csat_score <- round(sentiment_percentages["Positive"], 1)

    output$nps_promoters <- renderPrint({ paste("Promoters:", promoter_count) })
    output$nps_neutrals <- renderPrint({ paste("Neutrals:", neutral_count) })
    output$nps_detractors <- renderPrint({ paste("Detractors:", detractor_count) })
    output$nps_score <- renderPrint({ paste(nps_score, "(higher is better)") })
    output$csat_score <- renderPrint({ paste0(csat_score, "% positive sentiment") })

    output$business_insights <- renderPrint({
      insights <- c()
      if (nps_score > 50) {
        insights <- c(insights, "Strong promoter base suggests high loyalty and potential organic growth.")
      } else if (nps_score < 0) {
        insights <- c(insights, "Negative NPS is a red flag. Indicates dissatisfaction and possible churn.")
      }

      if (csat_score > 75) {
        insights <- c(insights, "High CSAT indicates good customer experience and retention.")
      } else if (csat_score < 50) {
        insights <- c(insights, "Low CSAT indicates a need to address product or service quality.")
      }

      if (length(insights) == 0) {
        insights <- c("Scores are moderate. Continuous monitoring and customer feedback loops are recommended.")
      }

      cat(paste(insights, collapse = "\n"))
    })
  })
})
