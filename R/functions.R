###claimR package 1.2###

#' @import dplyr
#' @import e1071
#' @import nnet
#' @import reticulate
#' @import shiny
#' @import shinyjs
#' @import stringr
#' @import text2vec
#' @import tidytext
#' @import tidyr

# Set Python Environment for Deep Learning Models

set_python_env <- function(python_path) {
  use_python(python_path, required = TRUE)
  print(paste("Using Python from:", python_path))
}


#' Split Text into Sentences
#' @name split_sentences
#' @param df A data frame
#' @param text_col The name of the text column in the data frame
#' @param circ A boolean indicating whether to include surrounding sentences
#' @return A data frame with sentence IDs
#' @export
split_sentences <- function(df, text_col, circ = FALSE) {
  df <- df %>%
    mutate(sentences = str_split(.data[[text_col]], "(?<!\\b[A-Z])(?<=[.!?])\\s+")) %>%
    unnest(sentences) %>%
    mutate(id = row_number())
  
  if (circ) {
    df <- df %>%
      mutate(
        prev_sentence = lag(sentences, default = ""),
        next_sentence = lead(sentences, default = "")
      ) %>%
      mutate(context = paste(prev_sentence, sentences, next_sentence))
  }
  
  return(df)
}


#' Identify Claims in Text
#' @name claim_id
#' @param df A data frame
#' @param text_col The name of the text column
#' @param search_word The word to search for
#' @param synon Boolean indicating whether to use synonyms
#' @return Data frame with an "identifier" column
#' @export
claim_id <- function(df, text_col, search_word, synon = FALSE) {
  if (synon) {
    synonyms <- tryCatch(text2vec::synonyms(search_word), error = function(e) character(0))
    search_words <- unique(c(search_word, synonyms))
  } else {
    search_words <- search_word
  }
  
  regex_pattern <- paste0("(?i)\\b(", paste(search_words, collapse = "|"), ")\\b")
  
  df <- df %>%
    rowwise() %>%
    mutate(identifier = ifelse(any(str_detect(.data[[text_col]], regex_pattern)), search_word, "FALSE"))
  
  return(df)
}


#' ClaimR - Finding Representative Claims in TXT
#' @name run_claimr
#' @param df The data frame containing the claims.
#' @param text_col The name of the column containing the text (as a string).
#' @export
#' @import shiny
#' @import shinyjs
#' @examples


run_claimr <- function(df, text_col) {

  # Ensure all necessary columns exist
  if (!"class" %in% colnames(df)) {
    df <- df %>% dplyr::mutate(class = NA)
  }
  if (!"distance" %in% colnames(df)) {
    df <- df %>% dplyr::mutate(distance = NA)
  }
  
  # Define the UI
  ui <- shiny::fluidPage(
    titlePanel("ClaimR - Finding Representative Claims in TXT"),
    
    # Create the display area
    mainPanel(
      div(style="text-align:center;font-size:30px;font-weight:bold;",
          uiOutput("text"), tags$br(),
          uiOutput("class"), tags$br(),
          uiOutput("distance")
      ),
      
      # Navigation Buttons
      fluidRow(
        column(6, align = "center", actionButton("back", "Back", class = "btn-primary")),
        column(6, align = "center", actionButton("forward", "Forward", class = "btn-primary"))
      ),
      
      # Create the Claim, No Claim and ? buttons
      div(style="display:flex;justify-content:space-around;margin-top:30px;",
          actionButton("claim", "Claim", class = "btn-success"),
          actionButton("no_claim", "No Claim", class = "btn-danger"),
          actionButton("unclear", "?", class = "btn-warning")
      ),
      
      # Create the Export button
      div(style="text-align:center;margin-top:50px;",
          actionButton("update", "Update Classifier")
      ),
      
      # Add this in the UI section
      div(style = "text-align:center; margin-top:20px;",
          actionButton("close_app", "Close App", class = "btn-danger")
      )
    ),
    
    # JavaScript for Keyboard Shortcuts
    tags$script(HTML("
      $(document).on('keydown', function(e) {
        if (e.key === 'y' || e.key === 'Y' || e.key === 'z' || e.key === 'Z') {
          $('#claim').trigger('click');
        } else if (e.key === 'n' || e.key === 'N') {
          $('#no_claim').trigger('click');
        } else if (e.key === 'q' || e.key === 'Q') {
          $('#unclear').trigger('click');
        }
      });
    "))
  )
  
  # Define the server
  server <- function(input, output, session) {
    # Use reactiveValues to store the data without constantly modifying the global environment
    data <- reactiveValues(df = df)
    
    # Create a reactive variable for the current row
    current_row <- reactiveVal(1)
    
    # Define the output UI elements
    output$text <- renderUI({
      req(data$df[[text_col]][current_row()])  # Ensure the text exists
      div(data$df[[text_col]][current_row()], style="font-size:30px;font-weight:bold;")
    })
    
    output$class <- renderUI({
      div(data$df$class[current_row()], style="font-size:30px;")
    })
    
    output$distance <- renderUI({
      if ("distance" %in% colnames(data$df)) {
        div(data$df$distance[current_row()], style="font-size:30px;")
      }
    })
    
    # Update the current row when the Back or Forward button is clicked
    observeEvent(input$back, {
      current_row(max(1, current_row() - 1))
    })
    
    observeEvent(input$forward, {
      current_row(min(nrow(data$df), current_row() + 1))
    })
    
    # Update the class variable in memory without writing to file immediately
    classify <- function(label) {
      data$df$class[current_row()] <- label
      if (current_row() < nrow(data$df)) {
        current_row(current_row() + 1)  # Move to next sentence automatically
      }
    }
    
    observeEvent(input$claim, {
      classify(TRUE)
    })
    
    observeEvent(input$no_claim, {
      classify(FALSE)
    })
    
    observeEvent(input$unclear, {
      classify("?")
    })
    
    # Export classifier when the update button is clicked
    observeEvent(input$update, {
      write_csv(data$df, "classified_claims.csv")
    })
    
    # Close the app
    observeEvent(input$close_app, {
      readr::write_csv(data$df, "classified_claims.csv")  # Save once at the end
      assign("df", data$df, envir = .GlobalEnv)  # Update global environment
      stopApp()
    })
  }
  
  # Run the app
  shinyApp(ui, server)
}



#' Classify Representative Claims
#' @param df A data frame containing the text to classify
#' @param text_col The column name containing text
#' @param label_col The column name containing labels for training
#' @param training_df A data frame containing training data
#' @param training_text_col The column name containing text in the training data
#' @param method The classification method to use ("svm", "naive_bayes", "xlnet", "xlm-roberta", "mbert")
#' @return A data frame with classification results
#' @export
library(dplyr)
library(tidytext)
library(text2vec)
library(e1071)  # For SVM and Naïve Bayes
library(reticulate)

# Ensure `stop_words` exists
if (!exists("stop_words")) {
  data("stop_words", package = "tidytext")
}

library(dplyr)
library(tidytext)
library(text2vec)
library(e1071)  # For SVM and Naïve Bayes
library(reticulate)

# Ensure `stop_words` exists
if (!exists("stop_words")) {
  data("stop_words", package = "tidytext")
}

#' Classify Representative Claims
#' @name classify_representative_claims
#' @param df A data frame containing the text to classify
#' @param text_col The column name containing text
#' @param label_col The column name containing labels for training
#' @param training_df A data frame containing training data
#' @param training_text_col The column name containing text in the training data
#' @param method The classification method to use ("svm", "naive_bayes", "xlnet", "xlm-roberta", "mbert")
#' @return A data frame with classification results
#' @export
classify_representative_claims <- function(df, text_col, label_col, training_df, training_text_col, method) {
  
  if (method %in% c("svm", "naive_bayes")) {
    
    # Ensure training data has an ID
    training_df <- training_df %>%
      mutate(id = row_number())
    
    # Prepare training data
    training_data <- training_df %>%
      unnest_tokens(word, !!sym(training_text_col)) %>%
      filter(!word %in% stop_words$word) %>%
      count(id, word) %>%
      cast_dtm(id, word, n)
    
    # Convert to data.frame for modeling
    training_matrix <- as.matrix(training_data)
    training_data <- as.data.frame(training_matrix)
    
    training_data$label <- as.factor(training_df[[label_col]])
    
    # Train the chosen model
    if (method == "svm") {
      model <- svm(label ~ ., data = training_data)
    } else if (method == "naive_bayes") {
      model <- naiveBayes(label ~ ., data = training_data)
    }
    
    # Ensure test data has an ID
    df <- df %>%
      mutate(id = row_number())
    
    # Process test data
    test_data <- df %>%
      unnest_tokens(word, !!sym(text_col)) %>%
      filter(!word %in% stop_words$word) %>%
      count(id, word) %>%
      cast_dtm(id, word, n)
    
    # Convert to data.frame for prediction
    test_matrix <- as.matrix(test_data)
    test_data <- as.data.frame(test_matrix)
    
    # Classify the text
    predictions <- predict(model, test_data)
    df$predictions <- predictions
    
    return(df)
    
  } else if (method %in% c("xlnet", "xlm-roberta", "mbert")) {
    
    # Check if Python is available
    if (!py_available()) stop("Python environment is not set. Call set_python_env() first.")
    
    # Ensure Python script is sourced
    source_python("classify_text.py")
    
    # Call Python-based classifier
    df$probability <- classify_text(df[[text_col]], method)
    
    return(df)
    
  } else {
    stop("Invalid method. Choose 'svm', 'naive_bayes', 'xlnet', 'xlm-roberta', or 'mbert'.")
  }
}

#' Train Text Classifier
#' @name train_text_classifier
#' @param df A data frame containing labeled text
#' @param text_col The text column name
#' @param label_col The label column name
#' @param method Classification method ("svm" or "naive_bayes")
#' @return Trained model
#' @export
train_text_classifier <- function(df, text_col, label_col, method = "svm") {
  
  print(colnames(df)) 
  
  if (!(text_col %in% colnames(df))) stop(paste("Fehler: Spalte", text_col, "existiert nicht!"))
  if (!(label_col %in% colnames(df))) stop(paste("Fehler: Spalte", label_col, "existiert nicht!"))
  
  df <- df %>%
    mutate(id = row_number()) %>%
    unnest_tokens(word, {{ text_col }}) %>%
    filter(!word %in% stop_words$word) %>%
    count(id, word) %>%
    cast_dtm(id, word, n)
  
  print(dim(df))  # Debug: Anzahl Zeilen/Spalten
  
  if (nrow(df) == 0) stop
  
  df <- as.data.frame(as.matrix(df))
  
  if (nrow(df) != length(train_df[[label_col]])) {
    stop(paste("Error: Number of Labels (", length(train_df[[label_col]]), 
               ") does not match number of rows (", nrow(df), ")!"))
  }
  
  df$label <- as.factor(train_df[[label_col]][1:nrow(df)])
  
  gc() 
  
  if (method == "svm") {
    model <- svm(label ~ ., data = df, scale = FALSE, cachesize = 500)
  } else if (method == "naive_bayes") {
    model <- naiveBayes(label ~ ., data = df)
  } else {
    stop("Invalid method. Choose 'svm' or 'naive_bayes'.")
  }
  
  return(model)
}

#' Classify text using a trained model
#' @name classify_text 
#' @param df Data frame with text to classify
#' @param text_col Column name containing text
#' @param model Trained model (SVM or Naïve Bayes)
#' @return Data frame with predictions
#' @export
classify_text <- function(df, text_col, model) {
  
  df <- df %>%
    mutate(id = row_number()) %>%
    unnest_tokens(word, {{ text_col }}) %>%
    filter(!word %in% stop_words$word) %>%
    count(id, word) %>%
    cast_dtm(id, word, n)
  
  # Convert to data.frame
  df_matrix <- as.matrix(df)
  df <- as.data.frame(df_matrix)
  
  # Predict with the trained model
  predictions <- predict(model, df)
  df$predictions <- predictions
  
  return(df)
}

#' Classify text using Python-based models
#' @name Classify_with_Python
#' @param df A data frame containing text to classify
#' @param text_col The column name containing text
#' @param model_choice The Python model to use ("xlnet", "xlm-roberta", "mbert")
#' @return Data frame with predictions
#' @export
classify_with_python <- function(df, text_col, model_choice) {
  
  # Ensure Python script is loaded
  source_python(system.file("python", "classify_text.py", package = "claimR"))
  
  # Extract text column as a character vector
  texts <- df[[text_col]]
  
  # Call Python function
  predictions <- classify_text(texts, model_choice)
  
  # Convert Python output to a data frame
  results <- do.call(rbind, lapply(predictions, function(x) {
    data.frame(label = x$label, score = x$score, stringsAsFactors = FALSE)
  }))
  
  # Attach to original dataset
  df <- cbind(df, results)
  
  return(df)
}
