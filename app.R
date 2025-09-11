
library("shiny")
library("DT")
library("dplyr")
library("shinyvalidate")
library("stringi")
library("purrr")
library("readr")

READ_ONLY <- TRUE
CSV_URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vS9p0U5NO8D-wI5Z1dju203NIqzJ3abB4UzR7z9c3velDakZKfghYpAvD0EL-SvBujSjDb4BXNtlvPT/pub?output=csv"

#needs a wrapper for the download button
downloadButton2 <- function(outputId, ...) {
  tag <- shiny::downloadButton(outputId, ...)
  tag$attribs$download <- NULL
  tag
}

BANK_FILE <- "question_bank.rds"

default_bank <- tibble::tibble(
  id        = c(1L, 2L, 3L),
  theme     = c("Math", "Science", "History"),
  writer = c("Ross", "Ross", "Ross"),
  question  = c(
    "What is 12 × 8?",
    "Which gas do plants primarily absorb for photosynthesis?",
    "In what year did the Berlin Wall fall?"
  ),
  choice1   = c("80", "Oxygen", "1961"),
  choice2   = c("96", "Carbon dioxide", "1989"),
  choice3   = c("88", "Nitrogen", "1991"),
  choice4   = c("108", "Hydrogen", "1979"),
  correct   = c(2L, 2L, 2L),  # index of the correct choice (1–4)
  explanation = c(
    "12 × 8 = 96.",
    "Plants absorb CO₂ during photosynthesis.",
    "The Berlin Wall fell in 1989."
  )
)

read_bank <- function() {
  df <- tryCatch(
    {
      tmp <- utils::read.csv(CSV_URL, na.strings = c("", "NA"))
      tibble::as_tibble(tmp) %>%
        dplyr::transmute(
          id = as.integer(id),
          theme = as.character(theme),
          writer = as.character(writer),
          question = as.character(question),
          choice1 = as.character(choice1),
          choice2 = as.character(choice2),
          choice3 = as.character(choice3),
          choice4 = as.character(choice4),
          correct = as.integer(correct),
          explanation = as.character(explanation)
        )
    },
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) default_bank else df
}

write_bank <- function(df) {
  # No-op in shinylive read-only mode
  invisible(NULL)
}


ui <- fluidPage(
  tags$head(tags$style(HTML("
    .card { border:1px solid #e5e7eb; border-radius:16px; padding:16px; box-shadow:0 1px 4px rgba(0,0,0,0.05); }
    .soft { background:#fafafa; border-radius:12px; padding:12px; }
    .pill { border-radius:999px; padding:4px 10px; background:#eef2ff; display:inline-block; font-size:12px; }
    .muted { color:#6b7280; }
    .answer-correct { background:#ecfdf5; border-left:4px solid #10b981; padding:10px; border-radius:8px; }
    .answer-wrong { background:#fef2f2; border-left:4px solid #ef4444; padding:10px; border-radius:8px; }
  "))),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('saveBank', function(x){
      try { localStorage.setItem('mcq_bank', JSON.stringify(x)); } catch(e) { console.warn('saveBank', e); }
    });
    document.addEventListener('DOMContentLoaded', function(){
      try {
        const raw = localStorage.getItem('mcq_bank');
        if (raw) Shiny.setInputValue('bank_from_storage', JSON.parse(raw), {priority: 'event'});
      } catch(e) { console.warn('loadBank', e); }
    });
  ")),
  titlePanel("Themed MCQ Bank & Quiz"),
  tabsetPanel(id = "tabs",
              tabPanel("Manage Bank",
                       br(),
                       fluidRow(
                         column(
                           width = 5,
                           div(class = "card",
                               h4("Add / Edit Question"),
                               div(class = "soft",
                                   uiOutput("theme_input"),
                                   uiOutput("writer_input"),
                                   textAreaInput("q_text", "Question", "", rows = 3, placeholder = "Enter the question text..."),
                                   fluidRow(
                                     column(6, textInput("c1", "Choice 1")),
                                     column(6, textInput("c2", "Choice 2"))
                                   ),
                                   fluidRow(
                                     column(6, textInput("c3", "Choice 3")),
                                     column(6, textInput("c4", "Choice 4"))
                                   ),
                                   radioButtons("correct_idx", "Correct choice", choices = setNames(1:4, paste("Choice", 1:4)), inline = TRUE),
                                   textAreaInput("explain", "Explanation (optional)", "", rows = 2,
                                                 placeholder = "Why is this correct? Helpful for review."),
                                   actionButton("add_btn", "Save Question", class = "btn btn-primary"),
                                   actionButton("reset_btn", "Reset Form", class = "btn btn-default"),
                                   tags$small(class="muted", "Tip: Use the table on the right to select a row for quick editing.")
                               )
                           ),
                           br(),
                           div(class = "card",
                               h4("Import / Export"),
                               fileInput("import_csv", "Import CSV", accept = c(".csv")),
                               downloadButton2("export_csv", "Export as CSV"),
                               tags$p(class="muted", "CSV columns: theme,question,choice1,choice2,choice3,choice4,correct,explanation (correct = 1–4).")
                           )
                         ),
                         column(
                           width = 7,
                           div(class = "card",
                               h4("Question Bank"),
                               fluidRow(
                                 column(6, selectInput("filter_theme", "Filter by theme", choices = c("All"), selected = "All")),
                                 column(6, actionButton("delete_btn", "Delete Selected", class = "btn btn-danger", width = "100%"))
                               ),
                               DTOutput("bank_table")
                           )
                         )
                       )
              ),
              tabPanel("Quiz",
                       br(),
                       div(class = "card",
                           h4("Setup"),
                           fluidRow(
                             column(5, uiOutput("quiz_theme_picker")),
                             column(3, numericInput("n_questions", "Number of questions", value = 5, min = 1, step = 1)),
                             column(4, actionButton("start_quiz", "Start Quiz", class = "btn btn-success", width = "100%"))
                           ),
                           tags$small(class="muted", "Questions are sampled at random from the selected themes.")
                       ),
                       br(),
                       uiOutput("quiz_ui"),
                       uiOutput("quiz_review")
              )
  )
)



server <- shinyServer(function(input, output, session) {
  
  # State: question bank
  bank <- reactiveVal(read_bank())
  
  observeEvent(input$bank_from_storage, {
    df <- tryCatch(tibble::as_tibble(input$bank_from_storage), error = function(e) NULL)
    if (!is.null(df) && nrow(df) > 0) {
      df <- df %>% mutate(id = as.integer(id), correct = as.integer(correct))
      bank(df)
    }
  }, once = TRUE)
  
  #observe bank changes
  observeEvent(bank(), {
    # send as plain data.frame (lists JSON-ify nicely)
    session$sendCustomMessage("saveBank", bank() %>% as.data.frame(stringsAsFactors = FALSE))
  }, ignoreInit = FALSE)
  
  observe({
    # Keep filter list and quiz theme list in sync
    themes <- sort(unique(bank()$theme))
    updateSelectInput(session, "filter_theme", choices = c("All", themes))
  })
  
  # THEME input (select existing or add new)
  output$theme_input <- renderUI({
    themes <- sort(unique(bank()$theme))
    tagList(
      selectizeInput("theme_sel", "Theme", choices = themes, multiple = FALSE,
                     options = list(placeholder = "Choose an existing theme…")),
      textInput("theme_new", NULL, placeholder = "…or type a new theme name")
    )
  })
  
  # WRITER Input
  
  output$writer_input <- renderUI({
    writers <- sort(unique(bank()$writer))
    tagList(
      selectizeInput("writer_sel", "Writer", choices = writers, multiple = FALSE,
                     options = list(placeholder = "Choose an existing writer…")),
      textInput("writer_new", NULL, placeholder = "…or type a new writer")
    )
  })
  
  # Validation
  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule("q_text", sv_required(message = "Question is required."))
  purrr::walk(c("c1","c2","c3","c4"), ~iv$add_rule(., sv_required("All four choices are required.")))
  iv$add_rule("correct_idx", sv_required("Pick which choice is correct."))
  observeEvent(input$add_btn, ignoreInit = TRUE, {
    if (!iv$is_valid()) {
      iv$enable()
      showNotification("Please fix validation errors.", type = "error")
      return()
    }
    theme_final <- if (nzchar(input$theme_new)) input$theme_new else input$theme_sel
    if (!nzchar(theme_final)) {
      showNotification("Select or enter a theme.", type = "error"); return()
    }
    writer_final <- if (nzchar(input$writer_new)) input$writer_new else input$writer_sel
    if (!nzchar(writer_final)) {
      showNotification("Select or enter a writer.", type = "error"); return()
    }
    # Build row
    new_row <- tibble::tibble(
      id = ifelse(nrow(bank()) == 0, 1L, max(bank()$id) + 1L),
      theme = theme_final,
      writer = writer_final,
      question = input$q_text,
      choice1 = input$c1,
      choice2 = input$c2,
      choice3 = input$c3,
      choice4 = input$c4,
      correct = as.integer(input$correct_idx),
      explanation = input$explain
    )
    # If editing (row selected), replace; else append
    proxy <- input$bank_table_rows_selected
    if (length(proxy) == 1) {
      # Map visible row index to actual row in filtered data, then to id
      df_filtered <- bank_filtered()
      target_id <- df_filtered$id[proxy]
      updated <- bank() %>% dplyr::mutate(
        theme = ifelse(id == target_id, theme_final, theme),
        writer = ifelse(id == target_id, writer_final, writer),
        question = ifelse(id == target_id, input$q_text, question),
        choice1 = ifelse(id == target_id, input$c1, choice1),
        choice2 = ifelse(id == target_id, input$c2, choice2),
        choice3 = ifelse(id == target_id, input$c3, choice3),
        choice4 = ifelse(id == target_id, input$c4, choice4),
        correct = ifelse(id == target_id, as.integer(input$correct_idx), correct),
        explanation = ifelse(id == target_id, input$explain, explanation)
      )
      bank(updated); write_bank(updated)
      showNotification("Question updated.", type = "message")
    } else {
      updated <- bind_rows(bank(), new_row)
      bank(updated); write_bank(updated)
      showNotification("Question added.", type = "message")
    }
    resetForm()
  })
  
  # Reset form
  resetForm <- function() {
    updateSelectInput(session, "theme_sel", selected = character(0))
    updateTextInput(session, "theme_new", value = "")
    updateSelectInput(session, "writer_sel", selected = character(0))
    updateTextInput(session, "writer_new", value = "")
    updateTextAreaInput(session, "q_text", value = "")
    updateTextInput(session, "c1", value = "")
    updateTextInput(session, "c2", value = "")
    updateTextInput(session, "c3", value = "")
    updateTextInput(session, "c4", value = "")
    updateRadioButtons(session, "correct_idx", selected = character(0))
    updateTextAreaInput(session, "explain", value = "")
    dataTableProxy("bank_table") %>% selectRows(NULL)
  }
  observeEvent(input$reset_btn, resetForm())
  
  # Filtered view of bank
  bank_filtered <- reactive({
    req(bank())
    if (!isTruthy(input$filter_theme) || input$filter_theme == "All") return(bank())
    bank() %>% filter(theme == input$filter_theme)
  })
  
  # Bank table
  output$bank_table <- renderDT({
    df <- bank_filtered() %>%
      transmute(
        id, Theme = theme, Writer = writer, Question = question,
        `Choice 1` = choice1, `Choice 2` = choice2, `Choice 3` = choice3, `Choice 4` = choice4,
        `Correct (1–4)` = correct, Explanation = explanation
      )
    datatable(df, selection = "single", rownames = FALSE, escape = TRUE,
              options = list(pageLength = 8, scrollX = TRUE))
  })
  
  # Populate form when selecting a row
  observeEvent(input$bank_table_rows_selected, {
    sel <- input$bank_table_rows_selected
    if (length(sel) != 1) return()
    df <- bank_filtered()[sel, ]
    updateSelectInput(session, "theme_sel", selected = if (df$theme %in% unique(bank()$theme)) df$theme else character(0))
    updateTextInput(session, "theme_new", value = ifelse(df$theme %in% unique(bank()$theme), "", df$theme))
    updateSelectInput(session, "writer_sel", selected = if (df$writer %in% unique(bank()$writer)) df$writer else character(0))
    updateTextInput(session, "writer_new", value = ifelse(df$writer %in% unique(bank()$writer), "", df$writer))
    updateTextAreaInput(session, "q_text", value = df$question)
    updateTextInput(session, "c1", value = df$choice1)
    updateTextInput(session, "c2", value = df$choice2)
    updateTextInput(session, "c3", value = df$choice3)
    updateTextInput(session, "c4", value = df$choice4)
    updateRadioButtons(session, "correct_idx", selected = as.character(df$correct))
    updateTextAreaInput(session, "explain", value = df$explanation)
  })
  
  # Delete
  observeEvent(input$delete_btn, {
    sel <- input$bank_table_rows_selected
    if (length(sel) != 1) {
      showNotification("Select exactly one row to delete.", type = "warning")
      return()
    }
    df_f <- bank_filtered()
    rem_id <- df_f$id[sel]
    updated <- bank() %>% filter(id != rem_id)
    bank(updated); write_bank(updated)
    dataTableProxy("bank_table") %>% selectRows(NULL)
    showNotification("Deleted.", type = "message")
  })
  
  # Import
  observeEvent(input$import_csv, {
    f <- input$import_csv$datapath
    if (!file.exists(f)) return()
    df <- tryCatch(
      {
        read.csv(f, stringsAsFactors = FALSE, na.strings = c("", "NA")) %>%
          tibble::as_tibble() %>%
          transmute(
            theme = as.character(theme),
            writer = as.character(writer),
            question = as.character(question),
            choice1 = as.character(choice1),
            choice2 = as.character(choice2),
            choice3 = as.character(choice3),
            choice4 = as.character(choice4),
            correct = as.integer(correct),
            explanation = as.character(explanation)
          )
      },
      error = function(e) NULL
    )
    if (is.null(df) || any(!c("theme","writer","question","choice1","choice2","choice3","choice4","correct","explanation") %in% names(df))) {
      showNotification("Import failed: CSV missing required columns.", type = "error"); return()
    }
    if (any(is.na(df$correct)) || any(df$correct < 1 | df$correct > 4)) {
      showNotification("Import failed: 'correct' must be integers 1–4.", type = "error"); return()
    }
    # Assign new IDs continuing from max
    start_id <- ifelse(nrow(bank()) == 0, 1L, max(bank()$id) + 1L)
    df$id <- seq.int(from = start_id, length.out = nrow(df))
    updated <- bind_rows(bank(), df %>% select(id, theme, writer, question, choice1, choice2, choice3, choice4, correct, explanation))
    bank(updated); write_bank(updated)
    showNotification(sprintf("Imported %d questions.", nrow(df)), type = "message")
  })
  
  # Export
  output$export_csv <- downloadHandler(
    filename = function() sprintf("question_bank_%s.csv", format(Sys.Date(), "%Y%m%d")),
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      df <- bank() %>% dplyr::select(id, theme, writer, question, choice1, choice2, choice3, choice4, correct, explanation)
      readr::write_excel_csv(df, file)  # UTF-8 with BOM (Excel-friendly)
    }
  )
  
  # ---- QUIZ ----
  output$quiz_theme_picker <- renderUI({
    themes <- sort(unique(bank()$theme))
    selectizeInput("quiz_themes", "Themes to include", choices = themes, multiple = TRUE,
                   options = list(placeholder = "Choose one or more themes…"))
  })
  
  # Quiz state
  quiz <- reactiveValues(
    running = FALSE,
    deck = NULL,    # data.frame of sampled questions
    idx = 1L,       # current question index
    answers = integer(0),
    submitted = FALSE
  )
  
  observeEvent(input$start_quiz, {
    req(input$quiz_themes)
    pool <- bank() %>% filter(theme %in% input$quiz_themes)
    if (nrow(pool) == 0) {
      showNotification("No questions available for the selected themes.", type = "warning"); return()
    }
    n <- min(input$n_questions %||% 5, nrow(pool))
    # Sample without replacement, shuffle rows
    set.seed(as.integer(substr(stri_rand_strings(1, 8, '[0-9]'), 1, 8)))
    deck <- pool %>% slice_sample(n = n)
    quiz$deck <- deck
    quiz$idx <- 1L
    quiz$answers <- rep(NA_integer_, n)
    quiz$running <- TRUE
    quiz$submitted <- FALSE
    updateTabsetPanel(session, "tabs", selected = "Quiz")
  })
  
  # Render quiz UI (question-by-question)
  output$quiz_ui <- renderUI({
    req(quiz$running, !quiz$submitted, !is.null(quiz$deck))
    i <- quiz$idx
    q <- quiz$deck[i, ]
    total <- nrow(quiz$deck)
    choices <- setNames(1:4, c(q$choice1, q$choice2, q$choice3, q$choice4))
    tagList(
      div(class = "card",
          span(class="pill", sprintf("Question %d of %d", i, total)),
          h4(q$question),
          radioButtons("quiz_choice", NULL, choices = choices, selected = quiz$answers[i], inline = FALSE),
          fluidRow(
            column(4, if (i > 1) actionButton("prev_q", "← Previous", class = "btn btn-default", width = "100%")),
            column(4, actionButton("save_q", "Save Answer", class = "btn btn-primary", width = "100%")),
            column(4, if (i < total) actionButton("next_q", "Next →", class = "btn btn-default", width = "100%"))
          ),
          br(),
          if (i == total) actionButton("submit_quiz", "Submit Quiz", class = "btn btn-success")
      )
    )
  })
  
  observeEvent(input$save_q, {
    if (isTruthy(input$quiz_choice)) {
      quiz$answers[quiz$idx] <- as.integer(input$quiz_choice)
      showNotification("Answer saved.", type = "message", duration = 1)
    } else {
      showNotification("Pick an option first.", type = "warning", duration = 1.5)
    }
  })
  observeEvent(input$next_q,  { quiz$idx <- min(quiz$idx + 1L, length(quiz$answers)) })
  observeEvent(input$prev_q,  { quiz$idx <- max(quiz$idx - 1L, 1L) })
  
  observeEvent(input$submit_quiz, {
    if (any(is.na(quiz$answers))) {
      showNotification("You have unanswered questions.", type = "warning"); return()
    }
    quiz$submitted <- TRUE
  })
  
  # Review & scoring
  output$quiz_review <- renderUI({
    req(quiz$running, quiz$submitted, !is.null(quiz$deck))
    df <- quiz$deck
    ans <- quiz$answers
    correct_flags <- ans == df$correct
    score <- sum(correct_flags)
    total <- length(ans)
    
    review_cards <- lapply(seq_len(total), function(i) {
      q <- df[i,]
      your <- ans[i]
      is_right <- your == q$correct
      div(class = if (is_right) "answer-correct" else "answer-wrong",
          strong(sprintf("Q%d: %s", i, q$question)),
          tags$ul(
            lapply(1:4, function(k) {
              lab <- q[[paste0("choice", k)]]
              prefix <- if (k == q$correct) "✓ " else if (k == your) "✗ " else "• "
              style <- if (k == q$correct) "font-weight:bold;" else if (k == your) "text-decoration:underline;" else ""
              tags$li(tags$span(style = style, paste0(prefix, lab)))
            })
          ),
          if (nzchar(q$explanation)) div(em("Why: "), q$explanation)
      )
    })
    
    tagList(
      div(class = "card",
          h4("Results"),
          h5(sprintf("Score: %d / %d (%.0f%%)", score, total, 100*score/total)),
          actionButton("retake", "Retake with same settings", class = "btn btn-primary"),
          actionButton("new_quiz", "Start New Quiz", class = "btn btn-default")
      ),
      br(),
      div(class = "card", h4("Review Answers"), review_cards)
    )
  })
  
  observeEvent(input$retake, {
    # Re-sample with same settings
    if (!isTruthy(input$quiz_themes)) return()
    pool <- bank() %>% filter(theme %in% input$quiz_themes)
    n <- min(input$n_questions %||% 5, nrow(pool))
    set.seed(as.integer(substr(stri_rand_strings(1, 8, '[0-9]'), 1, 8)))
    deck <- pool %>% slice_sample(n = n)
    quiz$deck <- deck
    quiz$idx <- 1L
    quiz$answers <- rep(NA_integer_, n)
    quiz$submitted <- FALSE
  })
  
  observeEvent(input$new_quiz, {
    quiz$running <- FALSE
    quiz$submitted <- FALSE
    quiz$deck <- NULL
    quiz$answers <- integer(0)
    quiz$idx <- 1L
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
