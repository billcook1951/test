# test version SRM family assessment app for positivity
#

function(input, output, session) {

  steps <- reactiveVal(steps_template)
  current_page <- reactive({
    steps()[page_num()]
  })

  # Page navigation logic --------------------
  page_num <- reactiveVal(1)

  # Disable next/prev buttons when appropriate
  observe({
    toggleState(id = "prevBtn", condition = page_num() > 1 && can_backtrack())
    toggleState(id = "nextBtn", condition = page_num() < PAGES_NUM && section_filled_out())
  })

  can_backtrack <- reactive({
    steps()[page_num() - 1] != "page_choose_next_participant"
  })

  # Show the current page when the page number changes
  observe({
    hide(selector = ".page")

    show(current_page())

    # Set the focus on the first input in each page, makes survey faster to use
    shinyjs::runjs('$("#main_area").find("input:visible:first").focus()')
  })

  # Navigate to previous/next page
  observeEvent(input$prevBtn, {
    navPage(-1)
  })
  observeEvent(input$nextBtn, {
    req(section_filled_out(), page_num() < PAGES_NUM)
    navPage(1)
  })
  navPage <- function(direction) {
    page_num(page_num() + direction)
  }

  # Keep track of all inputs on a question page
  page_questions <- reactive({
    if (page_num() >= PAGE_SURVEY_START && page_num() <= (length(survey_start) + PAGE_SURVEY_START - 1)) {
      names(survey_start[[page_num() - PAGE_SURVEY_START + 1]])
    } else {
      NULL
    }
  })

  # find the input values submitted by user
  page_question_values <- reactive({
    lapply(page_questions(), function(x) input[[x]])
  })

  # check that all values are non empty / truthy
   section_filled_out <- reactive({
     if (page_num() == DISCLAIMER_PAGE) {
       input$legal1 && input$legal2 && input$legal3
     } else {
       page_question_values() %>%
         lapply(isTruthy) %>%
         unlist() %>%
         all()
     }
   })

  past_participants <- reactiveVal(NULL)

  current_participant <- reactive({
    tail(past_participants(), 1)
  })

  observeEvent(input$nextBtn, priority = 1, {
    if (current_page() == "page_choose_next_participant") {
      past_participants(c(past_participants(), input$next_participant))
      relationship_pages <-
        paste(
          "page",
          rep(
            relationships %>%
              dplyr::filter(person_2 == current_participant()) %>%
              dplyr::pull(input_id),
            each = NUM_QUESTIONS_PER_RELATIONSHIP),
          LETTERS[1:NUM_QUESTIONS_PER_RELATIONSHIP],
          sep = "_"
        )
      page_num <- which(steps() == "page_relationship_XX")[1]
      steps_temp <- steps()
      steps_temp[seq(page_num, page_num + (NUM_QUESTIONS_PER_PERSON - 1))] <- relationship_pages
      steps(steps_temp)
    }
  })

  # The elements of setdiff(x,y) are those elements in x but not in y.
  # obtain respondents in the order family members select
  
  output$next_participant_ui <- renderUI({
    remaining_participants <- setdiff(ID_ALL_MEMBERS, past_participants())
    req(remaining_participants)
    participants_list <- setNames(remaining_participants, get_name(remaining_participants))
    radioButtons("next_participant", NULL, choices = participants_list)
  })

  # is the question a scale 1 - 7 question?
  is_relationship_question <- reactive({
    grepl("relationship", current_page(), fixed = TRUE)
  })

  # Show the "1=NEVER, 7=ALWAYS" instructions when appropriate
  observeEvent(is_relationship_question(), {
    toggle("scale_instructions", cond = is_relationship_question())
  })

  # Close the app when the user presses on the Close button on the last page
  observeEvent(input$exit_btn, {
    shinyjs::runjs("window.close();")
  })

  # consider # this out
  output$page_num_out <- renderText({
    paste(page_num(), "/", length(steps()))
  })

  # relationship survey question generation -----------------------

  # A dataframe keeping track of key demographic information of the family members
  fam_members_tbl <- reactive({
    tribble(
      ~person_id,     ~person,           ~name,              ~age,              ~gender,
      ID_MOTHER,      "mother",          input$name_you,     input$age_you,     "Female",
      ID_FATHER,      "father",          input$name_other,   input$age_other,   "Male",
 #     ID_CHILD_OLD,   "older sibling",   input$name_child_1, input$age_child_1, input$gender_child_1,
      ID_CHILD_YOUNG, "younger sibling", input$name_child_2, input$age_child_2, input$gender_child_2
    )
  })

  get_name <- function(person_id_) {
    fam_tbl <- fam_members_tbl()

    fam_tbl %>%
      dplyr::filter(person_id %in% person_id_) %>%
      dplyr::pull(name)
  }

  # Create outputs -----------

  # Fill in the name of the partner in the questions instructions
  lapply(seq(nrow(relationships)), function(i) {
    output[[paste0("likert_instruction_", i, "_1_A")]] <-
      output[[paste0("likert_labels_", i, "_1_A")]] <-
      output[[paste0("likert_labels_", i, "_1_B")]] <-
      output[[paste0("likert_labels_", i, "_1_C")]] <-
      output[[paste0("likert_labels_", i, "_1_D")]] <-
      renderText({
        get_name(relationships$person_1[i])
      })
    })
##############################################################
  

  
  # Fill in the name of the respondent in the questions instructions
  lapply(seq(nrow(relationships)), function(i) {
    output[[paste0("likert_instruction_", i, "_2_A")]] <-
        renderText({
        get_name(relationships$person_2[i])
      })
  })
  
##############################################################
  
  # age z score warnings -----------------
  age_z_scores <- reactive({
    req(fam_members_tbl())
    calc_age_z_score(fam_members_tbl())
  })

  age_z_score_warnings <- reactive({
    out <- age_z_scores() %>%
      mutate(z_score_warning = case_when(
        z_score > z_age_threshold ~ paste0(
          name,
          " is significantly older than the average ",
          person,
          " in the benchmark sample.  The results may not be valid."),
        z_score < -z_age_threshold ~ paste0(
          name,
          " is significantly younger than the average ",
          person,
          " in the benchmark sample.  The results may not be valid."
        ),
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(z_score_warning)) %>%
      pull(z_score_warning)
  })

  output$age_z_score_warnings_out <- renderUI({
    out <-
      lapply(age_z_score_warnings(), function(warning) {
        div(
          class = "alert alert-warning",
          warning
        )
      })

    column(
      width = 12,
      out
    )
  })

  # relationship z score warnings --------------------
  relationship_x <- reactive({
    lapply(relationship_input_ids, function(id) { input[[id]]} )
  })

  relationship_x_tbl <- reactive({
    req(relationship_x() %>% lapply(isTruthy) %>% unlist() %>% all())
    hold_x_input <- relationship_x() %>%
      unlist() %>%
      as.numeric()

    # calculate x value as average of every X likert questions
    x_out <- colMeans(matrix(hold_x_input, nrow = NUM_QUESTIONS_PER_RELATIONSHIP))

    # add user relationship positivity inputs to `relationships` table
    relationships$x <- x_out

    # create relationships_out as relationship-level table and merge with relationships
    # which has the x score and both have "relationship" variable to merge by
    relationships_out <- relationship_fits %>%
      mutate(relationship = build_relationship(person_1, person_2)) %>%
      left_join(relationships[, c("relationship", "x")], by = "relationship")
    relationships_out
  })

  # use regression coefficients to adjust scores in fam_members_tbl
  adjustment_values <- reactive({
    calculate_adjustment_values(fam_members_tbl())
  })

  relationship_z_scores <- reactive({
    rel_tbl <- relationship_x_tbl()

    adjustments <- adjustment_values()

    rel_tbl$adjustment_calced <- calc_adjustment(rel_tbl$adjustments, adjustments, by = "base")

    scores <- calc_z_score(rel_tbl)

    scores <- scores %>% mutate(
      score_word = case_when(
        z_score > z_positivity_threshold  ~ "High",
        z_score < -z_positivity_threshold ~ "Low",
        TRUE                              ~ "Average"
      )
    )
    scores
  })

  relationship_z_scores_text <- reactive({
    hold_fam_members_tbl <- fam_members_tbl()

    # join family member names to relationship z score table
    hold <- relationship_z_scores() %>%
      left_join(hold_fam_members_tbl, by = c("person_1" = "person_id")) %>%
      rename(name_1 = name) %>%
      left_join(hold_fam_members_tbl, by = c("person_2" = "person_id")) %>%
      rename(name_2 = name) %>%
      mutate(name_1 = paste0(name_1, "'s"))

    # create the text
    hold %>%
      mutate(z_score_text = paste0("The positivity that ",name_1, " experiences from ", name_2, " is ",
                                   "<strong>", score_word, "</strong>", ", (Z = ", z_score,").")
      )
  })

  html_positivity_results <- reactive({
    out_text <- data_frame(
      relationship = c(
        build_relationship(ID_MOTHER, ID_FATHER),
        build_relationship(ID_FATHER, ID_MOTHER),
#        build_relationship(ID_MOTHER, ID_CHILD_OLD),
#        build_relationship(ID_CHILD_OLD, ID_MOTHER),
        build_relationship(ID_MOTHER, ID_CHILD_YOUNG),
        build_relationship(ID_CHILD_YOUNG, ID_MOTHER),
#        build_relationship(ID_FATHER, ID_CHILD_OLD),
#        build_relationship(ID_CHILD_OLD, ID_FATHER),
        build_relationship(ID_FATHER, ID_CHILD_YOUNG),
        build_relationship(ID_CHILD_YOUNG, ID_FATHER)
#        build_relationship(ID_CHILD_OLD, ID_CHILD_YOUNG),
#        build_relationship(ID_CHILD_YOUNG, ID_CHILD_OLD)
      )
    )

    out_text <-
      left_join(out_text, relationship_z_scores_text(), by = "relationship") %>%
      pull(z_score_text)

    out <- tagList()  # drop which ones? try dropping 7, 9, 11
    for (i in c(1, 3, 5)) {
      out[[i]] <- div(
        HTML(out_text[i]), br(),
        HTML(out_text[i + 1]), br(), br()
      )
    }
    out
  })

  output$relationship_z_scores_tbl <- renderUI({
    tagList(
      html_positivity_results(),
      br()
    )
  })

  # SRM results ---------------------------
  srm <- reactive({
    rel_tbl <- relationship_x_tbl()
    fam_tbl <- fam_members_tbl()

    # `calc_act()` and `calc_part()` defined in "scores.R"
    fam_tbl$act <- lapply(fam_tbl$person_id, function(p) calc_act(p, rel_tbl)) %>% unlist()
    fam_tbl$part <- lapply(fam_tbl$person_id, function(p) calc_part(p, rel_tbl)) %>% unlist()

    fam <- sum(rel_tbl$x) / n*(n-1)

    # relationship effects
    rel_tbl <- rel_tbl %>%
      left_join(fam_tbl[, c("person_id", "act")], by = c("person_1" = "person_id")) %>%
      left_join(fam_tbl[, c("person_id", "part")], by = c("person_2" = "person_id")) %>%
      mutate(rel = x - act - part - fam) %>%  # x is the mean of the observed score
      select(person_1, person_2, x, rel)      #  rel is the relationship effect

    list(
      fam = fam,
      act_part = fam_tbl,
      rel_effects = rel_tbl
    )
  })

  srm_z_scores <- reactive({
    fam <- srm()$fam
    act_part <- srm()$act_part

    rel_effects <- srm()$rel_effects

    adjustment_values <- adjustment_values()

    ### calculate z scores for "act"
    # `act_fits` is defined in 'scores.R'
    act_z <- left_join(act_part, act_fits, by = "person_id")

    # `calc_adjustment()` is defined in 'scores.R'
    act_z$adjustment_calced <- calc_adjustment(act_z$adjustments, adjustment_values, by = "base")

    act_z <- act_z %>%
      rename(x = act) %>%
      calc_z_score()

    ### calculate z scores for part
    # `part_fits` is defined in 'scores.R'
    part_z <- left_join(act_part, part_fits, by = "person_id")

    part_z$adjustment_calced <- calc_adjustment(part_z$adjustments, adjustment_values, by = "base")

    part_z <- part_z %>%
      rename(x = part) %>%
      calc_z_score()

    ### calculate z scores for relationship effect
    # `rel_fits` is defined in 'scores.R'
    rel_effects_z <- left_join(rel_effects, rel_fits, by = c("person_1", "person_2"))

    rel_effects_z$adjustment_calced <- calc_adjustment(rel_effects_z$adjustments, adjustment_values, by = "base")

    rel_effects_z <- rel_effects_z %>%
      rename(
        rel_input = x,
        x = rel) %>%
      calc_z_score()

    list(
      "fam_z" = data_frame(
        "person_id" = ID_FAMILY,
        "fam" = fam,
        "z_score" =
          (fam  - act_fits[act_fits$person_id == ID_FAMILY, ]$coef) /
          act_fits[act_fits$person_id == ID_FAMILY, ]$se
      ),
      "act_z" = act_z,
      "part_z" = part_z,
      "rel_effects_z" = rel_effects_z
    )
  })

  # SRM report output
  srm_score_text <- reactive({
    hold <- srm_z_scores()

    lapply(hold, function(df) {
      df %>%
        mutate(
          text_highlow = case_when(
            z_score > z_srm_threshold ~ "High",
            z_score < -z_srm_threshold ~ "Low",
            TRUE ~ "Average"
          ),
          text_abovebelow = case_when(
            z_score > z_srm_threshold ~ "above average",
            z_score < -z_srm_threshold ~ "below average",
            TRUE ~ "average"
          )
        )
    })
  })

  html_srm_info <- sapply(ID_ALL_MEMBERS, function(person) {
    reactive({
      req(srm_score_text(), fam_members_tbl())
      srm_html_output(srm_score_text(), fam_members_tbl(), person)
    })
  })

  lapply(ID_ALL_MEMBERS, function(person) {
    output_name <- paste0("srm_out_", person)
    output[[output_name]] <- renderUI({
      html_srm_info[[person]]()
    })
  })

  # Generate the Narrative Explanation report pages
  html_narrative_info <- sapply(ID_ALL_MEMBERS, function(person) {
    reactive({
      req(srm_score_text(), relationship_z_scores(), fam_members_tbl())
      narrative_html_output(srm_score_text(), relationship_z_scores(), fam_members_tbl(), person)
    })
  })
  lapply(ID_ALL_MEMBERS, function(person) {
    output_name <- paste0("narrative_out_", person)
    output[[output_name]] <- renderUI({
      html_narrative_info[[person]]()
    })
  })

  html_all_pages <- reactive({
    as.character(tagList(
      h1("Round-Robin SRM Family Assessment"),
      h2("Positivity Results - Simple Scale Scores"),
      HTML(paste0(
        "Relationships are two-sided: A mother's experience of positivity from her child is not
      the same as a child's experience of positivity from the mother. Based on comparisons with the Benchmark
      families, the results below evaluate for each
      pair of family members how much positivity each person experiences receiving from the other. A Z-score 
      above 1.0 is considered a higher than average score and a Z-score below -1.0 is
        considerd a lower than average score. The scale scores (4-item averages) on which the Z-scores are based 
        are the input data for the Social Relations Model analysis."
      )),
      br(),
      html_positivity_results(),
      br(), br(),

      lapply(ID_ALL_MEMBERS, function(person) {
        tagList(
          html_srm_info[[person]](),
          br(), br(), br(),br(),br()
        )
      }),
      lapply(ID_ALL_MEMBERS, function(person) {
        tagList(
          html_narrative_info[[person]](),
          br(), br(), br(),br(),br(),br(),br(),br(),br(),br(),br()
        )
      })
    ))
  })

  output$download_report_ui <- renderUI({
    show_report <- FALSE
    if (file.exists("reportpassword.txt")) {
      req(input$download_pw_submit)
      password <- trimws(readLines("reportpassword.txt"))
      if (isolate(input$download_password) == password) {
        show_report <- TRUE
        downloadButton("download_report", "Download Report", class = "btn-lg btn-success")
      }
    } else {
      show_report <- TRUE
    }

    if (show_report) {
      shinyjs::hide("password_section")
      downloadButton("download_report", "Download Report", class = "btn-lg btn-success")
    } else {
      shinyjs::alert("Incorrect password")
    }
  })
  

  output$download_report <- downloadHandler(
    filename = "ycf.pdf",
    content = function(file) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Generating report...", value = 0.3)
      html <- html_all_pages()
      html_file <- tempfile(fileext = ".html")
      writeLines(html, html_file)
      progress$set(message = "Downloading report...", value = 0.6, detail = "This can take a minute")
      rmarkdown::pandoc_convert(html_file, output = file)
    }
  )
  
  
   if (DEV_MODE) {
    # For quicker testing, pre-accept the Terms and add family members names
    updateCheckboxInput(session, "legal1", value = TRUE)
    updateCheckboxInput(session, "legal2", value = TRUE)
    updateCheckboxInput(session, "legal3", value = TRUE)
    updateTextInput(session, "name_you", value = "mom")
    updateTextInput(session, "name_other", value = "dad")
 #   updateTextInput(session, "name_child_1", value = "old")
    updateTextInput(session, "name_child_2", value = "young")
    
    # Show results in realtime
    output$dev_table_fam_members_tbl <- DT::renderDT({
      fam_members_tbl()
    },
    options = list(scrollX = TRUE, pageLength = 100, dom = "t"),
    rownames = NULL
    )
    output$dev_download_fam_members_tbl <- downloadHandler(
      filename = "fam_members_tbl.csv",
      content = function(file) {
        write.csv(fam_members_tbl(), file)
      }
    )
    output$dev_table_relationship_x <- DT::renderDT({
      data.frame(relationship = relationship_input_ids, score = unlist(relationship_x()))
    },
    options = list(scrollX = TRUE, pageLength = 100, dom = "t"),
    rownames = NULL
    )
    output$dev_download_relationship_x <- downloadHandler(
      filename = "relationship_x.csv",
      content = function(file) {
        write.csv(data.frame(relationship = relationship_input_ids, score = unlist(relationship_x())), file)
      }
    )
#    output$dev_table_relationship_z_scores <- DT::renderDT({
#      data <- relationship_z_scores()
#      data$adjustments <- unlist(lapply(data$adjustments, function(adj) {
#        if (length(adj) == 1 && is.na(adj)) {
#          
#          "" 
#        } else {
#          
#          paste(paste(adj$base, ":", adj$adjustment), collapse = ";")
#        }
#      }))
#      data
#    },
#    options = list(scrollX = TRUE, pageLength = 100, dom = "t"),
#    rownames = NULL
 #   )
  #  output$dev_download_relationship_z_scores <- downloadHandler(
  #    filename = "relationship_z_scores.csv",
  #    content = function(file) {
  #      data <- relationship_z_scores()
  #      data$adjustments <- unlist(lapply(data$adjustments, function(adj) {
  #        if (length(adj) == 1 && is.na(adj)) {
  #          
  #          ""
  #        } else {
  #          paste(paste(adj$base, ":", adj$adjustment), collapse = ";")
  #         }
  #      }))
  #     write.csv(data, file)
  #    }
  #  )
  }
}
