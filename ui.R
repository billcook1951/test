# test version  SRM family assessment app for positivity
# 

fluidPage(
  useShinyjs(),
  includeCSS("www/styles.css"),
  tags$script(src = "app.js"),
  headerPanel(
    div(tags$strong("The Round-Robin SRM Family Assessment"),
        class = "text-center"),
    windowTitle = "Your Complicated Family"
  ),
  br(),
  fluidRow( 
    if (DEV_MODE) {
    column(
      3,
      tabsetPanel(
        tabPanel(
          "Demographics", br(),
          downloadButton("dev_download_fam_members_tbl"),
        #  DT::DTOutput("dev_table_fam_members_tbl", width = "100%")
        ),
        tabPanel(
          "Relationships", br(),
          downloadButton("dev_download_relationship_x"),
         # DT::DTOutput("dev_table_relationship_x", width = "100%")
        )
        #tabPanel(
         # "relationship_z_scores", br(),
         # downloadButton("dev_download_relationship_z_scores"),
        #  DT::DTOutput("dev_table_relationship_z_scores", width = "100%")
        #)
      )
    )
  } else {},
  column(
    width = 8,
    offset = if (DEV_MODE) 0 else 2,
    id = "main_area",
    
    
    ######################################
    
      # Intro page ---------------
      div(
        class = "page",
        id = "page_intro",
        h2(strong("Welcome")),
        h3(em("(A parent or guardian will please read the following to the other family members)")),
        br(),
        h3(strong("This survey will help your counselor or therapist understand your family relationships. ")),
        br(),
        tags$p(strong("First, a parent or guardian will enter your family data")),
        tags$p("This includes the names, ages, and gender of each family member"),
        br(),
        tags$p(strong("Then, each of you will take turns answering the questions.")),
                      tags$p("If you make a mistake or want to change one of your answers, you can click the button labelled ",
               tags$strong("Previous"), " to go back to that question. However, you cannot back up to another person's answers."),
        br(), br(),
        tags$p(strong("When it is your turn, please make sure that the box with your name in it has been clicked.  
                      When you have completed your turn, please pass the device (Ipad, tablet, whatever) to the next family member.")),
        tags$p("Your responses will not be shared with other family members unless you give your permission.
           There are no right or wrong answers. It is important for you to be as truthful as you can be."),
        br(), br()
      ),

      # Hide the rest of the pages initially
      tagList(hidden(

        # Disclaimer page ---------------
         div(
           class = "page",
           id = "page_disclaimer",
           h2("About This Assessment"),
           h3(strong("This assessment should only be used as one part of a comprehensive evaluation performed by a licensed counseling professional.")),
           br(),
           h4(em("A parent or guardian should go first")," and should check off each of the following boxes ",
           tags$strong("after getting the agreement")," of each family member."),
           br(),
           checkboxInput("legal1", "Each family member understands that the purpose of this
                         assessment is to give our counselor information about our family relationships that will 
                         help him or her make decisions about how best to help us.", FALSE, width = "100%"),
           checkboxInput("legal2", "Each family member agrees to respect the privacy of others by not asking how they answered
                         the questions, except with the consent and supervision of the counselor.", FALSE, width = "100%"),
           checkboxInput("legal3", "Each family member also agrees to keep their own answers to the questions private,
                         except with the consent and supervision of the counselor.", FALSE, width = "100%"),
           
           br(), br()
          ),

        # Demographic questions pages
        # note survey_start is in helpers/survey-participant-info
        lapply(names(survey_start), function(survey_page_name) {
          survey_page <- survey_start[[survey_page_name]]
          div(
            class = "page page_demographic_q",
            id = paste0("page_", survey_page_name),
            lapply(names(survey_page), function(survey_question_name) {
              survey_question <- survey_page[[survey_question_name]]
              build_survey_input(survey_question_name, survey_question)
            })
          )
        }),

        # Choose next participant page --------------
        div(
          class = "page",
          id = "page_choose_next_participant",
          h2("Choose the next participant"),
          br(),
          uiOutput("next_participant_ui"),
          br(), br()
        ),

        # Likert scale / relationship questions ----------

        # First relationsip question --------
        lapply(1:nrow(relationships), function(i) {
          perspective <- " says or does when together with you"
          compliment <- " shows approval or compliments you"

          div(
            class = "page page_likert_q",
            id = paste0("page_relationship_", relationships$person_1[i], "_", relationships$person_2[i], "_A"),
            tags$strong(textOutput(paste0("likert_instruction_", i, "_2_A"), inline = TRUE)),
            "think about the kinds of things ",
            tags$strong(textOutput(paste0("likert_instruction_", i, "_1_A"), inline = TRUE)),
            perspective,
            br(), br(),
            tags$h4(
              tags$strong(textOutput(paste0("likert_labels_", i, "_1_A"), inline = TRUE)),
              compliment
            ),
            br(),
            br(),
            radioButtons(
              paste0("relationship_", relationships$person_2[i], "_", relationships$person_1[i], "_A"),
              label = NULL,
              choices = 1:7,   # change for 5 point likert scales for effectance and acquiescence
              inline = TRUE,
              selected = 4
            )
          )
        }),

        # Second relationship question ------------
        lapply(1:nrow(relationships), function(i) {
          compliment <- " expresses gratitude towards you"

          div(
            class = "page page_likert_q",
            id = paste0("page_relationship_", relationships$person_1[i], "_", relationships$person_2[i], "_B"),
            br(), br(),
            tags$h4(
              tags$strong(textOutput(paste0("likert_labels_", i, "_1_B"), inline = TRUE)),
              compliment
            ),
            br(),
            br(),
            radioButtons(
              paste0("relationship_", relationships$person_2[i], "_", relationships$person_1[i], "_B"),
              label = NULL,
              choices = 1:7,
              inline = TRUE,
              selected = 4
            )
          )
        }),

        # Third relationship question ------------
        lapply(1:nrow(relationships), function(i) {
          compliment <- " does something nice for you"

          div(
            class = "page page_likert_q",
            id = paste0("page_relationship_", relationships$person_1[i], "_", relationships$person_2[i], "_C"),
            br(), br(),
            tags$h4(
              tags$strong(textOutput(paste0("likert_labels_", i, "_1_C"), inline = TRUE)),
              compliment
            ),
            br(),
            br(),
            radioButtons(
              paste0("relationship_", relationships$person_2[i], "_", relationships$person_1[i], "_C"),
              label = NULL,
              choices = 1:7,
              inline = TRUE,
              selected = 4
            )
          )
        }),


        # Fourth relationship question ------------
        lapply(1:nrow(relationships), function(i) {
          compliment <- " shares feelings or thoughts with you"

          div(
            class = "page page_likert_q",
            id = paste0("page_relationship_", relationships$person_1[i], "_", relationships$person_2[i], "_D"),
            br(), br(),
            tags$h4(
              tags$strong(textOutput(paste0("likert_labels_", i, "_1_D"), inline = TRUE)),
              compliment
              ),
            br(),
            br(),
            radioButtons(
              paste0("relationship_", relationships$person_2[i], "_", relationships$person_1[i], "_D"),
              label = NULL,
              choices = 1:7,
              inline = TRUE,
              selected = 4
            )
          )
        }),

        # Exit page
        div(
          id = "page_exit",
          class = "page",
          br(), br(), 
          h2("Thank you! The survey is finished."), br(), br(),
          h2("Please return the device to your counselor or the office assistant."),
          br(),
          br(),
          tags$p(em("For Office Use Only")),
          div(
            id = "password_section",
            passwordInput("download_password", "Enter password to access report"),
            actionButton("download_pw_submit", "Submit")
          ),
          uiOutput("download_report_ui"),
          br(), br(),
          actionButton("exit_btn", "Exit", class = "btn-primary btn-lg"),
          br(), br(), br()
        ),

        # 1-7 scale
        div(
          id = "scale_instructions",
          #"For these questions:",
          div(class = "scale_label", "NEVER"),
          div(class = "scale_label"),
          div(class = "scale_label", "SOMETIMES"),
          div(class = "scale_label"),
          div(class = "scale_label", "OFTEN"),
          div(class = "scale_label"),
          div(class = "scale_label", "ALWAYS")
        )
      ))
    )
  ),

  # Navigation buttons
  fluidRow(
    column(
      width = 4,
      offset = 4,
      div(
        id = "survey_nav",
        actionButton("prevBtn", "< Previous"),
        actionButton("nextBtn", "Next >", class = "btn-primary pull-right")
      )
    ),
    column(12, textOutput("page_num_out"))
  )
)


