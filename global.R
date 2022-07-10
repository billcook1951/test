# test version  SRM family assessment app for positivity
#
library(shiny)
library(shinyjs)
library(dplyr)

# You can turn on developer mode by setting a variable `DEV <- TRUE`
#DEV_MODE <- exists("DEV") && DEV
DEV_MODE <- TRUE

NUM_PEOPLE     <- 4
ID_MOTHER      <- "MO"
ID_FATHER      <- "FA"
ID_CHILD_OLD   <- "C1"
ID_CHILD_YOUNG <- "C2"
ID_FAMILY      <- "FAM"
ID_ALL_MEMBERS <- c(ID_MOTHER, ID_FATHER, ID_CHILD_OLD, ID_CHILD_YOUNG)
build_relationship <- function(id1, id2) paste0(id1, "_", id2)


source("helpers/scores.R")
source("helpers/ui-helpers.R")
source("helpers/survey-participant-info-format.R")
source("helpers/results-text.R")

# The order of person_1 and person_2 will define the order of the rating questions

relationships <- data_frame(
  person_1 = c(
    ID_FATHER, ID_CHILD_OLD, ID_CHILD_YOUNG,
    ID_MOTHER, ID_MOTHER, ID_MOTHER,
    ID_FATHER, ID_FATHER, ID_CHILD_OLD,
    ID_CHILD_OLD, ID_CHILD_YOUNG, ID_CHILD_YOUNG
  ),
  person_2 = c(
    ID_MOTHER, ID_MOTHER, ID_MOTHER,
    ID_FATHER, ID_CHILD_OLD, ID_CHILD_YOUNG,
    ID_CHILD_OLD, ID_CHILD_YOUNG, ID_FATHER,
    ID_CHILD_YOUNG, ID_FATHER, ID_CHILD_OLD
  )
)

relationships <- relationships %>%
  dplyr::mutate(
    relationship = build_relationship(person_1, person_2),
    input_id = build_relationship("relationship", relationship))  # yields e.g. relationship_MO_FA

# expanding likert scale questions from 1 question per relationship to
# X questions per relationship
NUM_QUESTIONS_PER_RELATIONSHIP <- 4
NUM_QUESTIONS_PER_PERSON <- (NUM_PEOPLE - 1) * NUM_QUESTIONS_PER_RELATIONSHIP
input_letters <- LETTERS[1:NUM_QUESTIONS_PER_RELATIONSHIP]
relationship_input_ids <- as.vector(t(outer(relationships$input_id, input_letters, paste, sep = "_")))
# for example relationship_MO_FA_A, relationship_MO_FA_B.

# Define all the pages
steps_template <- paste0(
  "page_",
  c(
    "intro",
    "disclaimer",
    names(survey_start),
    rep(c("choose_next_participant", rep("relationship_XX", NUM_QUESTIONS_PER_PERSON)), NUM_PEOPLE),
    "exit"
  )
)
DISCLAIMER_PAGE <- 2
PAGE_SURVEY_START <- DISCLAIMER_PAGE + 1
PAGES_NUM <- length(steps_template)
