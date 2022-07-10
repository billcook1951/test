# The initial questions in the survey, gathering participant information.
# Each item in the list represents a page in the survey, with the name of the
# item being the page name, and the value of the item being a list of questions
# on the page. Each question in the list of questions is also a list, where
# the name of the item is the name of the question (and automatically used as
# the name of the Shiny input), and the value of the item being the parameters
# of the question.
survey_start <- list(
  "name_you" = list(
    "name_you" = list(
      type = "text",
      label = "What is the mother's first name?"
    )
  ),
  "age_you" = list(
    "age_you" = list(
      type = "numeric",
      label = "How old is the mother?",
      value = 45
    )
  ),
  "name_other" = list(
    "name_other" = list(
      type = "text",
      label = "What is the father's first name?"
    )
  ),
  "age_other" = list(
    "age_other" = list(
      type = "numeric",
      label = "How old is the father?",
      value = 48
    )
  ),
  "name_child_1" = list(
    "name_child_1" = list(
      type = "text",
      label = "What is the name of the older child in the family?"
    )
  ),
  "child_1" = list(
    "age_child_1" = list(
      type = "numeric",
      label = "What is the age of the older child?",
      value = 19
    ),
    "gender_child_1" = list(
      type = "radio",
      label = "What is the gender of the older child?",
      choices = c("Male", "Female")
    )
  ),
  "name_child_2" = list(
    "name_child_2" = list(
      type = "text",
      label = "What is the name of the younger child in the family?"
    )
  ),
  "child_2" = list(
    "age_child_2" = list(
      type = "numeric",
      label = "What is the age of the younger child?",
      value = 16
    ),
    "gender_child_2" = list(
      type = "radio",
      label = "What is the gender of the younger child?",
      choices = c("Male", "Female")
    )
  )
)
