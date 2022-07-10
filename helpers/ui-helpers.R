# Build an input for the identifying info part of the survey
build_survey_input <- function(id, params) {
  # Decide which input function to use
  input_func <- switch(
    params$type,
    "text" = textInput,
    "numeric" = numericInput,
    "radio" = radioButtons
  )

  # Add/remove parameters
  params$inputId <- id
  if (params$type %in% c("text", "numeric")) {
    params$width <- "100%"
  }
  if (params$type == "radio") {
    params$inline <- TRUE
  }
  params$type <- NULL

  # Build the input tag
  tag <- do.call(input_func, params)
  tag
}
