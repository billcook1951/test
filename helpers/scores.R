
# SRM family assessment app for positivity
#
source("helpers/score-parameters.R")

#' Fix the adjustments variable of a dataframe from a user-friendly version
#' to a machine-friendly version. The original dataframe can specify adjustments
#' as a list for ease of use, and this function will convert them to dataframes.
fix_adjustments <- function(df, colname = "adjustments") {
  if (colname %in% colnames(df)) {
    df[[colname]][] <- lapply(df[[colname]], function(x) {
      if(length(x) == 1 && is.na(x)) NA
      else data_frame(base = names(x), adjustment = unlist(unname(x)))
    })
  }
  df
}

relationship_fits <- fix_adjustments(relationship_fits)
act_fits <- fix_adjustments(act_fits)
part_fits <- fix_adjustments(part_fits)
rel_fits <- fix_adjustments(rel_fits)

# The adjustment values used to adjust various scores
adjustments_df <- tribble(
  ~base,    ~coef,
  "MO_age", (age_fits %>% filter(person_id == ID_MOTHER) %>% pull(coef)),
  "FA_age", (age_fits %>% filter(person_id == ID_FATHER) %>% pull(coef)),
  "C1_age", (age_fits %>% filter(person_id == ID_CHILD_OLD) %>% pull(coef)),
  "C2_age", (age_fits %>% filter(person_id == ID_CHILD_YOUNG) %>% pull(coef)),
  "C1_sex", 0,
  "C2_sex", 0
)
calculate_adjustment_values <- function(fam_info) {
  data_frame(
    base = c("MO_age", "FA_age", "C1_age", "C2_age", "C1_sex", "C2_sex"),
    x = c(
      fam_info %>% filter(person_id == ID_MOTHER) %>% pull(age),
      fam_info %>% filter(person_id == ID_FATHER) %>% pull(age),
      fam_info %>% filter(person_id == ID_CHILD_OLD) %>% pull(age),
      fam_info %>% filter(person_id == ID_CHILD_YOUNG) %>% pull(age),
      if(fam_info %>% filter(person_id == ID_CHILD_OLD) %>% pull(gender) == "Male") 1 else 0,
      if(fam_info %>% filter(person_id == ID_CHILD_YOUNG) %>% pull(gender) == "Male") 1 else 0
    )
  ) %>%
    left_join(adjustments_df, by = "base") %>%
    mutate(value = x - coef) %>%
    select(base, value)
}

#' calculate the "act" for each family member
#'
#' @param person_id_ the family member. valid options include "MO", "FA", "C1", and "C2"
#' @param rel_tbl the `relationship_fits` table above with one additional column `x` that
#' holds the user relationship inputs from the app
#'
#' @return act numeric vector of length 1
#'
#' @examples
#' calc_act("MO", )
#'
calc_act <- function(person_id_, rel_tbl) {    # in rel_tbl, person_1 is actor and person_2 is the partner: see relationship_fits in score parameters
  feelings <- rel_tbl %>%
    filter(person_1 == person_id_) %>%        # 3 item scores where person_id_ is actor
    pull(x)                                   # 
                                              # eg mother-father, c1_father, c2_father
  feelings_towards <- rel_tbl %>%
    filter(person_2 == person_id_) %>%         # 3 item scores where person is the partner
    pull(x)                                   # 

  act_formula(                                # this applies the formula to the scores
    feelings = sum(feelings),                 # the formula uses these arguments
    feelings_towards = sum(feelings_towards),
    fam_average = sum(rel_tbl$x) / 12
  )
}


#' calculate the "part" for each family member
#'
#' @param person_id_ the family member. valid options include "MO", "FA", "C1", and "C2"
#' @param rel_tbl the `relationship_fits` table above with one additional column `x` that
#' holds the user relationship inputs from the app
#'
#' @return part numeric vector of length 1
#'
calc_part <- function(person_id_, rel_tbl) {
  feelings <- rel_tbl %>%
    filter(person_1 == person_id_) %>%
    pull(x)

  feelings_towards <- rel_tbl %>%
    filter(person_2 == person_id_) %>%
    pull(x)

  part_formula(
    feelings = sum(feelings),
    feelings_towards = sum(feelings_towards),
    fam_average = sum(rel_tbl$x) / 12
  )
}

#' caluclate the coef adjustment
#'
#' used to calculate adjustment to the coeffiecient for the
#' z score calculation for "act", "part", and "relationship effects"
#'
#' @param l list of data frames that contain the adjustments.  comes from the
#' `adjustments` column of the `act_fits`, `part_fits`, and `rel_fits` data frames
#' defined above
#' @param df data frame with `value` column for predictor variables.  The `value`
#' is the Shiny app input variable that will be multiplied by the adjustment coefficient
#'
#' @return vector of length `length(l)` with the calculated coef adjustment
#'
#' @examples
#' adjustment_values <- data_frame(
#'    base = c("FA_age", "C1_sex", "C2_sex"),
#'    value = c(40, 10, 10)
#' )
#'
#' calc_adjustment(act_fits$adjustments, adjustment_values, by = "base")
#'
calc_adjustment <- function(l, df_x, by) {
  out <- lapply(l, function(df) {
    if (is.data.frame(df)) {
      dplyr::left_join(df, df_x, by) %>%
        mutate(calc = adjustment * value) %>%
        pull(calc) %>%
        sum()
    } else {
      0
    }
  })

  unlist(out)
}

#' calc_age_z_score
#'
#' @param survey_inputs
#'
#' @import dplyr
#'
calc_age_z_score <- function(survey_inputs) {
  survey_inputs %>%
    dplyr::left_join(age_fits, by = "person_id") %>%
    dplyr::mutate(z_score = (age - coef) / se)
}

#' calc_z_score
#'
#' @param tbl_w_inputs data frame that must have 4 specific columns:
#'   - `coef` the coefficient from the model
#'   - `se` the standard error of the coefficient
#'   - `adjustment_calc` the adjustment to the coefficient made for the z score calculation.  This
#'   column is created by `calc_adjustment()`
#'   - `x` the x variable which is obtained from inputs in the Shiny app
#'
#' @return the `tbl_w_inputs` data frame with an additional column `z_score` which contains the calculated
#' z score
#'
#' @import dplyr
#'
calc_z_score <- function(tbl_w_inputs) {
  tbl_w_inputs %>%
    dplyr::mutate(z_score = round((x - coef - adjustment_calced) / se, digits = 3))
}

