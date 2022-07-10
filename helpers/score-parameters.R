#  test version SRM family assessment app for positivity
#

# --- Define constants for z thresholds ----
z_age_threshold <- 1.959
z_positivity_threshold <- 1.00
z_srm_threshold <- 1.00


# --- Define constants for age z scores ----
age_fits <- tribble(
  ~person_id,     ~coef,  ~se,
  ID_MOTHER,      45.88,  4.43,
  ID_FATHER,      48.01,  5.41,
  ID_CHILD_OLD,   19.72,  2.16,
  ID_CHILD_YOUNG, 16.15,  2.16
)


# --- Define constants for positivity adjusted z scores ----
relationship_fits <- tribble(
  ~person_1,      ~person_2,      ~coef,   ~se,    ~adjustments,
  ID_MOTHER,      ID_FATHER,      4.903,   1.166,  list("FA_age" = -0.038),
  ID_MOTHER,      ID_CHILD_OLD,   5.066,   1.006,  list("C1_sex" = -0.365),
  ID_MOTHER,      ID_CHILD_YOUNG, 4.897,   1.015,  list("C2_sex" = -0.412),
  ID_FATHER,      ID_MOTHER,      4.915,   1.023,  NA,
  ID_FATHER,      ID_CHILD_OLD,   4.571,   0.974,  NA,
  ID_FATHER,      ID_CHILD_YOUNG, 4.596,   1.041,  list("C2_sex" = -0.449), 
  ID_CHILD_OLD,   ID_MOTHER,      5.239,   0.997,  list("C1_age" =  0.071),
  ID_CHILD_OLD,   ID_FATHER,      4.709,   1.118,  list("C2_age" =  0.055),
  ID_CHILD_OLD,   ID_CHILD_YOUNG, 4.753,   0.986,  list("C2_age" =  0.085, "C2_sex" = -0.247),
  ID_CHILD_YOUNG, ID_MOTHER,      5.193,   1.009,  list("C2_sex" = -0.508),
  ID_CHILD_YOUNG, ID_FATHER,      4.539,   1.107,  NA,
  ID_CHILD_YOUNG, ID_CHILD_OLD,   4.633,   1.099,  NA
)

# --- Define SRM actor effect formula ----
act_formula <- function(feelings, feelings_towards, fam_average) {
  ((3 * 3) / (4 * 2) * feelings / 3) +
    (3 / (4 * 2) * feelings_towards / 3) -
    (3 / 2 * fam_average)
}


# --- Define SRM partner effect formula ----
part_formula <- function(feelings, feelings_towards, fam_average) {
  ((3 * 3) / (4 * 2) * feelings_towards / 3) +
    (3 / (4 * 2) * feelings / 3) -
    (3 / 2 * fam_average)
}


# --- Define constants for actor effect adjusted z scores ----
act_fits <- tribble(
  ~person_id,     ~coef,               ~se,    ~adjustments,
  ID_FAMILY,       4.867,              0.534,  list("C2_sex" = -0.213),
  ID_MOTHER,       0.191,              0.591,  NA,
  ID_FATHER,      -0.142,              0.613,  NA,
  ID_CHILD_OLD,    0.183,              0.569,  NA,
  ID_CHILD_YOUNG, -0.028,              0.619, list("C2_sex" = -0.194)
)

# --- Define constants for partner effect adjusted z scores ----
part_fits <- tribble(
  ~person_id,     ~coef,     ~se,    ~adjustments,
  ID_MOTHER,      0.326,     0.612,  NA,
  ID_FATHER,     -0.166,     0.543,  list("FA_age" = -0.018, "C2_sex" = 206),
  ID_CHILD_OLD,   0.109,     0.552,  list("C1_sex" = -0.312),
  ID_CHILD_YOUNG, -0.112,    0.581,  list("C2_sex" = -0.258)
)

# --- Define constants for relationship effect adjusted z scores ----
rel_fits <- tribble(
  ~person_1,      ~person_2,      ~coef,     ~se,    ~adjustments,
  ID_MOTHER,      ID_FATHER,       0.044,    0.551,  list("MO_age" = -0.015),
  ID_MOTHER,      ID_CHILD_OLD,    0.024,    0.462,  list("C1_sex" =  -0.120),
  ID_MOTHER,      ID_CHILD_YOUNG, -0.018,    0.452,  list("FA_age" =   0.010),
  ID_FATHER,      ID_MOTHER,      -0.012,    0.498,  NA,
  ID_FATHER,      ID_CHILD_OLD,   -0.004,    0.401,  NA,
  ID_FATHER,      ID_CHILD_YOUNG,  0.015,    0.388,  NA,
  ID_CHILD_OLD,   ID_MOTHER,       0.043,    0.436,  list("C2_sex" = -0.031),
  ID_CHILD_OLD,   ID_FATHER,      -0.047,    0.448,  NA,
  ID_CHILD_OLD,   ID_CHILD_YOUNG,  0.003,    0.535,  list("MO_age" = -0.018),
  ID_CHILD_YOUNG, ID_MOTHER,       0.042,    0.454,  list("C2_sex" =  -0.150),
  ID_CHILD_YOUNG, ID_FATHER,       0.006,    0.477,  NA,
  ID_CHILD_YOUNG, ID_CHILD_OLD,    0.029,    0.559,  NA
)
