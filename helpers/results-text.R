# test version  SRM family assessment app for positivity
#

#' Capitalize the first letter of every word in a string (tools::toTitleCase
#' doesn't capitalize words such as "you")
capitalize_words <- function(s) {
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", s, perl = TRUE)
}

#' Get the name of a person in the family
person_name <- function(fam_members, person_id_) {
  fam_members %>% filter(person_id == person_id_) %>% pull(name)
}

#' Return the role of the person in the family (mother, father, older sibling,
#' or younger sibling).
person_role <- function(fam_members, person_id_) {
  fam_members %>% filter(person_id == person_id_) %>% pull(person)
}



person_gender <- function(fam_members, person_id_)
  fam_members %>% filter(person_id == person_id_) %>% pull(gender)


#' Used to be -> Return "your" when referring to the mom or "Bob's" when referring to someone
#' else.
your_theirs <- function(fam_members, person_id_) {
      paste0(person_name(fam_members, person_id_), "'s")
}


#' upper case Return "Your" when referring to person_id_ or person_name when referring to someone
#' else.
Your_Theirs <- function(fam_members, person_id_) {
     paste0(person_name(fam_members, person_id_), "'s")
}

#' Return "your" when referring to the mom or "his"/"her" when referring to
#' someone else.
your_his_her <- function(person_id_) {
  "his/her"
}


#' UPPER CASE for Return "Your" when referring to person_id_ or "His"/"Her" when referring to
#' someone else at beginning of sentence
Your_His_Her <- function(person_id_) {
  "His/Her"
}


#' Return "you elicit" or "Bob elicits"
you_they_elicit <- function(fam_members, person_id_) {
    paste0(person_name(fam_members, person_id_), " elicits")
}

#' Return "you have" or "Bob has"
#you_they_have <- function(fam_members, person_id_) {
#  if (person_id_ == ID_MOTHER) {
#    "you have"
#  } else {
#    paste0(person_name(fam_members, person_id_), " has")
#  }
#}

#new from Bill C.
#' Return "you have" or "Bob has"
you_they_express <- function(fam_members, person_id_) {
     paste0(person_name(fam_members, person_id_), " expresses")
}
  

you_they_name <- function(fam_members, person_id_) {
  paste0(person_name(fam_members, person_id_))
}


#' srm_html_output
#'
#' generates the html to display the SRM results for each family member
#'
#' @param srm srm z scores from `srm_score_text()` reactive
#' @param fam_members  data frame with the names of each family member.  From `fam_members_tbl()` reactive
#'
#' @return tagList to display SRM results
#'
srm_html_output <- function(srm, fam_members, person_id_) {
  fam_z <- srm$fam_z
  act_z <- srm$act_z
  part_z <- srm$part_z
  rel_z <- srm$rel_effects_z

  ppl <- fam_members %>% mutate(person = capitalize_words(person))
  person_id_name <- ppl %>% filter(person_id == person_id_) %>% pull(person)
  other_ppl <- ppl %>% filter(person_id != person_id_)

  rels_html <- tagList()

  your_theirs_text <- your_theirs(fam_members, person_id_)
  Your_Theirs_text <- Your_Theirs(fam_members, person_id_) # upper case for Your (above)
  your_his_her_text <- your_his_her(person_id_)
  Your_His_Her_text <- Your_His_Her(person_id_) # upper case for Your His/Her (above)
  you_they_elicit_text <- you_they_elicit(fam_members, person_id_)
  you_they_express_text <- you_they_express(fam_members, person_id_)
  you_they_name_text <- you_they_name(fam_members, person_id_)
  

  for (i in seq(nrow(other_ppl))) {
    rels_html[[i]] <- div(
      h4(paste0(person_id_name, " - ", other_ppl$person[i], " Relationship Effect"), class = "section-subtitle"), br(),
      HTML(paste0(
        Your_Theirs_text, " experience of ",
        fam_members %>% filter(person_id == other_ppl$person_id[i]) %>% pull(name),
        "'s positivity that is unique to their relationship is ",
        tags$strong(rel_z %>% filter(person_1 == person_id_, person_2 == other_ppl$person_id[i]) %>% pull(text_highlow)),
        ", (Z = ", rel_z %>%  filter(person_1 == person_id_, person_2 == other_ppl$person_id[i]) %>% pull(z_score),")."
      ))
    )
  }

  tagList(
    h2(paste0(person_id_name, "'s Social Relations Model (SRM) Results")),
    HTML(paste0("The Family Effect measures the amount of positivity experienced by the 
                            average person in the family. It will be the same for each family member. 
                            The Perceiver Effect measures how much the individual 
                            experiences other family members in general as positive.  The Partner Effect measures how 
                            positive an individual is in the experience of other family members in general. A Relationship
                            Effect measures how much positivity the individual experiences from the particular
                            partner, over and above the positivity due to the family, perceiver, and partner 
                            effects affecting that relationship.")),
    br(),
    h4("Family Effect", class = "section-subtitle"), br(),
    HTML(paste0(
      "Compared to the average benchmark family, the level
      of positivity in this family is ", 
      tags$strong(fam_z$text_highlow),
      ", (Z = ", round(fam_z$z_score, 2),")."
    )),
    h4(paste0(person_id_name, " Perceiver Effect"), class = "section-subtitle"),br(),
    HTML(paste0(
      Your_Theirs_text,
      " general experience of positivity from other family members is ",
      tags$strong(act_z %>% filter(person_id == person_id_) %>% pull(text_highlow)),
      ", (Z = ", srm$act_z %>%  filter(person_id == person_id_) %>% pull(z_score),")."
    )),
    h4(paste0(person_id_name, " Partner Effect"), class = "section-subtitle"), br(),
    HTML(paste0(
      "The amount of positivity that family members generally experience from ",
      tags$strong(you_they_name_text), " is ",
      tags$strong(part_z %>% filter(person_id == person_id_) %>% pull(text_highlow)),
      ", (Z = ", part_z %>%  filter(person_id == person_id_) %>% pull(z_score),")."
    )),
    rels_html
    )
}



narrative_html_output <- function(srm, zscores, fam_members, person_id_) {
  role <- person_role(fam_members, person_id_)
  your_theirs_text <- your_theirs(fam_members, person_id_)
  you_they_express_text <- you_they_express(fam_members, person_id_)
  you_they_elicit_text <- you_they_elicit(fam_members, person_id_)
  
 
  title <- h2("The SRM Explanation of",
              paste0(capitalize_words(role), "'s"),
              "Family Relationships")

  intro_paragraph <- tags$div(HTML(paste0(
    tags$strong(capitalize_words(your_theirs_text)),
    " experience of positivity from a particular family member is influenced by the general level
     of positivity in the family (the family effect), ",
    tags$strong(your_theirs_text),
    " experience of positivity from other family members in general (",
    tags$strong(your_theirs_text),
    " perceiver effect), the amount of positivity family members generally experience
     from that partner (",tags$strong("the partner effect"), "), and ",
    tags$strong(your_theirs_text),
    " experience of positivity that is unique to their relationship (the relationship effect)."
  )))
  
#paste0(person_name(fam_members, person_id_))
  
  
  relationship_paragraph_template <- function(other_person_id_) {
    other_name <- person_name(fam_members, other_person_id_)
    you_they_express_text <- you_they_express(fam_members, person_id_) #######################################
    you_they_elicit_text <- you_they_elicit(fam_members, other_person_id_)

    tags$div(HTML(paste0(
      br(),
      "The amount of positivity that ",
      paste0(person_name(fam_members, person_id_)),
     # tags$strong(capitalize_words(your_theirs_text)),
      " experiences from ",
      (other_name),
      " is ",
      tags$strong(zscores %>% filter(person_1 == person_id_, person_2 == other_person_id_) %>% pull(score_word)),
      ". It is influenced by the ",tags$strong("family group effect"),", which is ",
      (srm$fam_z$text_highlow),
      ", the amount of positivity that ",
     paste0(person_name(fam_members, person_id_))," experiences from others in general (",
      tags$strong("the perceiver effect"),"), which is ",
      (srm$act_z %>% filter(person_id == person_id_) %>% pull(text_highlow)), 
      ", the amount of positivity family members generally experience from ",
      (other_name),  
     " (",
     tags$strong("the partner effect"),
     "), which is ",
      (srm$part_z %>% filter(person_id == other_person_id_) %>% pull(text_highlow)),
      ", and the degree to which ",
     paste0(person_name(fam_members, person_id_)),
      " experiences a unique level of positivity from ",
     tags$strong(other_name),
      " (",tags$strong("the relationship effect"),"), which is ",
      (srm$rel_effects_z %>% filter(person_1 == person_id_, person_2 == other_person_id_) %>% pull(text_highlow)),
      "."
    )))
  }

  other_members <- setdiff(fam_members$person_id, person_id_)

  tagList(
    title,
    intro_paragraph,
    lapply(other_members, relationship_paragraph_template)
  )
}
