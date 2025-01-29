
install_github("MikkelVembye/AIscreenR")

library(AIscreenR)
library(revtools) 
library(tibble)    
library(dplyr)     
library(purrr)
library(future) 
library(readxl)

#Reading path to risfiles
excl_path <- list.files(pattern = "irrelevan")

# Loading EXCLUDED studies
ris_dat_excl <- revtools::read_bibliography(excl_path) |> 
  suppressWarnings()

set.seed(291024)

excluded_sample <- 
  ris_dat_excl |> 
  as_tibble() |>
  select(author, title, abstract) |> # Using only relevant variables
  mutate(
    human_code = 0,
    across(c(author, title, abstract), ~ na_if(., "NA"))
  ) |> 
  dplyr::filter(!is.na(abstract)) |> 
  sample_references(n = 100) 
  

# Loading INCLUDED studies
incl_path <- list.files(pattern = "included")

ris_dat_incl <- revtools::read_bibliography(incl_path) |> 
  suppressWarnings()

included_sample <- 
  ris_dat_incl |> 
  as_tibble() |>
  select(author, title, abstract) |> # Using only relevant variables
  mutate(
    human_code = 1,
    across(c(author, title, abstract), ~ na_if(., "NA"))
  ) |> 
  dplyr::filter(!is.na(abstract)) |> 
  sample_references(n = 50) 
  

test_dat <- 
  bind_rows(excluded_sample, included_sample) |> 
  mutate(studyid = 1:n())

prompt <- "We are screening studies (i.e., titles and abstracts) for a systematic review. 
The central question of  the review is whether abortion increase the risk of adverse mental health outcomes. 
For each study, I would like you to assess:
1) Is the study about abortion and not about: legislation, legal cases, clinical practice, financial costs, or pain medication?
2) Does the study clearly reference mental health outcomes (or sequelae) of abortion?
3) Is this a quantitative study? In other words, does this study (or substudy) design include a non-abortion comparison group?
"
  

#plan(multisession)
#
#return_dat <-
#  tabscreen_gpt(
#    data = test_dat,
#    prompt = prompt,
#    studyid = studyid,
#    title = title,
#    abstract = abstract,
#    model = "gpt-4o-mini"
#   
#  ); return_dat
#
#plan(sequential)
#
#return_dat |> screen_analyzer()
#saveRDS(return_dat, file = "abortion_res_obj_4o-mini.rds")
return_dat <- readRDS("abortion_res_obj_4o-mini.rds")
return_dat |> screen_analyzer()

# With 10 reps
#plan(multisession)
#
#return_dat_reps10 <-
#  tabscreen_gpt(
#    data = test_dat,
#    prompt = prompt,
#    studyid = studyid,
#    title = title,
#    abstract = abstract,
#    model = "gpt-4o-mini",
#    reps = 10
#  )
#
#plan(sequential)
#
#saveRDS(return_dat_reps10, file = "abortion_res_obj_4o-mini_reps10.rds")
return_dat_reps10 <- readRDS("abortion_res_obj_4o-mini_reps10.rds")
return_dat_reps10 |> screen_analyzer()


map(0:9/10, ~ {
  return_dat_reps10$answer_data_aggregated |> 
    mutate(
      final_decision_gpt = dplyr::if_else(incl_p > .x, "Include", "Exclude"),
      final_decision_gpt_num = dplyr::if_else(final_decision_gpt == "Include", 1, 0) 
    ) |> 
    screen_analyzer() |> 
    mutate(criteria = paste0("Studies have been included in at least ", (.x + 0.1)*10, " out of ", reps, " screenings."))
}
)
# Testing differences between models
#plan(multisession)
#
#return_dat2 <-
#  tabscreen_gpt(
#    data = test_dat,
#    prompt = prompt,
#    studyid = studyid,
#    title = title,
#    abstract = abstract,
#    model = c("gpt-4o", "gpt-4")
#    
#  ); return_dat2
#
#plan(sequential)
#
#saveRDS(return_dat2, file = "abortion_res_obj_4_and_4o.rds")
return_dat2 <- readRDS("abortion_res_obj_4_and_4o.rds")
return_dat2 |> screen_analyzer()

# Get detailed decision for disaggreements

disagree_dat <- 
  return_dat$answer_data |> 
  filter(human_code == 1, decision_binary == 0) |>  
  tibble::as_tibble() |> 
  select(author:abstract)
  

#plan(multisession)
#
#dis_res <-
#  tabscreen_gpt(
#    data = disagree_dat,
#    prompt = prompt,
#    studyid = studyid,
#    title = title,
#    abstract = abstract,
#    model = "gpt-4o-mini",
#    decision_description = TRUE
#  ); dis_res
#
#plan(sequential)
#
#
#saveRDS(dis_res, file = "disagreement_description_res_obj_4o-mini.rds")
dis_res <- readRDS("disagreement_description_res_obj_4o-mini.rds")
dis_res$answer_data |> select(author, detailed_description) |> View()

# Approximate prize of full screening (13782 refs)
price_dat <- bind_rows(ris_dat_excl, ris_dat_incl) |> as_tibble() 

app_price <- 
  approximate_price_gpt(
    data = price_dat,
    prompt = prompt,
    title = title,
    abstract = abstract,
    model = "gpt-4o-mini"
)

app_price$price_data
app_price$price_dollar # 1.4456 CRAZY

#Larger screening to test consistency

set.seed(301024)

#EXCLUDED studies
excluded_sample_2000 <- 
  ris_dat_excl |> 
  as_tibble() |>
  select(author, title, abstract) |> # Using only relevant variables
  mutate(
    human_code = 0,
    across(c(author, title, abstract), ~ na_if(., "NA"))
  ) |> 
  dplyr::filter(!is.na(abstract)) |> 
  sample_references(n = 2000) 


# INCLUDED studies 
included_sample_all <- 
  ris_dat_incl |> 
  as_tibble() |>
  select(author, title, abstract) |> # Using only relevant variables
  mutate(
    human_code = 1,
    across(c(author, title, abstract), ~ na_if(., "NA"))
  ) |> 
  dplyr::filter(!is.na(abstract)) 


test_dat2 <- 
  bind_rows(excluded_sample_2000, included_sample_all) |> 
  mutate(studyid = 1:n())

prompt <- "We are screening studies (i.e., titles and abstracts) for a systematic review. 
The central question of  the review is whether abortion increase the risk of adverse mental health outcomes. 
For each study, I would like you to assess:
1) Is the study about abortion and not about: legislation, legal cases, clinical practice, financial costs, or pain medication?
2) Does the study clearly reference mental health outcomes (or sequelae) of abortion?
3) Is this a quantitative study? In other words, does this study (or substudy) design include a non-abortion comparison group?
"

#plan(multisession)
#
#large_return_dat <-
#  tabscreen_gpt(
#    data = test_dat2,
#    prompt = prompt,
#    studyid = studyid,
#    title = title,
#    abstract = abstract,
#    model = "gpt-4o-mini"
#    
#  ); large_return_dat
#
#plan(sequential)
#
#saveRDS(large_return_dat, file = "large_test_res_obj_4o-mini.rds")
large_return_dat <- readRDS("large_test_res_obj_4o-mini.rds")
large_return_dat |> screen_analyzer(key_result = FALSE) |> print(width = 300)

#disagree_incl_dat <- 
#  large_return_dat$answer_data |> 
#  dplyr::filter(human_code == 1, decision_binary == 0)
#
#rescreen_disagreed <- 
#  tabscreen_gpt(
#    data = disagree_incl_dat,
#    prompt = prompt,
#    studyid = studyid,
#    title = title,
#    abstract = abstract,
#    model = "gpt-4o-mini",
#    decision_description = TRUE
#  )
#
#saveRDS(rescreen_disagreed, file = "large_rescreen_disagreed_4o-mini.rds")
rescreen_disagreed <- readRDS("large_rescreen_disagreed_4o-mini.rds")
  
dis_dat <- 
  rescreen_disagreed$answer_data |> 
  dplyr::filter(decision_binary == 0)

# Report function
report <- 
  function(data, studyid, title, abstract, answer, format = "html_document", ...){
    
    studyid <- data |> dplyr::pull({{ studyid }})
    title <- data |> dplyr::pull({{ title }})
    abstract <- data |> dplyr::pull({{ abstract }})
    
    studyid_txt <- paste0("**STUDY(id) ", studyid, ":**", "\n\n")
    title_text <- paste0("-- *Title:* '", title, "'", "\n\n")
    abs_txt <- paste0("-- *Abstract*: ", abstract, "\n\n")
    
    if (missing(answer)){
      answer_txt <- NULL
    } else {
      answer <- data |> dplyr::pull( {{answer}} )
      answer_txt <- paste0("-- *Answer (GPT)*: ", answer, "\n\n")
    }
    
    print_test <- paste0(studyid_txt, title_text, abs_txt, answer_txt)
    
    cat("---
title: \"Study report\"
output: html_document
---
           
\`\`\`{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
\`\`\`


\`\`\`{r, echo=FALSE, results='asis', include=TRUE}

base::cat(print_test)

\`\`\`", 
        file = "StudyReport_AIscreenR.Rmd"
    )
    message(paste0("Saving to", getwd(), "/", "StudyReport_AIscreenR.Rmd")) 
    message("Rendering StudyReport_AIscreenR.Rmd")  
    file_out <-  rmarkdown::render("StudyReport_AIscreenR.Rmd", output_format = format, quiet = TRUE, ...) 
    
    message("Opening StudyReport_AIscreenR.Rmd")  
    shell.exec(file_out)
    
    invisible(file_out)
    
  }

report(data = dis_dat, studyid = studyid, title = title, abstract = abstract, answer = detailed_description, format = "word_document")


# False included (Will add later)