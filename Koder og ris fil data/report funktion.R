# Report function - vil indlægge senere i AIscreenR --------
report <- 
  function(data, studyid, title, abstract, answer, format = "html_document", ...){
    
    studyid <- data |> dplyr::pull({{ studyid }})
    title <- data |> dplyr::pull({{ title }})
    abstract <- data |> dplyr::pull({{ abstract }})
    
    studyid_txt <- paste0("**STUDY-ID: ", studyid, ":**", "\n\n")
    title_text <- paste0("-- *Title:* '", title, "'", "\n\n")
    abs_txt <- paste0("-- *Abstract*: ", abstract, "\n\n")
    
    if (missing(answer)){
      answer_txt <- NULL
    } else {
      answer <- data |> dplyr::pull( {{answer}} )
      answer_txt <- paste0("-- *Answer (GPT)*: ", answer, "\n\n")
    }
    
    comment_text <- paste0("*Please add a comment on whether and why you agree with the GPT decision or not:*\n\n \n\n")
    
    print_test <- paste0(studyid_txt, title_text, abs_txt, answer_txt, comment_text)
    
    cat("---
title: \"Study Report - Disagreement Explanations\"
subtitle: \"Included by humans, excluded by GPT\"
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
    message(paste0("Saving to ", getwd(), "/", "StudyReport_AIscreenR.Rmd")) 
    message("Rendering StudyReport_AIscreenR.Rmd")  
    file_out <-  rmarkdown::render("StudyReport_AIscreenR.Rmd", output_format = format, quiet = TRUE, ...) 
    
    message("Opening StudyReport_AIscreenR.Rmd")  
    shell.exec(file_out)
    
    invisible(file_out)
    
  }