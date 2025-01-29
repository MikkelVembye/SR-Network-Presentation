# For CRAN version
# install.packages("AIscreenR")

# Udviklingspakken. PÅ nuværende tidspunkt indeholder denne nye funktioner 
# til at skabe fine tuned data og der kommer også snart til at ligge den 
# report funktion som jeg fremviser til sidst i scriptet.  

# install.packages("devtools")
# Jeg benytter altid selv development pakken.
devtools::install_github("MikkelVembye/AIscreenR")

library(AIscreenR) # Bruges til at screene
library(synthesisr)  # Bruges til at loade RIS filer
library(tibble)    # Til at arbejde med en forbedre version af data.frames kaldet tibbler
library(dplyr)     # Bruges til data manipulation
library(future)    # Bruges til at køre AIscreenR funktionerne parallelt 
library(readxl)    # Bruges til at gennem Excel filer, hvis man ønsker dette

# Sæt sti til filer
setwd("C:/Users/B199526/Desktop/GitHub repos/SR-Network-Presentation/Koder og ris fil data/")

# Tjek sti er korrekt
getwd()

# Indlæs og konverter referencer ----------

# Indlæs irrelevante referencer 
ris_dat_excl <- synthesisr::read_refs("irrelevant_amh-review.ris") 

set.seed(291024)

# Lav test data ved at sample x antal referencer ----------
excluded_sample <- 
  ris_dat_excl |> 
  as_tibble() |>
  # Bruger kun relevant variabler
  select(author, title, abstract) |> # Using only relevant variables
  mutate(
    # Tracker den menneskelig beslutning
    human_code = 0, 
    # Håndtering af missing observationer
    across(c(author, title, abstract), ~ na_if(., "NA")) 
  ) |> 
  # Fjerner referencer uden abstract, da disse kan forstyre screeeningen
  dplyr::filter(!is.na(abstract)) |> 
  # Funktion fra AIscreenR til at sample referencer som kan bruges til at skabe test data
  AIscreenR::sample_references(n = 100) 
  

# Indlæs irrelevante referencer
ris_dat_incl <- synthesisr::read_refs("title-abs-included_amh-review.ris") 

# Sample referencer til test data
included_sample <- 
  ris_dat_incl |> 
  as_tibble() |>
  # Bruger kun relevant variabler
  select(author, title, abstract) |> 
  mutate(
    # Tracker den menneskelig beslutning
    human_code = 1,
    # Håndtering af missing observationer
    across(c(author, title, abstract), ~ na_if(., "NA")) 
  ) |> 
  # Fjerner referencer uden abstract, da disse kan forstyre screeeningen
  dplyr::filter(!is.na(abstract)) |> 
  sample_references(n = 50) 
  
# Sammensæt test data
test_dat <- 
  bind_rows(excluded_sample, included_sample) |> 
  # Lav study IDs
  mutate(studyid = 1:n()) 


# Skriv prompt ----------

prompt <- "We are screening studies (i.e., titles and abstracts) for a systematic review. 
The central question of  the review is whether abortion increase the risk of adverse mental health outcomes. 
For each study, I would like you to assess: 1) Is the study about abortion and not 
about: legislation, legal cases, clinical practice, financial costs, or pain medication?
2) Does the study clearly reference mental health outcomes (or sequelae) of abortion?
3) Is this a quantitative study? In other words, does this study (or substudy) design 
include a non-abortion comparison group?"

# Håndter API nøgle ----------

# Midlertid tilføjelse af API nøgle til environment
AIscreenR::set_api_key()

# For at se, hvad funktion har gjort, man man kalde Sys.getenv()

# Tilføj API nøgle som fast environment variabel. Herefter skal du ikke tænke på den igen 
#usethis::edit_r_environ(). Se mere i vignette om dette. 
AIscreenR::get_api_key()

# Rate limits info ----------
# Her I få indblik i, hvilken og hvor stor adgang I har til OpenAI's modeller. Ens tier-gruppe kan sætte begrænsninger for en. 
rate_limit <- AIscreenR::rate_limits_per_minute()  
# Se alle modeller. 
rlpm_alle_modeller <- rate_limits_per_minute(model = model_prizes$model); rlpm_alle_modeller  

# Kør første screening ----------

# Sæt parallel plan
plan(multisession)

resultat_obj <-
  tabscreen_gpt(
    data = test_dat,                       # Ens data
    prompt = prompt,                       # Ens prompts(s) - kan tage flere prompts
    studyid = studyid,                     # Variabel navn med study IDs
    title = title,                         # Variable navn som indeholder titler
    abstract = abstract,                   # Variable navn som indeholder abstracts
    model = "gpt-4o-mini",                 # Model - kan tage mange modeller
    reps = 1,                              # Antal screeninger
    rpm = rate_limit$requests_per_minute   # Angiv hvor requests per minute man har på den givne model
   
  ); resultat_obj

# Lukker parallel plan
plan(sequential)

# Worst case
#saveRDS(resultat_obj, file = "res_obj_4o-mini_1rep.rds")
#resultat_obj <- readRDS("res_obj_4o-mini_1rep.rds")

# Analyser første screening ----------
performance <- 
  resultat_obj |> 
  AIscreenR::screen_analyzer(
    # Angiv variabel navn på de menneskelige beslutninger
    human_decision = human_code,
    # Angiv om kun nøgleresultater skal printes
    key_result = TRUE # Default
  )

performance 

#saveRDS(return_dat, file = "res_obj_4o-mini_1rep.rds")

# Brug af flere screeninger ----------
# Lav 10 screening for at indfange modellens usikkerhed. 
plan(multisession)

resultat_obj_reps10 <-
  tabscreen_gpt(
    data = test_dat,
    prompt = prompt,
    studyid = studyid,
    title = title,
    abstract = abstract,
    model = "gpt-4o-mini",
    reps = 10,
    rpm = rate_limit$requests_per_minute
  )

resultat_obj_reps10

plan(sequential)

# Gem resultat
#saveRDS(resultat_obj_reps10, file = "res_obj_4o-mini_reps10.rds")

# Har snydt hjemme fra ;-) 
resultat_obj_reps10 <- readRDS("res_obj_4o-mini_reps10.rds")
resultat_obj_reps10

performance10 <- 
  resultat_obj_reps10 |> screen_analyzer()

performance10

performance_dist <- attr(performance10, "p_incl_data")
performance_dist

# Hvis vi har tid:
# Tjek uenigheder for de studier som mennesker inkluderede
# HUSK også at tjekke studier som er inkluderet af GPT men eksluderet af mennesker
disagree_dat <- 
  resultat_obj_reps10$answer_data_aggregated |> 
  filter(human_code == 1, incl_p == 0) 
  
plan(multisession)

dis_res <-
  tabscreen_gpt(
    data = disagree_dat,
    prompt = prompt,
    studyid = studyid,
    title = title,
    abstract = abstract,
    model = "gpt-4o-mini",
    decision_description = TRUE,
    reps = 10, 
    rpm = rate_limit$requests_per_minute
  ); dis_res

plan(sequential)


# 
#saveRDS(dis_res, file = "disagreement_description_res_obj_4o-mini_10reps.rds")
# Worst case
#dis_res <- readRDS("disagreement_description_res_obj_4o-mini_10reps.rds")

dis_res$answer_data_aggregated |> select(author, abstract, final_decision_gpt, longest_answer) |> View()

source("report funktion.R")

report(
  data = dis_res$answer_data_aggregated, 
  studyid = studyid, 
  title = title, 
  abstract = abstract, 
  answer = longest_answer, 
  format = "html_document"
)

# Anslå pris-----------
# Anslå pris for fuld screening (13782 refs)
all_dat <- bind_rows(ris_dat_excl, ris_dat_incl) |> as_tibble() 
nrow(all_dat)

app_price <- 
  approximate_price_gpt(
    data = all_dat,
    prompt = prompt,
    title = title,
    abstract = abstract,
    model = "gpt-4o-mini",
    reps = 10
  )

app_price$price_data
app_price$price_dollar 

