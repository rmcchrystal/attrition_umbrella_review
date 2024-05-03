
# Load script for packages, functions, etc.
source("Draft_2/Scripts/00_packages_functions_etc.R")

# Load data extracts
import_studies <- read_csv("Draft_2/Data/Extracts/extract_studies.csv")
import_participants <- read_csv("Draft_2/Data/Extracts/extract_participants.csv")
import_trials <- read_csv("Draft_2/Data/Extracts/extract_trials.csv")
import_ramstar <- read_csv("Draft_2/Data/Extracts/extract_ramstar.csv")


## Create lookups --------------------------------------------------------------

# Create authors and years
get_authors_years_tidied <- import_studies %>% 
  select(ref = RefID, Authors, Year) %>% 
  distinct() %>% 
  mutate(
    first_author =  # Get first author
      gsub(
        regex_first_author, "\\1",
        Authors
      ),
    author_short =  # Get last name, removing comma-separated initials
      gsub(
        regex_remove_initials, "",
        first_author,
        perl = TRUE
      ),
    author =  # Final tidying (e.g. remove missed initials, hyphens)
      case_when(
        author_short == "Stahl A" ~ "Stahl",
        grepl(" -", author_short) ~ gsub(" -", "", author_short),
        grepl(" ", author_short)
        & author_short != "de Campos Moreira" ~ gsub(" ", "", author_short),
        TRUE ~ author_short
      ),
    author_year =  # Combine author and year
      paste0(author, " et al. (", Year, ")")
  ) %>% 
  select(ref, author_year)

# Get unique conditions and tidy into a new variable
get_conditions <- import_studies %>% 
  select(Setting) %>% 
  distinct() %>% 
  mutate(
    condition =  # Tidy conditions
      case_when(
        grepl("Anxiety", Setting) ~ "Anxiety disorders",
        grepl("Trauma", Setting) ~ "PTSD",
        grepl("Rheumatoid", Setting) ~ "Rheumatoid arthritis",
        grepl("problems", Setting) ~ "Psychological and behavioural health disorders",
        grepl("conditions", Setting) ~ gsub("conditions", "disorders", Setting),
        Setting == "Mixed" ~ "Mixed conditions",
        TRUE ~ Setting
      )
  )

# Save conditions for manual grouping
#write_csv(
#  get_conditions, 
#  "Draft_2/Scratch_data/toGroup_unique_conditions.csv"
#)

# Import grouped conditions, join to extract
get_conditions_tidied <- get_conditions %>% 
  left_join(
    read_csv("Draft_2/Scratch_data/grouped_unique_conditions.csv")
  )

# Get associated and unassociated participant characteristics in each study
get_participant_chars <- import_participants %>% 
  select(ref = RefID, participant_char = `Associated PC`) %>% 
  mutate(outcome = "associated") %>%
  full_join(  ## Join and separate unassociated characteristics per study
    import_participants %>% 
      select(ref = RefID, participant_char = `Unassociated PCs`) %>% 
      mutate(
        outcome = "unassociated",
        participant_char = str_split(participant_char, ", ")
      ) %>% 
      unnest(participant_char)
  ) %>% 
  filter(  ## Remove irrelevant (not analysed, none, NAs)
    !(participant_char %in% c("Not analysed", "None"))
    & !is.na(participant_char)
  )

# Get unique participant characteristics, save for manual tidying
get_participant_chars_unique <- get_participant_chars %>% 
  distinct(participant_char)

#write_csv(
#  get_participant_chars_unique, 
#  "Draft_2/Scratch_data/toTidy_unique_participant_chars.csv"
#)

# Import tidied participant characteristics, join to extract
get_participant_chars_tidied <- get_participant_chars %>% 
  left_join(
    read_csv("Draft_2/Scratch_data/tidied_unique_participant_chars.csv")
  ) %>% 
  arrange(ref)

# Get associated and unassociated trial characteristics in each study
get_trial_chars <- import_trials %>% 
  select(ref = RefID, trial_char = `Associated TC`) %>% 
  mutate(outcome = "associated") %>% 
  full_join(  ## Same workflow as participant characteristics
    import_trials %>% 
      select(ref = RefID, trial_char = `Unassociated TC`) %>% 
      mutate(
        outcome = "unassociated",
        trial_char = str_split(trial_char, ", ")
      ) %>% 
      unnest(trial_char)
  ) %>% 
  filter(
    !(trial_char %in% c("Not analysed", "None"))
    & !is.na(trial_char)
  )

# Get unique trial characteristics, save for manual tidying
get_trial_chars_unique <- get_trial_chars %>% 
  distinct(trial_char)

#write_csv(
#  get_trial_chars_unique, 
#  "Draft_2/Scratch_data/toTidy_unique_trial_chars.csv"
#)

# Import tidied participant characteristics, join to extract
get_trial_chars_tidied <- get_trial_chars %>% 
  left_join(
    read_csv("Draft_2/Scratch_data/tidied_unique_trial_chars.csv")
  ) %>% 
  arrange(ref)

# Get unique attrition outcomes, save for manual tidying
get_attrition_oms_unique <- import_studies %>% 
  select(`Attrition outcome`) %>% 
  distinct()

write_csv(
  get_attrition_oms_unique, 
  "Draft_2/Scratch_data/toTidy_unique_attrition_oms.csv"
)

# Import tidied attrition outcomes, join to extract
get_attrition_oms_tidied <- get_attrition_oms_unique %>% 
  left_join(
    read_csv("Draft_2/Scratch_data/tidied_unique_attrition_oms.csv")
  )

# Save lookups
#write_csv(get_authors_years_tidied, "Draft_2/Data/Lookups/lookup_authors.csv")
#write_csv(get_conditions_tidied, "Draft_2/Data/Lookups/lookup_conditions.csv")
#write_csv(get_participant_chars_tidied, "Draft_2/Data/Lookups/lookup_participant_chars.csv")
#write_csv(get_trial_chars_tidied, "Draft_2/Data/Lookups/lookup_trial_chars.csv")
#write_csv(get_attrition_oms_tidied, "Draft_2/Data/Lookups/lookup_attrition_oms.csv")


## Prepare data for analysis ---------------------------------------------------

# Join lookups to relevant variables
prepare_studies <- get_authors_years_tidied %>% 
  left_join(
    import_studies %>% 
      select(ref = RefID, Setting, `Attrition outcome`:`Dropout value`)
  ) %>% 
  left_join(
    get_conditions_tidied,
    by = "Setting"
  ) %>% 
  left_join(
    get_attrition_oms_tidied,
    by = "Attrition outcome"
  ) %>% 
  select(-Setting) %>% 
  rename(
    attrition_outcome = `Attrition outcome`,
    attrition_outcome_def = `Attrition outcome definition`,
    attrition_analysis = `Attrition related analysis`,
    effect_signif = `Measures of effect and significance`,
    n_rcts = `Number of RCTs`,
    participants_unit = `Participants unit`,
    participants_study = `Participants value`,
    attrition_measure = `Dropout measure`,
    attrition_stat = `Dropout statistic`,
    attrition_study = `Dropout value`,
    attrition_outcome_tidy = tidy_attrition_outcome,
    attrition_outcome_group = tidy_attrition_outcome_group
  ) %>% 
  select(
    c(
      ref:author_year, contains("attrition_outcome"), everything()
    )
  ) %>% 
  arrange(condition_group2)

prepare_participants <- get_authors_years_tidied %>% 
  left_join(
    get_participant_chars_tidied %>% 
      mutate(
        outcome = if_else(outcome == "associated", 1, 0)
      )
  ) %>% 
  filter(!is.na(outcome))

prepare_trials <- get_authors_years_tidied %>% 
  left_join(
    get_trial_chars_tidied %>% 
      mutate(
        outcome = if_else(outcome == "associated", 1, 0)
      )
  ) %>% 
  filter(!is.na(outcome))

prepare_ramstar <- import_ramstar %>% 
  rename(
    ref = RefID,
    q01_design = "1. Was an \"a priori\" design provided?",
    q02_selection_extraction = "2. Was there duplicate study selection and data extraction?",
    q03_search = "3. Was a comprehensive literature search performed?",
    q04_status = "4. Was the status of publication (i.e. grey literature) used as an inclusion criteria?",
    q05_list_studies = "5. Was a list of studies (included and excluded) provided?",
    q06_chars = "6. Were the characteristics of the included studies provided?",
    q07_qual_assess = "7. Was the scientific quality of the included studies assessed and documented?",
    q08_qual_conclu = "8. Was the scientific quality of the included studies used appropriate in formulating conclusions?",
    q09_combine = "9. Were the methods used to combine the findings of studies appropriate?",
    q10_pub_bias = "10. Was the likelihood of publication bias assessed?",
    q11_coi = "11. Was the conflict of interest included?",
    total = "Total score"
  ) %>% 
  select(-Note) %>% 
  pivot_longer(
    c(q01_design:total),
    names_to = "domain",
    values_to = "score"
  )

# Save prepared data
#write_csv(prepare_studies, "Draft_2/Data/prepared_studies.csv")
#write_csv(prepare_participants, "Draft_2/Data/prepared_participant_chars.csv")
#write_csv(prepare_trials, "Draft_2/Data/prepared_trial_chars.csv")
#write_csv(prepare_ramstar, "Draft_2/Data/prepared_ramstar.csv")






