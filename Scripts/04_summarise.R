
# Load script for packages, functions, etc.
source("Draft_2/Scripts/00_packages_functions_etc.R")

# Load data extracts
import_studies <- read_csv("Draft_2/Data/prepared_studies.csv")
import_participants <- read_csv("Draft_2/Data/prepared_participant_chars.csv")
import_trials <- read_csv("Draft_2/Data/prepared_trial_chars.csv")

# Variables for calculations
var_n_studies <- length(unique(import_studies$ref))
var_studies_participants <- length(unique(import_participants$ref))
var_studies_trials <- length(unique(import_trials$ref))

## Introductory summaries ------------------------------------------------------

# Studies conducted since attrition was identified as priority
summarise_studies_year <- import_studies %>% 
  select(ref, author_year) %>% 
  mutate(
    year = as.numeric(str_extract(author_year, regex_year)),
    since_2014 = 
      case_when(
        year >= 2014 ~ "since",
        TRUE ~ "before"
      ),
  ) %>% 
  distinct() %>% 
  group_by(since_2014) %>% 
  reframe(
    count = n(),
    total = var_n_studies,
    percent = round((count/total) * 100, 2),
    value = paste0(count, "/", total, " (", percent, ")")
  ) %>% 
  arrange(desc(percent)) %>% 
  mutate_all(., as.character) %>% 
  pivot_longer(
    count:value,
    names_to = "summary",
    values_to = "value"
  )

tbl_studies_year <- summarise_studies_year %>% 
  filter(summary == "value") %>% 
  mutate(summary = "studies conducted since 2014") %>% 
  rename(category = since_2014)

# Attrition outcomes
summarise_attrition_oms <- import_studies %>% 
  select(ref, attrition_outcome_tidy) %>% 
  distinct() %>% 
  group_by(attrition_outcome_tidy) %>% 
  reframe(
    count = n(),
    total = var_n_studies,
    percent = round((count/total) * 100, 2),
    value = paste0(count, "/", total, " (", percent, ")")
  ) %>% 
  arrange(desc(percent)) %>% 
  mutate_all(., as.character) %>% 
  pivot_longer(
    count:value,
    names_to = "summary",
    values_to = "value"
  )

tbl_attrition_oms <- summarise_attrition_oms %>% 
  filter(summary == "value") %>% 
  mutate(summary = "attrition outcomes") %>% 
  rename(category = attrition_outcome_tidy)

# Number of RCTs
summarise_n_rcts <- import_studies %>% 
  select(ref, n_rcts) %>% 
  distinct() %>%  
  reframe(
    median_rcts = median(n_rcts, na.rm = TRUE),  # Range: 1, 559
    value = paste0(median_rcts, " (", 1, ", ", 559, ")")
  ) %>% 
  mutate_all(., as.character) %>% 
  pivot_longer(
    everything(),
    names_to = "summary",
    values_to = "value"
  )

tbl_n_rcts <- summarise_n_rcts %>% 
  filter(summary == "value") %>% 
  mutate(
    category = "central tendency",
    summary = "median (range) rcts"
  )

# Number of participants - N=63 reported a total number
summarise_n_participants <- import_studies %>% 
  select(ref, contains("participants")) %>% 
  mutate(
    participants_study =
      case_when(
        participants_unit != "N" ~ NA,
        grepl(">", participants_study) ~ gsub(">", "", participants_study),
        TRUE ~ participants_study
      ),
    participants_study = as.numeric(participants_study)
  ) %>% 
  filter(!is.na(participants_study)) %>% 
  select(ref, participants_study) %>% 
  distinct() %>% 
  reframe(
    median_n = median(participants_study, na.rm = TRUE),  # Range: 36, 131836
    value = paste0(median_n, " (", 36, ", ", 131836, ")")
  ) %>% 
  mutate_all(., as.character) %>% 
  pivot_longer(
    everything(),
    names_to = "summary",
    values_to = "value"
  )

tbl_n_participants <- summarise_n_participants %>% 
  filter(summary == "value") %>% 
  mutate(category = "central tendency",
         summary = "median (range) participants")

# Combining introductory summaries
tbl_intro_summary <- rbind(
  tbl_n_rcts,
  tbl_n_participants,
  tbl_attrition_oms,
  tbl_studies_year
)

## Conditions and characteristics ----------------------------------------------

# Number of conditions studied
tbl_conditions <- import_studies %>% 
  select(ref, contains("condition_group")) %>% 
  distinct() %>% 
  left_join(
    import_studies %>% 
      select(ref, contains("condition_group")) %>% 
      distinct() %>% 
      group_by(condition_group2) %>%
      reframe(
        group_studies = length(ref),
        group_pcnt = round((group_studies/var_n_studies) * 100, 2),
        group_value = paste0(group_studies, "/", var_n_studies, " (", group_pcnt, ")")
      )
  ) %>% 
  left_join(
    import_studies %>% 
      select(ref, contains("condition_group")) %>% 
      distinct() %>% 
      group_by(condition_group1) %>% 
      reframe(
        cond_studies = length(ref)
      )
  ) %>% 
  mutate(
    cond_pcnt = round((cond_studies/group_studies) * 100, 2),
    cond_value = paste0(cond_studies, "/", group_studies, " (", cond_pcnt, ")")
  ) %>% 
  select(-ref) %>% 
  distinct() %>% 
  select(
    condition_group2, group_studies, group_pcnt, group_value, everything()
  ) %>% 
  arrange(condition_group2, desc(cond_studies))

# Number of participant characteristics studied and associations
tbl_participant_chars <- import_participants %>% 
  select(ref, tidy_participant_char, participant_char_group) %>% 
  distinct() %>% 
  left_join(
    import_participants %>% 
      select(ref, participant_char_group) %>% 
      distinct() %>% 
      group_by(participant_char_group) %>%
      reframe(
        group_studies = length(ref),
        group_pcnt = round((group_studies/var_studies_participants) * 100, 2),
        group_value = paste0(group_studies, "/", var_studies_participants, " (", group_pcnt, ")")
      )
  ) %>% 
  left_join(
    import_participants %>% 
      select(ref, tidy_participant_char, participant_char_group) %>% 
      distinct() %>% 
      group_by(tidy_participant_char) %>% 
      reframe(
        cond_studies = length(ref)
      )
  ) %>% 
  left_join(
    import_participants %>% 
      select(ref, tidy_participant_char, participant_char_group, outcome) %>% 
      distinct() %>% 
      group_by(tidy_participant_char) %>% 
      reframe(
        assc_studies = sum(outcome == 1),
        unassc_studies = sum(outcome == 0)
      )
  ) %>%
  mutate(
    cond_pcnt = round((cond_studies/group_studies) * 100, 2),
    cond_value = paste0(cond_studies, "/", group_studies, " (", cond_pcnt, ")"),
    assc_pcnt = round((assc_studies/cond_studies) * 100, 2),
    assc_value = paste0(assc_studies, "/", cond_studies, " (", assc_pcnt, ")"),
    unassc_pcnt = round((unassc_studies/cond_studies) * 100, 2),
    unassc_value = paste0(unassc_studies, "/", cond_studies, " (", unassc_pcnt, ")")
  ) %>% 
  select(-ref) %>% 
  distinct() %>% 
  select(
    participant_char_group, group_studies, group_pcnt, group_value, everything()
  ) %>% 
  arrange(participant_char_group, desc(cond_studies))

# Number of participants characteristics studied per condition
summary_conditions_participants <- import_studies %>% 
  select(ref, condition_group1) %>% 
  distinct() %>% 
  left_join(
    tbl_conditions %>% 
      select(condition_group1, cond_studies)
  ) %>% 
  left_join(
    import_participants %>% 
      select(ref, tidy_participant_char, outcome) %>% 
      distinct()
  ) %>% 
  filter(!is.na(outcome))

tbl_conditions_participants <- summary_conditions_participants %>% 
  left_join(
    summary_conditions_participants %>% 
      group_by(condition_group1, tidy_participant_char) %>% 
      reframe(
        studies = n(),
        assc = sum(outcome == 1),
        unassc = sum(outcome == 0),
      )
  ) %>% 
  select(-ref, -outcome) %>% 
  distinct() %>% 
  mutate(
    studies_pcnt = round((studies/cond_studies) * 100, 2),
    studies_value = paste0(studies, "/", cond_studies, " (", studies_pcnt, ")"),
    assc_pcnt = round((assc/studies) * 100, 2),
    assc_value = paste0(assc, "/", studies, " (", assc_pcnt, ")"),
    unassc_pcnt = round((unassc/studies) * 100, 2),
    unassc_value = paste0(unassc, "/", studies, " (", unassc_pcnt, ")")
  ) %>% 
  select(
    condition_group1:studies, studies_pcnt, studies_value,
    assc, assc_pcnt, assc_value, everything()
  ) %>% 
  arrange(condition_group1, desc(studies))

# Number of trial characteristics studied
tbl_trial_chars <- import_trials %>% 
  select(ref, tidy_trial_char_tabular, trial_char_group) %>% 
  distinct() %>% 
  left_join(
    import_trials %>% 
      select(ref, trial_char_group) %>% 
      distinct() %>% 
      group_by(trial_char_group) %>%
      reframe(
        group_studies = length(ref),
        group_pcnt = round((group_studies/var_studies_trials) * 100, 2),
        group_value = paste0(group_studies, "/", var_studies_trials, " (", group_pcnt, ")")
      )
  ) %>% 
  left_join(
    import_trials %>% 
      select(ref, tidy_trial_char_tabular, trial_char_group) %>% 
      distinct() %>% 
      group_by(tidy_trial_char_tabular) %>% 
      reframe(
        cond_studies = length(ref)
      )
  ) %>% 
  left_join(
    import_trials %>% 
      select(ref, tidy_trial_char_tabular, trial_char_group, outcome) %>% 
      distinct() %>% 
      group_by(ref, tidy_trial_char_tabular) %>% 
      mutate(
        count = n(),
        associated = 
          case_when(
            count > 1 & outcome == 1 ~ 1,
            count > 1 & outcome == 0 ~ NA,
            TRUE ~ outcome
          )
      ) %>% 
      ungroup() %>% 
      filter(!is.na(associated)) %>% 
      group_by(tidy_trial_char_tabular) %>% 
      reframe(
        assc_studies = sum(associated == 1),
        unassc_studies = sum(associated == 0)
      )
  ) %>%
  mutate(
    cond_pcnt = round((cond_studies/group_studies) * 100, 2),
    cond_value = paste0(cond_studies, "/", group_studies, " (", cond_pcnt, ")"),
    assc_pcnt = round((assc_studies/cond_studies) * 100, 2),
    assc_value = paste0(assc_studies, "/", cond_studies, " (", assc_pcnt, ")"),
    unassc_pcnt = round((unassc_studies/cond_studies) * 100, 2),
    unassc_value = paste0(unassc_studies, "/", cond_studies, " (", unassc_pcnt, ")")
  ) %>% 
  select(-ref) %>% 
  distinct() %>% 
  select(
    trial_char_group, tidy_trial_char_tabular, contains("_value"), everything()
  ) %>% 
  arrange(trial_char_group, desc(cond_studies))

# Number of trial characteristics studied per condition
summary_conditions_trials <- import_studies %>% 
  select(ref, condition_group1) %>% 
  distinct() %>% 
  left_join(
    tbl_conditions %>% 
      select(condition_group1, contains("cond_"))
  ) %>% 
  left_join(
    import_trials %>% 
      select(ref, tidy_trial_char_tabular, outcome) %>% 
      distinct()
  ) %>% 
  filter(!is.na(outcome))

tbl_conditions_trials <- summary_conditions_trials %>% 
  left_join(
    summary_conditions_trials %>% 
      group_by(condition_group1, tidy_trial_char_tabular) %>% 
      reframe(
        studies = n(),
        assc = sum(outcome == 1),
        unassc = sum(outcome == 0),
      )
  ) %>% 
  select(-ref, -outcome) %>% 
  distinct() %>% 
  mutate(
    studies_pcnt = round((studies/cond_studies) * 100, 2),
    studies_value = paste0(studies, "/", cond_studies, " (", studies_pcnt, ")"),
    assc_pcnt = round((assc/studies) * 100, 2),
    assc_value = paste0(assc, "/", studies, " (", assc_pcnt, ")"),
    unassc_pcnt = round((unassc/studies) * 100, 2),
    unassc_value = paste0(unassc, "/", studies, " (", unassc_pcnt, ")")
  ) %>% 
  select(
    condition_group1:studies, studies_pcnt, studies_value,
    assc, assc_pcnt, assc_value, everything()
  ) %>% 
  arrange(condition_group1, desc(studies)) %>% 
  left_join(
    import_trials %>% 
      select(
        tidy_trial_char_tabular, tidy_trial_char_heatmap
      ) %>% 
      distinct()
  )


## Study characteristics -------------------------------------------------------

# Get original participant and trial extracts
import_orig_participants <- read_csv("Draft_2/Data/Extracts/extract_participants.csv")
import_orig_trials <- read_csv("Draft_2/Data/Extracts/extract_trials.csv")

# Main characteristics table
tbl_studies_main <- import_studies %>% 
  select(
    author_year,
    condition_group1,
    n_rcts,
    participants_unit,
    participants_study,
    attrition_outcome_tidy,
    attrition_measure,
    attrition_stat,
    attrition_study
  ) %>% 
  group_by(author_year) %>% 
  mutate(
    across(
      everything(),
      ~ as.character(.)
    ),
    participants_unit =
      if_else(
        participants_unit == "Median yoga group size (N)",
        "Median group size (N)",
        participants_unit
      ),
    across(
      c(participants_unit, participants_study),
      ~ case_when(
        . != "NR" ~ paste0(participants_unit, " - ", participants_study, collapse = "; "),
        TRUE ~ .
      )
    ),
    across(
      c(attrition_stat, attrition_study),
      ~ case_when(
        . != "NR" ~ paste0(attrition_stat, " - ", attrition_study, collapse = "; "),
        TRUE ~ .
      )
    ),
    across(
      everything(),
      ~ case_when(
        length(.) > 1 
        & !is.na(.) ~ paste0(., collapse = "; "),
        TRUE ~ .
      )
    ),
    across(
      everything(),
      ~ case_when(
        length(.) > 1 & !is.na(.) ~ {
          values <- strsplit(., "; ")[[1]]
          if(length(values) > 1 && values[1] == values[2]) {
            values[1]
          } else {
            paste(values, collapse = "; ")
          }
        },
        TRUE ~ as.character(.)
      )
    ),
    across(
      everything(),
      ~ if_else(. == "NR" | is.na(.), "Not reported", .)
    )
  ) %>% 
  distinct() %>% 
  ungroup() %>% 
  arrange(author_year)

# Supplementary studies table for participant characteristics evaluated
tbl_studies_supp_participants <- import_studies %>% 
  select(
    ref,
    author_year,
    attrition_analysis
  ) %>% 
  left_join(
    import_orig_participants %>% 
      select(
        ref = RefID,
        assc = `Associated PC`,
        assc_findings = `Associated PC findings`,
        assc_measures = `Associated PC statistic`,
        assc_analysis = `Associated PC analysis`,
        assc_values = `Associated PC value`,
        unassc = `Unassociated PCs`
      )
  ) %>% 
  mutate(
    assc_measure = 
      case_when(
        !(assc_values %in% c("None", "Not analysed")) ~ paste0(assc_measures, " - ", assc_values),
        TRUE ~ assc_values
      )
  ) %>% 
  distinct() %>% 
  select(
    -assc_values, -assc_measures
  ) %>% 
  select(
    ref:attrition_analysis, contains("assc_"), everything()
  ) %>% 
  arrange(author_year)

# Supplementary studies table for trial characteristics evaluated
tbl_studies_supp_trials <- import_studies %>% 
  select(
    ref,
    author_year,
    attrition_analysis
  ) %>% 
  left_join(
    import_orig_trials %>% 
      select(
        ref = RefID,
        assc = `Associated TC`,
        assc_findings = `Associated TC findings`,
        assc_measures = `Associated TC statistic`,
        assc_analysis = `Associated TC analysis`,
        assc_values = `Associated TC value`,
        unassc = `Unassociated TC`
      )
  ) %>% 
  mutate(
    assc_measure = 
      case_when(
        !(assc_values %in% c("None", "Not analysed")) ~ paste0(assc_measures, " - ", assc_values),
        TRUE ~ assc_values
      ),
    unassc = if_else(is.na(unassc), "None", unassc)
  ) %>% 
  distinct() %>% 
  select(
    -assc_values, -assc_measures
  ) %>% 
  select(
    ref:attrition_analysis, contains("assc_"), everything()
  ) %>% 
  arrange(author_year)

## Certainty of evidence -------------------------------------------------------

# Import R-AMSTAR
import_ramstar <- read_csv("Draft_2/Data/prepared_ramstar.csv")

# Template for joining participant and trial chars
coe_template <- import_studies %>% 
  select(ref, author_year, condition_group1) %>% 
  distinct() %>% 
  left_join(
    import_ramstar %>% 
      filter(domain == "total") %>% 
      select(-domain) %>% 
      rename(ramstar = score)
  ) %>% 
  mutate(
    mean = round(mean(ramstar)),
    sd = round(sd(ramstar)),
    grade = 
      case_when(
        ramstar >= mean + (2 * sd) ~ "A",
        ramstar < mean + (2 * sd) & ramstar >= mean + sd ~ "B",
        ramstar < mean + sd & ramstar >= mean ~ "C",
        ramstar < mean & ramstar >= mean - sd ~ "D",
        ramstar < mean - sd ~ "E"
      )
  ) %>% 
  select(ref:ramstar, grade)

# Grades to add to table
#write_csv(
#  coe_template %>% 
#    select(author_year, grade) %>% 
#    arrange(author_year),
#  "Draft_2/Output/Summary_statistics/ramstar_grade.csv"
#)

# Participant characteristics variables
coe_participants <- coe_template %>% 
  inner_join(
    import_participants %>% 
      select(
        ref, author_year, char = tidy_participant_char, outcome
      ) %>% 
      distinct()
  )

# Aggregate findings
coe_participants_agg <- coe_participants %>% 
  group_by(condition_group1, char) %>% 
  reframe(
    n_evaluated = n(),
    n_assc = sum(outcome == 1)
  )

# Categorise certainty
coe_participants_cat <- coe_participants_agg %>% 
  group_by(char) %>% 
  mutate(
    n_conditions = n(),
    multiple_conditions_reviews = 
      case_when(
        sum(n_conditions >= 3 & n_evaluated >= 3) >= 3 ~ 1,
        TRUE ~ 0
      ),
    multiple_conditions = if_else(n_conditions >= 3, 1, 0),
    multiple_reviews = if_else(sum(n_evaluated) >= 3, 1, 0),
    assc_prop = round(sum(n_assc)/sum(n_evaluated), 2)
  ) %>% 
  ungroup() %>% 
  select(char:assc_prop) %>% 
  group_by(char) %>% 
  mutate(
    n_evaluated = sum(n_evaluated),
    n_assc = sum(n_assc)
  ) %>% 
  distinct() %>% 
  mutate(
    coe =
      case_when(
        multiple_conditions_reviews == 1
        & assc_prop >= 0.5 ~ "High",
        multiple_conditions == 1
        & multiple_reviews == 1
        & assc_prop >= 0.25 ~ "Moderate",
        assc_prop == 0 ~ "None",
        TRUE ~ "Low"
      )
  )

# Trial characteristics variables
coe_trial <- coe_template %>% 
  inner_join(
    import_trials %>% 
      select(
        ref, author_year, char = tidy_trial_char_tabular, outcome
      ) %>% 
      distinct()
  )

# Aggregate findings
coe_trial_agg <- coe_trial %>% 
  group_by(condition_group1, char) %>% 
  reframe(
    n_evaluated = n(),
    n_assc = sum(outcome == 1)
  )

# Categorise certainty
coe_trial_cat <- coe_trial_agg %>% 
  group_by(char) %>% 
  mutate(
    n_conditions = n(),
    multiple_conditions_reviews = 
      case_when(
        sum(n_conditions >= 3 & n_evaluated >= 3) >= 3 ~ 1,
        TRUE ~ 0
      ),
    multiple_conditions = if_else(n_conditions >= 3, 1, 0),
    multiple_reviews = if_else(sum(n_evaluated) >= 3, 1, 0),
    assc_prop = round(sum(n_assc)/sum(n_evaluated), 2)
  ) %>% 
  ungroup() %>% 
  select(char:assc_prop) %>% 
  group_by(char) %>% 
  mutate(
    n_evaluated = sum(n_evaluated),
    n_assc = sum(n_assc)
  ) %>% 
  distinct() %>% 
  mutate(
    coe =
      case_when(
        multiple_conditions_reviews == 1
        & assc_prop >= 0.5 ~ "High",
        multiple_conditions == 1
        & multiple_reviews == 1
        & assc_prop >= 0.25 ~ "Moderate",
        assc_prop == 0 ~ "None",
        TRUE ~ "Low"
      )
  )

# Save summary tables
write_csv(tbl_intro_summary, "Draft_2/Output/Summary_statistics/intro_summary.csv")
write_csv(tbl_conditions, "Draft_2/Output/Summary_statistics/conditions.csv")

write_csv(tbl_participant_chars, "Draft_2/Output/Summary_statistics/participant_chars.csv")
write_csv(tbl_conditions_participants, "Draft_2/Output/Summary_statistics/conditions_participants.csv")

write_csv(tbl_trial_chars, "Draft_2/Output/Summary_statistics/trial_chars.csv")
write_csv(tbl_conditions_trials, "Draft_2/Output/Summary_statistics/conditions_trials.csv")

write_csv(tbl_studies_main, "Draft_2/Output/Summary_statistics/study_chars_main.csv")
write_csv(tbl_studies_supp_participants, "Draft_2/Output/Summary_statistics/study_chars_supp_participants.csv")
write_csv(tbl_studies_supp_trials, "Draft_2/Output/Summary_statistics/study_chars_supp_trials.csv")

write_csv(coe_participants_cat, "../Analysis/Draft_2/Output/Summary_statistics/coe_participants.csv")
write_csv(coe_trial_cat, "../Analysis/Draft_2/Output/Summary_statistics/coe_trials.csv")








