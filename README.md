# Participant and trial characteristics reported in predictive analyses of trial attrition: An umbrella review of systematic reviews of randomised controlled trials across multiple conditions
This repository contains the R scripts and extracted data that was used in the synthesis of this umbrella review. Folder contents are described below.

## Data
Contains the prepared data for synthesis, lookup tables and original extracts

prepared_participant_chars - Participant-level characteristic data prepared for synthesis
prepared_ramstar - R-AMSTAR quality appraisal data prepared for synthesis
prepared_studies - Included study characteristics prepared for synthesis
prepared_trial_chars - Trial-level characteristic data prepared for synthesis

Extracts - The original extracts separated into:
1. Extract_participants - Participant-level characteristic extracts
2. Extract_ramstar - R-AMSTAR quality appraisal
3. Extract_studies - Included study characteristics
4. Extract_trials - Trial-level characteristic extracts

Lookups - Lookup tables used by the analysis scripts
1. lookup_attrition_oms - Definitions of attrition outcomes, along with tidied versions
2. lookup_authors - Distiller reference ID and first author with year
3. lookup_authors_conditions - First author with year and condition studied
4. lookup_conditions - Original condition terms and groupings applied
5. lookup_participant_chars - Original characteristic terms and groupings applied
6. lookup_trial_chars - Original characteristic terms and groupings applied

## Scratch_data
Contains scratch data created for tidying and grouping data

grouped_unique_conditions - Grouping of conditions
tidied_unique_attrition_oms - Tidied definitions of attrition outcomes
tidied_unique_participant_chars - Tidied unique participant characteristics
tidied_unique_trial_chars - Tidied unique trial characteristics
toGroup_unique_conditions - Unique conditions prior to grouping
toTidy_unique_attrition_oms - Unique attrition outcome definitions to tidy
toTidy_unique_participant_chars - Unique participant characteristics to tidy
toTidy_unique_trial_chars - Unique trial characteristics to tidy

## Scripts
Scripts used for data preparation and synthesis

00_packages_functions_etc - Packages, functions and regex used for pattern finding
01_prepare_data - Data preparation
02_heatmaps - Heatmap generation
03_ramstar - R-AMSTAR summaries
04_summarise - Summarising data

 
