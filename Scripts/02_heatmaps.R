
# Load script for packages, functions, etc.
source("Draft_2/Scripts/00_packages_functions_etc.R")

# Load data extracts
import_studies <- read_csv("Draft_2/Data/prepared_studies.csv")
import_participants <- read_csv("Draft_2/Data/prepared_participant_chars.csv")
import_trials <- read_csv("Draft_2/Data/prepared_trial_chars.csv")

# Variables for ordering heatmap
ordered_authors <- import_studies %>% 
  arrange(condition_group2, condition_group1) %>% 
  select(author_year, condition_group1) %>% 
  distinct()
var_order_authors <- unique(ordered_authors$author_year)

ordered_participant_chars <- import_participants %>% 
  arrange(participant_char_group, tidy_participant_char)
var_order_prts <- unique(ordered_participant_chars$tidy_participant_char)

ordered_trial_chars <- import_trials %>% arrange(trial_char_group, tidy_trial_char_heatmap)
var_order_trials <- unique(ordered_trial_chars$tidy_trial_char_heatmap)

var_order_findings <- c("1", "0", NA)

# Variable to split trial heatmaps in two (1-43, 44-87)
var_trials_plot1 <- var_order_authors[1:43]
var_trials_plot2 <- var_order_authors[44:88]

# Save ordered authors for labelling heatmaps
write_csv(ordered_authors, "Draft_2/Data/Lookups/lookup_authors_conditions.csv")

## Heatmaps --------------------------------------------------------------------

# Participant characteristic data prepared for heatmap
data_heatmap_participant <- import_participants %>%
  select(author_year, tidy_participant_char, outcome) %>% 
  distinct() %>% 
  group_by(author_year, tidy_participant_char) %>% 
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
  select(-count, -outcome) %>% 
  complete(  # Add all characteristics to each study
    author_year, 
    tidy_participant_char
  ) %>%
  mutate(
    author_year = 
      factor(
        author_year, 
        levels = var_order_authors
      ),
    tidy_participant_char =
      factor(
        tidy_participant_char,
        levels = var_order_prts
      ),
    associated =
      factor(
        associated,
        levels = var_order_findings
      )
  )

# Participant characteristic heatmap
heatmap_participants <- ggplot(
  data_heatmap_participant,
  aes(
    x = ordered(author_year, var_order_authors),
    y = fct_rev(tidy_participant_char),
    fill = associated
  )
) +
  geom_tile(colour = "black") +
  labs(x = NULL, y = NULL, fill = "Findings") +
  scale_fill_manual(
    values = c("#4daf4a", "#e41a1c", "white"),
    breaks = c(1, 0, NA),
    labels = c("Associated", "Not associated", "Not evaluated"),
    na.value = "white") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x =
      element_text(
        size = 12,
        angle = 45,
        hjust = 1,
        colour = "white"
      ),
    axis.text.y =
      element_text(
        size = 12,
        colour = "black"
      ),
    legend.title =
      element_text(
        size = 12,
        face = "bold",
        colour = "black"
      ),
    legend.text =
      element_text(
        size = 12,
        colour = "black"
      ),
    aspect.ratio = 1
  )

heatmap_participants

# Trial characteristic data prepared for heatmap
data_heatmap_trial <- import_trials %>%
  select(author_year, tidy_trial_char_heatmap, outcome) %>% 
  distinct() %>% 
  group_by(author_year, tidy_trial_char_heatmap) %>% 
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
  select(-count, -outcome) %>% 
  complete(author_year, tidy_trial_char_heatmap) %>% 
  mutate(
    author_year = 
      factor(
        author_year, 
        levels = var_order_authors
      ),
    tidy_trial_char_heatmap =
      factor(
        tidy_trial_char_heatmap,
        levels = var_order_trials
      ),
    associated =
      factor(
        associated,
        levels = var_order_findings
      )
  )

# Split heatmap data for trials in two for plotting
data_heatmap_trial_1 <- data_heatmap_trial %>% 
  filter(author_year %in% var_trials_plot1) %>% 
  mutate(
    author_year = 
      ordered(author_year, var_order_authors)
  )

data_heatmap_trial_2 <- data_heatmap_trial %>% 
  filter(author_year %in% var_trials_plot2) %>% 
  mutate(
    author_year = 
      ordered(author_year, var_order_authors)
  )

# Trial characteristic heatmap #1 (reviews 1-43)
heatmap_trials_1 <- ggplot(
  data_heatmap_trial_1,
  aes(
    x = author_year,
    y = fct_rev(tidy_trial_char_heatmap),
    fill = associated
  )
) +
  geom_tile(colour = "black") +
  labs(x = NULL, y = NULL, fill = "Findings") +
  scale_fill_manual(
    values = c("#4daf4a", "#e41a1c", "white"),
    breaks = c(1, 0, NA),
    labels = c("Associated", "Not associated", "Not evaluated"),
    na.value = "white") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x =
      element_text(
        size = 12,
        angle = 45,
        hjust = 1,
        colour = "white"
      ),
    axis.text.y =
      element_text(
        size = 12,
        colour = "black"
      ),
    legend.title =
      element_text(
        size = 12,
        face = "bold",
        colour = "black"
      ),
    legend.text =
      element_text(
        size = 12,
        colour = "black"
      ),
    aspect.ratio = 1
  )

heatmap_trials_1

# Trial characteristic heatmap #2 (reviews 44-48)
heatmap_trials_2 <- ggplot(
  data_heatmap_trial_2,
  aes(
    x = author_year,
    y = fct_rev(tidy_trial_char_heatmap),
    fill = associated
  )
) +
  geom_tile(colour = "black") +
  labs(x = NULL, y = NULL, fill = "Findings") +
  scale_fill_manual(
    values = c("#4daf4a", "#e41a1c", "white"),
    breaks = c(1, 0, NA),
    labels = c("Associated", "Not associated", "Not evaluated"),
    na.value = "white") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x =
      element_text(
        size = 12,
        angle = 45,
        hjust = 1,
        colour = "white"
      ),
    axis.text.y =
      element_text(
        size = 12,
        colour = "black"
      ),
    legend.title =
      element_text(
        size = 12,
        face = "bold",
        colour = "black"
      ),
    legend.text =
      element_text(
        size = 12,
        colour = "black"
      ),
    aspect.ratio = 1
  )

heatmap_trials_2

# Save heatmaps
ggsave(
  "Draft_2/Output/Plots/heatmap_participants_web.pdf",
  heatmap_participants,
  width = 3600,
  height = 3000,
  dpi = 300,
  units = "px"
)

ggsave(
  "Draft_2/Output/Plots/heatmap_participants_final.pdf",
  heatmap_participants,
  width = 255,
  height = 204,
  dpi = 300,
  units = "mm"
)

ggsave(
  "Draft_2/Output/Plots/heatmap_trials_1_web.pdf",
  heatmap_trials_1,
  width = 3600,
  height = 3000,
  dpi = 300,
  units = "px"
)

ggsave(
  "Draft_2/Output/Plots/heatmap_trials_1_final.pdf",
  heatmap_trials_1,
  width = 340,
  height = 284,
  dpi = 300,
  units = "mm"
)

ggsave(
  "Draft_2/Output/Plots/heatmap_trials_2_web.pdf",
  heatmap_trials_2,
  width = 3600,
  height = 3000,
  dpi = 300,
  units = "px"
)

ggsave(
  "Draft_2/Output/Plots/heatmap_trials_2_final.pdf",
  heatmap_trials_2,
  width = 340,
  height = 284,
  dpi = 300,
  units = "mm"
)
