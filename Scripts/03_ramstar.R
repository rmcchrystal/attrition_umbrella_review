
# Load script for packages, functions, etc.
source("Draft_2/Scripts/00_packages_functions_etc.R")

# Load data extracts
import_ramstar <- read_csv("Draft_2/Data/prepared_ramstar.csv")
import_studies <- read_csv("Draft_2/Data/Lookups/lookup_authors.csv")

## R-AMSTAR --------------------------------------------------------------------

# Variables for labelling domains in plots and ordering factor levels
var_domains <- as_labeller(
  c(
    q01_design = "1 - A priori design provided",
    q02_selection_extraction = "2 - Duplicate selection/extraction",
    q03_search = "3 - Comprehensive literature search",
    q04_status = "4 - Publication status in eligibility criteria",
    q05_list_studies = "5 - Listed included/excluded studies",
    q06_chars = "6 - Presented study characteristics",
    q07_qual_assess = "7 - Quality assessment performed",
    q08_qual_conclu = "8 - Quality informed findings/conclusions",
    q09_combine = "9 - Appropriately combined findings",
    q10_pub_bias = "10 - Assessed publication bias",
    q11_coi = "11 - Conflicts of interest stated",
    total = "R-AMSTAR total score"
  )
)

var_domains_levels <- c(
  "total",
  "q01_design",
  "q02_selection_extraction",
  "q03_search",
  "q04_status",
  "q05_list_studies",
  "q06_chars",
  "q07_qual_assess",
  "q08_qual_conclu",
  "q09_combine",
  "q10_pub_bias",
  "q11_coi"
)

# Joining author names and years for table
tbl_ramstar <- import_studies %>% 
  left_join(
    import_ramstar
  ) %>% 
  mutate(
    domain =
      case_when(
        domain == "q01_design" ~ "1 - A priori design provided",
        domain == "q02_selection_extraction" ~ "2 - Duplicate selection/extraction",
        domain == "q03_search" ~ "3 - Comprehensive literature search",
        domain == "q04_status" ~ "4 - Publication status in eligibility criteria",
        domain == "q05_list_studies" ~ "5 - Listed included/excluded studies",
        domain == "q06_chars" ~ "6 - Presented study characteristics",
        domain == "q07_qual_assess" ~ "7 - Quality assessment performed",
        domain == "q08_qual_conclu" ~ "8 - Quality informed findings/conclusions",
        domain == "q09_combine" ~ "9 - Appropriately combined findings",
        domain == "q10_pub_bias" ~ "10 - Assessed publication bias",
        domain == "q11_coi" ~ "11 - Conflicts of interest stated",
        domain == "total" ~ "R-AMSTAR total score"
      )
  ) %>% 
  pivot_wider(
    names_from = "domain",
    values_from = "score"
  )

# Save table
write_csv(tbl_ramstar, "Draft_2/Output/Summary_statistics/ramstar.csv")

# Create a facet of histograms for domain and total scores
histogram_data <- import_ramstar %>% 
  mutate(
    domain = 
      factor(
        domain, 
        levels = var_domains_levels
      ),
    type =  # Distinguish domain and total score levels
      if_else(
        grepl("q", domain), 
        "domains",
        "total"
      )
  )

ramstar_barchart <- histogram_data %>% 
  ggplot(
    aes(
      score,
      fill = type
    )
  ) +
  geom_bar(
    colour = "black",
    linewidth = 0.75,
    width = 1
  ) +
  geom_histogram(
    data = . %>% filter(domain == "total"),
    colour = "black",
    bins = 5,
    linewidth = 0.75
  ) +
  labs(
    x = "R-AMSTAR Score - Total (Maximum 44), Domains (1-4)",
    y = "Number of Studies",
    fill = NULL
  ) +
  facet_wrap(
    ~ domain,
    scales = "free",
    labeller = var_domains
  ) +
  facetted_pos_scales(  ## Set y-axis scales for total and domain scores
    y = list(
      grepl("q", domain) ~ scale_y_continuous(limits = c(0, 60), n.breaks = 6),
      domain == "total" ~ scale_y_continuous(limits = c(0, 40), n.breaks = 6)
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = 
      element_text(
        size = 10,
        face = "bold",
        colour = "black"
      ),
    axis.text =
      element_text(
        size = 10,
        colour = "black"
      ),
    strip.text =
      element_text(
        colour = "black",
        face = "bold"
      ),
    strip.background = 
      element_rect(
        colour = "black",
        linewidth = 1,
      ),
    panel.border = 
      element_rect(
        colour = "black",
        linewidth = 1
      )
  )

# Save density facets
ggsave(
  "Draft_2/Output/Plots/RAMSTAR_barchart.pdf",
  ramstar_barchart,
  width = 14,
  height = 8,
  dpi = 300
)

ggsave(
  "Draft_2/Output/Plots/RAMSTAR_barchart.png",
  ramstar_barchart,
  width = 14,
  height = 8,
  dpi = 300
)



