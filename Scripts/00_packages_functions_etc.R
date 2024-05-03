
library(tidyverse)
library(styler)
library(ggh4x)
library(ggprism)

## Regex for extracting first author surname for authors lookup ----------------

regex_first_author <- "^(.*?),.*$"
regex_remove_initials <- "\\b(?![,\\-])\\w\\."
regex_year <- "\\b\\d{4}\\b"
