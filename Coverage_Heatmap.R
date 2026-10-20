library(tidyverse) # work with data
library(ggpubr) # ggarrange, theme_pubclean

# Define theme -----------------------------------------------
palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#9999FF", "#33A02C", "#FB9A99",
  "#990066", "#9933FF", "#999933", "#FFCCFF", "#336600", "#999999", "#990000"
)
options(scipen = 999)

# Load data -----------------------------------------------
protocol_files <- c(
  "PIM" = "Master_Benthic_PIM_2010-2025.csv",
  "Inverts" = "Master_Benthic_Inverts_2010-2025.csv",
  "Recruits" = "Master_Benthic_Recruit_2023-2025.csv",
  "Coral Community" = "Master_Coral_Community_2010-2025.csv",
  "Fish" = "Master_Fish_Survey_2010-2025.csv"
)

# Prepare data -----------------------------------------------
coverage <- imap_dfr(protocol_files, function(f, name) {
  read_csv(f, col_types = cols(.default = "c"), progress = FALSE) %>%
    distinct(Year, Locality) %>%
    mutate(Survey = name, .before = 1)
}) %>%
  mutate(Year = as.integer(Year))
all_years <- sort(unique(coverage$Year))
all_localities <- sort(unique(coverage$Locality))
survey_levels <- names(protocol_files)

# Define helper -----------------------------------------------
make_heatmap <- function(data, x, y, x_levels, y_levels, title) {
  grid <- data %>%
    distinct(.data[[x]], .data[[y]]) %>%
    mutate(Covered = "Yes") %>%
    right_join(
      expand_grid(!!x := x_levels, !!y := y_levels),
      by = c(x, y)
    ) %>%
    mutate(
      Covered = replace_na(Covered, "No"),
      !!x := factor(.data[[x]], levels = x_levels),
      !!y := factor(.data[[y]], levels = rev(y_levels))
    )
  ggplot(grid, aes(x = .data[[x]], y = .data[[y]], fill = Covered)) +
    geom_tile(color = "black", linewidth = 0.6) +
    theme_pubclean() +
    labs(x = NULL, y = NULL, title = title) +
    scale_fill_manual(values = c("Yes" = "#898989", "No" = "white"), name = "Data present")
}

# Create figure -----------------------------------------------
A <- make_heatmap(coverage, "Year", "Survey", all_years, survey_levels, "A") +
  theme(legend.position = "none")
B <- make_heatmap(coverage, "Locality", "Survey", all_localities, survey_levels, "B") +
  theme(legend.position = "none")
C <- make_heatmap(coverage, "Year", "Locality", all_years, all_localities, "C")
fig_coverage <- ggarrange(ggarrange(A, B, ncol = 2), C + theme(legend.position = "bottom"), nrow = 2, heights = c(1, 1.1))
ggsave("fig_coverage.png", fig_coverage, width = 11, height = 13, dpi = 300)
