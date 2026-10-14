# 1. Load Data & Standardize Column Names --------------------------------------
setwd("C:/Users/Lenovo/Documents/R/Sasu")
df <- read.csv("sv_diet.csv", check.names = FALSE) %>%
  rename(Season = Seasons) %>%        # Fix plural 'Seasons' -> 'Season'
  select(-any_of("Sl.No"))            # Exclude row index column from analysis

# Define metadata and dietary component columns
meta_cols <- c("Species", "Season")
diet_cols <- setdiff(colnames(df), meta_cols)

# Create unique row identifier
df <- df %>%
  mutate(RowID = paste(Species, Season, sep = " - "))

library(tidyverse)
library(vegan)
library(ggdendro)
library(patchwork)


# Set working directory (Update if necessary)
setwd("C:/Users/Lenovo/Documents/R/Sasu")

# 1. Load & Prepare Data ------------------------------------------------------
df <- read.csv("sv_diet.csv", check.names = FALSE) %>%
  rename(Season = Seasons) %>%
  select(-any_of("Sl.No"))

meta_cols <- c("Species", "Season")
diet_cols <- setdiff(colnames(df), meta_cols)

# Remove rows where all dietary values sum to 0
df_clean <- df %>%
  filter(rowSums(select(., all_of(diet_cols))) > 0)

# 2. Species-Level Clustering -------------------------------------------------
# Calculate the mean dietary composition per species for the dendrogram
df_species <- df_clean %>%
  group_by(Species) %>%
  summarise(across(all_of(diet_cols), mean))

# Calculate Bray-Curtis distance matrix on species averages
diet_matrix <- as.matrix(df_species[, diet_cols])
rownames(diet_matrix) <- df_species$Species

bc_dist <- vegdist(diet_matrix, method = "bray")
hc <- hclust(bc_dist, method = "average") # UPGMA clustering

dend_data <- dendro_data(hc)
dend_segments <- dend_data$segments

# Get species order from dendrogram leaves (1 to N)
ordered_species <- dend_data$labels$label
max_y <- length(ordered_species)

# 3. Y-Coordinate Mapping for Seasons -----------------------------------------
# Assign a base y-coordinate (1, 2, 3...) for each species leaf
y_base_df <- data.frame(
  Species = ordered_species,
  y_base = seq_along(ordered_species)
)

# Map the seasonal rows around the species' base leaf position.
# Increased offset to 0.3 so the stacked bars look thicker and touch vertically.
df_plot <- df_clean %>%
  left_join(y_base_df, by = "Species") %>%
  mutate(
    y_pos = case_when(
      Season == "Monsoon"      ~ y_base + 0.3,
      Season == "Post.Monsoon" ~ y_base,
      Season == "Pre.Monsoon"  ~ y_base - 0.3,
      TRUE ~ y_base
    )
  )

# Convert to long format and drop 0% rows for cleaner plotting
df_long <- df_plot %>%
  pivot_longer(cols = all_of(diet_cols), names_to = "Component", values_to = "Percentage") %>%
  mutate(Component = factor(Component, levels = diet_cols)) %>%
  filter(Percentage > 0)

# Color Palettes
season_colors <- c(
  "Monsoon" = "#1f77b4", "Post.Monsoon" = "#2ca02c", "Pre.Monsoon" = "#d62728"
)

diet_colors <- c(
  "Algae" = "#31a354", "Copepods" = "#6baed6", "Amphipods" = "#1f77b4",
  "Isopods" = "#f768a1", "Shrimp" = "#ff7f0e", "Crab" = "#ffff33",
  "Clams" = "#c5b0d5", "Fish" = "#d62728", "Detritus" = "#d9d9d9",
  "FishEggs" = "#8c564b", "Polychaete" = "#7b4173"
)

# 4. Build Panel A: Dendrogram (Top Axis) -------------------------------------
p_dend <- ggplot() +
  geom_segment(data = dend_segments, aes(x = y, y = x, xend = yend, yend = xend), linewidth = 0.5) +
  geom_vline(xintercept = 0.6, linetype = "dashed", color = "black", linewidth = 0.4) +
  scale_x_reverse(limits = c(1.0, -0.05), breaks = seq(1.0, 0, by = -0.2), position = "top") +
  scale_y_continuous(limits = c(0.4, max_y + 0.8), expand = c(0, 0)) +
  labs(x = "Bray–Curtis dissimilarity") +
  theme_classic() +
  theme(
    axis.title.y = element_blank(), axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), axis.line.y = element_blank(),
    axis.title.x.top = element_text(size = 10, face = "bold", margin = margin(b = 10)),
    axis.text.x.top = element_text(size = 9, color = "black"),
    plot.margin = margin(t = 20, r = 0, b = 20, l = 10)
  )

# 5. Build Panel B: Species & Season Sidebar ----------------------------------
p_meta <- ggplot() +
  annotate("text", x = 0, y = max_y + 0.6, label = "Species", hjust = 0, fontface = "bold", size = 3.5) +
  annotate("text", x = 3.5, y = max_y + 0.6, label = "Season", hjust = 0, fontface = "bold", size = 3.5) +
  geom_text(data = y_base_df, aes(x = 0, y = y_base, label = Species), hjust = 0, size = 3.5, fontface = "italic") +
  # Match height to the bar width (0.3)
  geom_tile(data = df_plot, aes(x = 3.5, y = y_pos, fill = Season), width = 0.35, height = 0.3, color = "white", linewidth = 0.1) +
  geom_text(data = df_plot, aes(x = 3.8, y = y_pos, label = Season), hjust = 0, size = 3) +
  scale_fill_manual(values = season_colors) +
  scale_x_continuous(limits = c(0, 6.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.4, max_y + 0.8), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none")

# 6. Build Panel C: Stacked Bars (Top Axis) -----------------------------------
p_bar <- ggplot(df_long, aes(y = y_pos, x = Percentage, fill = Component)) +
  # ADDED orientation = "y" to force horizontal plotting on dual-continuous axes
  geom_col(position = "stack", width = 0.3, color = "black", linewidth = 0.1, orientation = "y") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 101), breaks = seq(0, 100, by = 20), position = "top") +
  scale_y_continuous(limits = c(0.4, max_y + 0.8), expand = c(0, 0)) +
  scale_fill_manual(values = diet_colors) +
  labs(x = "Relative dietary composition (%)") +
  theme_classic() +
  theme(
    axis.title.y = element_blank(), axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), axis.line.y = element_blank(),
    axis.title.x.top = element_text(size = 10, face = "bold", margin = margin(b = 10)),
    axis.text.x.top = element_text(size = 9, color = "black"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    plot.margin = margin(t = 20, r = 10, b = 20, l = 0)
  ) +
  guides(fill = guide_legend(title = "Dietary components", ncol = 1))

# 7. Assemble & Export Figure -------------------------------------------------
composite_plot <- p_dend + p_meta + p_bar + 
  plot_layout(widths = c(1, 1.2, 2.5)) +
  plot_annotation(
    title = "Hierarchical clustering of dietary composition (Bray–Curtis dissimilarity)\nacross species and seasons",
    theme = theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5, margin = margin(b = 20))
    )
  )

print(composite_plot)

ggsave("Dietary_Clustering_Final.png", composite_plot, width = 14, height = 9, dpi = 300)