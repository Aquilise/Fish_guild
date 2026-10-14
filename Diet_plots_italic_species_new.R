# Original diet plots — fish scientific names italicized
setwd("C:/Users/Lenovo/Documents/R/Sasu")

library(ggplot2)
library(dplyr)
library(readr)
library(ggrepel)

data <- read.csv("Diet Overall compilation.csv", check.names = TRUE)
summary(data)

# 1. Stacked bar plot
p <- ggplot(data, aes(x = Species, y = X.IRI, fill = Prey)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic")) +
  labs(x = "Species", y = "%IRI", fill = "Prey") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c(
    "Crab" = "#E41A1C", "Shrimp" = "#377EB8", "Fish" = "#4DAF4A",
    "Clams" = "#984EA3", "Amphipods" = "#FF7F00", "Copepods" = "#FFFF33",
    "Detritus" = "#A65628", "Fish_eggs" = "#F781BF", "Isopods" = "#999999",
    "Polychaete" = "#66C2A5", "Algae" = "#00BFC4"
  ))
print(p)
ggsave("Stacked Bar Plot_italic.tiff", plot = p, width = 10, height = 6,
       dpi = 300, compression = "lzw")

# 2. Faceted stacked bar plot
p1 <- ggplot(data, aes(x = Species, y = X.IRI, fill = Prey)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Guild, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   size = 12, face = "italic")) +
  labs(x = "Species", y = "%IRI", fill = "Prey")
print(p1)
ggsave("Plot_2_new_italic.tiff", plot = p1, width = 10, height = 6,
       dpi = 300, compression = "lzw")

# 3. PCA input: one row per Species and Guild, one column per prey
# read.csv() changes '%IRI' to 'X.IRI'; reshape uses that exact name.
data_wide <- reshape(data, idvar = c("Species", "Guild"),
                     timevar = "Prey", direction = "wide")
names(data_wide) <- sub("^X\\.IRI\\.", "", names(data_wide))
data_wide[is.na(data_wide)] <- 0
rownames(data_wide) <- data_wide$Species
pca_data <- data_wide[, !(names(data_wide) %in% c("Species", "Guild")),
                      drop = FALSE]
# prcomp(scale.=TRUE) needs nonzero variance in each retained column.
stopifnot(all(vapply(pca_data, is.numeric, logical(1))))
stopifnot(all(vapply(pca_data, sd, numeric(1)) > 0))
pca_res <- prcomp(pca_data, scale. = TRUE)
pca_df <- as.data.frame(pca_res$x)
pca_df$Species <- rownames(pca_data)
pca_df$Guild <- data_wide$Guild

# An ellipse requires at least 3 observations per Guild.
# Both the raw-data plot and the original convex-hull plot still show all fish.
pca_df_ellipse <- pca_df %>%
  group_by(Guild) %>%
  filter(n() >= 3) %>%
  ungroup()

# 4. PCA plot with dashed ellipses
g2 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Guild)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = Species), size = 3,
                  fontface = "italic", max.overlaps = 100) +
  stat_ellipse(data = pca_df_ellipse, aes(group = Guild), linetype = 2) +
  coord_cartesian(clip = "off") + theme_bw() +
  theme(plot.margin = margin(10, 30, 10, 10)) +
  labs(x = paste0("PC1 (", round(summary(pca_res)$importance[2, 1] * 100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca_res)$importance[2, 2] * 100, 1), "%)"))
print(g2)
ggsave("Plot_PCA1_new_italic.tiff", plot = g2, width = 10, height = 6,
       dpi = 300, compression = "lzw")

# 5. PCA with filled confidence ellipses
p3 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Guild)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = Species), size = 3,
                  fontface = "italic", max.overlaps = 100,
                  box.padding = 0.5, point.padding = 0.3) +
  stat_ellipse(data = pca_df_ellipse,
               aes(group = Guild, fill = Guild), geom = "polygon",
               alpha = 0.2, color = NA) +
  coord_cartesian(clip = "off") + theme_bw() +
  theme(plot.margin = margin(10, 30, 10, 10)) +
  labs(x = paste0("PC1 (", round(summary(pca_res)$importance[2, 1] * 100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca_res)$importance[2, 2] * 100, 1), "%)"))
print(p3)
ggsave("Plot_3_new_italic.tiff", plot = p3, width = 10, height = 6,
       dpi = 300, compression = "lzw")

# 6. Final PCA with convex hulls
hull_data <- pca_df %>%
  group_by(Guild) %>%
  slice(chull(PC1, PC2)) %>%
  ungroup()

p4 <- ggplot(pca_df, aes(PC1, PC2, color = Guild)) +
  geom_point(size = 3) +
  geom_polygon(data = hull_data, aes(fill = Guild), alpha = 0.2, color = NA) +
  geom_text_repel(aes(label = Species), size = 4,
                  fontface = "italic", max.overlaps = 100,
                  box.padding = 0.5, point.padding = 0.3) +
  coord_cartesian(clip = "off") + theme_bw() +
  theme(plot.margin = margin(10, 30, 10, 10)) +
  labs(x = paste0("PC1 (", round(summary(pca_res)$importance[2, 1] * 100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca_res)$importance[2, 2] * 100, 1), "%)"))
print(p4)
ggsave("Plot_4_new_italic.tiff", plot = p4, width = 10, height = 6,
       dpi = 300, compression = "lzw")
