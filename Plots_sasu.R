
setwd("C:/Users/Koushik/OneDrive/Documents/Rstudio/NMDS_Sasu")

library(ggplot2)
library(dplyr)
library(readr)
library(ggrepel)

data <- read.csv("Diet Overall compilation.csv")
summary(data)

#Stacked Bar Plot
p <- ggplot(data, aes(x = Species, y = X.IRI, fill = Prey)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Species", y = "%IRI", fill = "Prey") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c("Crab" = "#E41A1C","Shrimp" = "#377EB8","Fish" = "#4DAF4A",
    "Clams" = "#984EA3","Amphipods" = "#FF7F00","Copepods" = "#FFFF33","Detritus" = "#A65628",
    "Fish.eggs" = "#F781BF","Isopods" = "#999999","Polychaete" = "#66C2A5",
    "Algae" = "#00BFC4"))
p


#Save the plot
ggsave("Stacked Bar Plot.tiff", width = 10, height = 6, dpi = 300)


#Faceted Plot by Guild

p2 <- ggplot(data, aes(x = Species, y = X.IRI, fill = Prey)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Guild, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) +
  labs(x = "Species", y = "%IRI", fill = "Prey")
p2

#Save the plot
ggsave("Plot_2_new.tiff", width = 10, height = 6, dpi = 300)


#PCA 

data_wide <- reshape(data,
  idvar = c("Species", "Guild"),
  timevar = "Prey",
  direction = "wide")

names(data_wide) <- gsub("%IRI.", "", names(data_wide))
names(data_wide) <- gsub("\\.", "_", names(data_wide))

data_wide[is.na(data_wide)] <- 0
rownames(data_wide) <- data_wide$Species
pca_data <- data_wide[, !(names(data_wide) %in% c("Species", "Guild"))]

#New 

data_wide <- reshape(data, idvar = c("Species", "Guild"),
  timevar = "Prey",direction = "wide")

# Clean names
names(data_wide) <- gsub("%IRI.", "", names(data_wide))
names(data_wide) <- gsub("\\.", "_", names(data_wide))

# Replace NA with 0
data_wide[is.na(data_wide)] <- 0

rownames(data_wide) <- data_wide$Species
pca_data <- data_wide[, !(names(data_wide) %in% c("Species", "Guild"))]
pca_res <- prcomp(pca_data, scale. = TRUE)

pca_df <- as.data.frame(pca_res$x)
pca_df$Species <- rownames(pca_data)
pca_df$Guild <- data_wide$Guild

# PCA second best plot Fixing the error

g2 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Guild)) +
  geom_point(size = 3) + geom_text_repel(aes(label = Species), size = 3,
    max.overlaps = 100) + stat_ellipse(data = pca_df_ellipse,
    aes(group = Guild),
    linetype = 2) + coord_cartesian(clip = "off") + theme_bw() + 
    theme(plot.margin = margin(10, 30, 10, 10)) + 
    labs(x = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100,1), "%)"),
    y = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100,1), "%)"))
g2

#Save the plot
ggsave("Plot_PCA1_new.tiff", width = 10, height = 6, dpi = 300)


#Final PCA plot (p4)

p4 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Guild)) +
  geom_point(size = 3) + geom_text_repel(aes(label = Species),
    size = 3, max.overlaps = 100, box.padding = 0.5, point.padding = 0.3) +
    stat_ellipse(aes(group = Guild, fill = Guild), geom = "polygon",
    alpha = 0.2, color = NA) + coord_cartesian(clip = "off") + theme_bw() +
    theme(plot.margin = margin(10, 30, 10, 10)) + 
    labs(x = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100,1), "%)"),
    y = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100,1), "%)"))

p4

#save the plot
ggsave("Plot_3_new.tiff", width = 10, height = 6, dpi = 300)

#Another PCA plot (near to final)


# Function to get hull points convex hull
hull_data <- pca_df %>%
  group_by(Guild) %>%
  slice(chull(PC1, PC2))

p4 <- ggplot(pca_df, aes(PC1, PC2, color = Guild)) + geom_point(size = 3) +
  geom_polygon(data = hull_data,
               aes(fill = Guild),
               alpha = 0.2,
               color = NA) +
  geom_text_repel(aes(label = Species), size = 4,
    max.overlaps = 100,
    box.padding = 0.5,
    point.padding = 0.3) + coord_cartesian(clip = "off") +
  theme_bw() + theme(plot.margin = margin(10, 30, 10, 10)) +
  labs(x = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100,1), "%)"),
    y = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100,1), "%)"))
p4

#Save the plot
ggsave("Plot_4_new.tiff", width = 10, height = 6, dpi = 300)


