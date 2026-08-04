
data<-read.delim("clipboard",stringsAsFactors=T)
View(data)
summary(data)

# Convert to data frame
data <- as.data.frame(data)

library(vegan)
library(readxl)
# Select only diet composition columns
diet <- data[, c("algae", "copepods", "amphipods", "isopods",
                 "shrimp", "crab", "clams", "fish",
                 "detritus", "fish_eggs", "polychaete")]

# Optional but recommended for percentage data
diet <- diet / 100

meta <- data[, c("Species", "Seasons")]

# Check for NA values
sum(is.na(diet))

# Identify rows with any NA
which(rowSums(is.na(diet)) > 0)

# Identify empty rows (all zeros)
which(rowSums(diet, na.rm = TRUE) == 0)

diet[is.na(diet)] <- 0

# Keep only rows with some diet content
keep <- rowSums(diet) > 0

diet_clean <- diet[keep, ]
meta_clean <- meta[keep, ]

dist_mat <- vegdist(diet_clean, method = "bray")


#PERMANOVA (Adonis2) Here the result is significant (Species)
adonis2(dist_mat ~ Species + Seasons,
        data = meta_clean,
        permutations = 999,
        by = "margin")

# Effect of Season on diet composition (not sign.)
adonis2(dist_mat ~ Seasons,
        data = meta_clean,
        permutations = 999,
        by = "margin")

# Effect of Season on Species composition (Sign. species)
adonis2(dist_mat ~ Species,
        data = meta_clean,
        permutations = 999,
        by = "margin")

#Interaction model
adonis2(dist_mat ~ Species * Seasons,
        data = meta_clean,
        permutations = 999,
        by = "margin")
#homogeneity of dispersion for Season (not sign.)

disp_season <- betadisper(dist_mat, meta_clean$Seasons)
anova(disp_season) #not sign.if p>0.05 PERMANOVA valid
permutest(disp_season)

#homogeneity of dispersion for Species
disp_species <- betadisper(dist_mat, meta_clean$Species)
anova(disp_species) #not sign. if p>0.05 PERMANOVA valid
permutest(disp_species)

library(vegan)

set.seed(123)  # reproducibility

nmds <- metaMDS(diet_clean,
                distance = "bray",
                k = 2,
                trymax = 100,
                autotransform = FALSE) #Interpretation < 0.10 → excellent, 0.10–0.20 → acceptable,0.20 → caution

nmds$stress


##NMDS Plot for seasons (optional)####
plot(nmds, type = "n")

points(nmds,
       display = "sites",
       col = as.numeric(meta_clean$Seasons),
       pch = 19,
       cex = 1.2)

legend("topright",
       legend = levels(meta_clean$Seasons),
       col = 1:length(levels(meta_clean$Seasons)),
       pch = 19,
       bty = "n")

#Add 95% confidence ellipses
ordiellipse(nmds,
            groups = meta_clean$Seasons,
            kind = "sd",
            conf = 0.95,
            draw = "polygon",
            col = 1:length(levels(meta_clean$Seasons)),
            border = NA,
            alpha = 0.2,
            label = TRUE)

# Alternatives Ellipses for Species (Optional)

plot(nmds, type = "n")

points(nmds,
       display = "sites",
       col = as.numeric(meta_clean$Species),
       pch = 19)

ordiellipse(nmds,
            groups = meta_clean$Species,
            kind = "sd",
            conf = 0.95,
            draw = "polygon",
            col = 1:length(levels(meta_clean$Species)),
            border = NA,
            alpha = 0.2)

legend("topright",
       legend = levels(meta_clean$Species),
       col = 1:length(levels(meta_clean$Species)),
       pch = 19,
       bty = "n")

centroids <- aggregate(scores(nmds, display = "sites"),
                       by = list(meta_clean$Seasons),
                       FUN = mean)

points(centroids[, 2:3],
       pch = 4,
       cex = 2,
       lwd = 2)

# Final PLots for journals 

library(vegan)
library(ggplot2)

set.seed(123)

nmds <- metaMDS(diet_clean,
                distance = "bray",
                k = 2,
                trymax = 100,
                autotransform = FALSE)

scores_nmds <- as.data.frame(scores(nmds, display = "sites"))
scores_nmds$Species <- meta_clean$Species
scores_nmds$Seasons <- meta_clean$Seasons

#Plot 1 ~~~ Sasu's requirement for MS
p_season <- ggplot(scores_nmds,
                   aes(x = NMDS1, y = NMDS2, colour = Seasons)) +
  
  geom_point(size = 3, alpha = 0.9) +
  
  stat_ellipse(aes(fill = Seasons),
               geom = "polygon",
               type = "norm",
               level = 0.95,
               alpha = 0.2,
               colour = NA) +
  
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  
  theme_classic(base_size = 14) +
  
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA)
  ) +
  
  labs(
    x = "NMDS1",
    y = "NMDS2",
    colour = "Season",
    fill = "Season",
    title = "Diet composition by season"
  )
p_season

#Save the plot####
ggsave("NMDS_Season.png",
       p_season,
       width = 7,
       height = 6,
       dpi = 600)

#Plot 2 (Not good~ Sasu click the arrow if you are interested to run the code)####
#p_species <- ggplot(scores_nmds,aes(x = NMDS1, y = NMDS2, colour = Species)) +
  #geom_point(size = 3, alpha = 0.9) +
  #stat_ellipse(aes(fill = Species),
              # geom = "polygon",
              # type = "norm",
              # level = 0.95,
              # alpha = 0.2,
              # colour = NA) +
  
  #scale_colour_brewer(palette = "Set1") +
  #scale_fill_brewer(palette = "Set1") + theme_classic(base_size = 14) + theme(
    #legend.position = "right",
    #legend.title = element_text(face = "bold"),
    #axis.title = element_text(face = "bold"),
    #panel.border = element_rect(colour = "black", fill = NA)) + labs(
   # x = "NMDS1",
   # y = "NMDS2",
   # colour = "Species",
   # fill = "Species",
   # title = "NMDS of diet composition by species")

#p_species

 

#Addition The plot can be use or main for the MS #### 

diet_fit <- envfit(nmds, diet_clean, permutations = 999)
scores_sites <- as.data.frame(scores(nmds, display = "sites"))
scores_sites$Species <- meta_clean$Species

vectors <- as.data.frame(scores(diet_fit, display = "vectors"))
vectors$DietItem <- rownames(vectors)

# Optional: keep only significant diet items (p < 0.05)
sig_vectors <- vectors[diet_fit$vectors$pvals < 0.05, ]


#Plot 3 (Final plot for Species diet composition with prey)
p_diet <- ggplot() +
  
# Species points
geom_point(data = scores_sites,
             aes(x = NMDS1, y = NMDS2, colour = Species),
             size = 3,
             alpha = 0.9) +
  
# Species labels (optional but recommended)
geom_text(data = scores_sites,
            aes(x = NMDS1, y = NMDS2, label = Species),
            size = 3,
            vjust = -0.8,
            show.legend = FALSE) +
  
# Diet vectors
geom_segment(data = sig_vectors,
               aes(x = 0, y = 0,
                   xend = NMDS1 * 1.1,
                   yend = NMDS2 * 1.1),
               arrow = arrow(length = unit(0.25, "cm")),
               colour = "black",
               linewidth = 0.7) +
  
# Diet labels
geom_text(data = sig_vectors,
            aes(x = NMDS1 * 1.15,
                y = NMDS2 * 1.15,
                label = DietItem),
            size = 4,
            fontface = "bold") + theme_classic(base_size = 14) + theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA)) + labs(
    x = "NMDS1",
    y = "NMDS2",
    colour = "Species",
    title = "NMDS of species diet composition with ingested prey items")

#Print Plot
p_diet

