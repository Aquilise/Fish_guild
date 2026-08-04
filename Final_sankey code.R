

setwd("C:/Users/Koushik/OneDrive/Documents/Rstudio/NMDS_Sasu")

#2nd (3rd I skipped for the dataframe_ for me it is corrected in the CSV file)
data <- read.csv("Diet Overall compilation_sankey.csv")

#1st
library(networkD3)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(scales)
library(webshot2)
library(magick)
library(htmlwidgets)

#4th Clean text (IMPORTANT)
data$Guild <- trimws(data$Guild)
data$Species <- trimws(data$Species)
data$Prey <- trimws(data$Prey)

#5th Extract Genus
data$Genus <- word(data$Species, 1)

#6th Create links__(Guild → Species)
#Guild → Species

links1 <- data %>%
  group_by(Guild, Species) %>%
  summarise(value = sum(IRI), .groups = "drop") %>%
  rename(source = Guild, target = Species)

# (Species → Prey)
links2 <- data %>%
  group_by(Species, Prey) %>%
  summarise(value = sum(IRI), .groups = "drop") %>%
  rename(source = Species, target = Prey)

#Combine 
links <- rbind(links1, links2)

#7th Create nodes
nodes <- data.frame(name = unique(c(links$source, links$target)))

#8th Convert to indices
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1


#9th Assign node groups (CRITICAL Steps)
nodes$group <- NA

# Guild
for(g in unique(data$Guild)){
  nodes$group[nodes$name == g] <- g
}

# Species → Genus
for(i in 1:nrow(data)){
  nodes$group[nodes$name == data$Species[i]] <- data$Genus[i]
}

# Prey
for(p in unique(data$Prey)){
  nodes$group[nodes$name == p] <- p
}

#10th DEFINE CLEAN COLORS (Guilds+Species+Prey)
guild_colors <- c(
  "Crab_feeders" = "#0072B2",
  "Shrimp & fish_predators" = "#D55E00",
  "Piscivores" = "#009E73",
  "Clam & shrimp_feeders" = "#CC79A7")

#Soft color
genus_list <- unique(data$Genus)

genus_colors <- setNames(hue_pal(l = 70, c = 40)(length(genus_list)),
  genus_list)

#Prey distinct but clean
prey_list <- unique(data$Prey)

prey_colors <- setNames(colorRampPalette(brewer.pal(8, "Dark2"))(length(prey_list)),
  prey_list)

#11th Combine colors (very important)

all_groups <- unique(nodes$group)

base_colors <- colorRampPalette(brewer.pal(12, "Set3"))(length(all_groups))
all_colors <- setNames(base_colors, all_groups)

# overwrite with correct colors
all_colors[names(guild_colors)] <- guild_colors
all_colors[names(genus_colors)] <- genus_colors
all_colors[names(prey_colors)] <- prey_colors

#12th Create colourScale

colourScale <- paste0('d3.scaleOrdinal().domain(["',
  paste(names(all_colors), collapse = '","'),
  '"]).range(["',paste(all_colors, collapse = '","'),'"])')

#13 th Convert to data.frame (fix warning)

links <- as.data.frame(links)
nodes <- as.data.frame(nodes)

#14 th Sankey plot Final outcome


p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
  Target = "target", Value = "value", NodeID = "name", NodeGroup = "group", fontSize = 14,
  nodeWidth = 40, sinksRight = FALSE, colourScale = colourScale, width = 800,height = 500)

p

#Save the image for sankey plot

# 1. Save HTML
saveWidget(p, "Sankey_plot.html", selfcontained = TRUE)

# 2. HTML → PNG
webshot2::webshot("Sankey_plot.html", file = "Sankey_plot.png", vwidth = 800,
  vheight = 500)

# 3. PNG → TIFF correct the dpi 
img <- image_read("Sankey_plot.png")
image_write(img, path = "Sankey_plot_final.tiff",
  format = "tiff", density = "300x300")

#Save final image in tiff 
image_write(img, "Sankey_plot.tiff", format = "tiff")









