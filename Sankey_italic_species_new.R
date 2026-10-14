setwd("C:/Users/Lenovo/Documents/R/Sasu")

library(networkD3)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(scales)
library(webshot2)
library(magick)
library(htmlwidgets)

data <- read.csv("Diet Overall compilation_sankey.csv", stringsAsFactors = FALSE)
data$Guild <- trimws(data$Guild)
data$Species <- trimws(data$Species)
data$Prey <- trimws(data$Prey)
data$Genus <- word(data$Species, 1)

links1 <- data %>%
  group_by(Guild, Species) %>%
  summarise(value = sum(IRI), .groups = "drop") %>%
  rename(source = Guild, target = Species)
links2 <- data %>%
  group_by(Species, Prey) %>%
  summarise(value = sum(IRI), .groups = "drop") %>%
  rename(source = Species, target = Prey)
links <- rbind(links1, links2)
nodes <- data.frame(name = unique(c(links$source, links$target)))
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1
nodes$group <- NA_character_
for (g in unique(data$Guild)) nodes$group[nodes$name == g] <- g
for (i in seq_len(nrow(data))) nodes$group[nodes$name == data$Species[i]] <- data$Genus[i]
for (prey in unique(data$Prey)) nodes$group[nodes$name == prey] <- prey

guild_colors <- c(
  "Crab_feeders" = "#0072B2",
  "Shrimp & fish_predators" = "#D55E00",
  "Piscivores" = "#009E73",
  "Clam & shrimp_feeders" = "#CC79A7"
)
genus_list <- unique(data$Genus)
genus_colors <- setNames(hue_pal(l = 70, c = 40)(length(genus_list)), genus_list)
prey_list <- unique(data$Prey)
prey_colors <- setNames(colorRampPalette(brewer.pal(8, "Dark2"))(length(prey_list)), prey_list)
all_groups <- unique(nodes$group)
base_colors <- colorRampPalette(brewer.pal(12, "Set3"))(length(all_groups))
all_colors <- setNames(base_colors, all_groups)
all_colors[names(guild_colors)] <- guild_colors
all_colors[names(genus_colors)] <- genus_colors
all_colors[names(prey_colors)] <- prey_colors
colourScale <- paste0('d3.scaleOrdinal().domain(["',
  paste(names(all_colors), collapse = '\",\"'),
  '"]).range(["', paste(all_colors, collapse = '\",\"'), '"])')
links <- as.data.frame(links)
nodes <- as.data.frame(nodes)

p <- sankeyNetwork(
  Links = links, Nodes = nodes, Source = "source", Target = "target",
  Value = "value", NodeID = "name", NodeGroup = "group", fontSize = 14,
  nodeWidth = 40, sinksRight = FALSE, colourScale = colourScale,
  width = 800, height = 500
)

# Only italicize node labels belonging to fish species.
# Other text, the original Sankey layout and styling are unchanged.
species_names <- unique(data$Species)
p <- htmlwidgets::onRender(
  p,
  "function(el, x, speciesNames) {
     d3.select(el).selectAll('.node text')
       .filter(function(d) { return speciesNames.indexOf(d.name) !== -1; })
       .style('font-style', 'italic');
   }",
  data = species_names
)
p

htmlwidgets::saveWidget(p, "Sankey_plot_italic.html", selfcontained = TRUE)
webshot2::webshot("Sankey_plot_italic.html", file = "Sankey_plot_italic.png",
                  vwidth = 800, vheight = 500, delay = 2)
img <- magick::image_read("Sankey_plot_italic.png")
magick::image_write(img, path = "Sankey_plot_italic.tiff",
                    format = "tiff", density = "300x300", compression = "LZW")
