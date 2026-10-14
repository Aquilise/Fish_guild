# Sankey diagram: guild-colored links and italic fish scientific names
# Original nodes, group colors, IRI values, and 800 x 500 layout settings retained.
setwd("C:/Users/Lenovo/Documents/R/Sasu")

library(networkD3)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(scales)
library(htmlwidgets)
library(webshot2)
library(magick)

data <- read.csv("Diet Overall compilation_sankey.csv", stringsAsFactors = FALSE)
stopifnot(all(c("Guild", "Species", "Prey", "IRI") %in% names(data)))
data <- data %>%
  mutate(across(c(Guild, Species, Prey), trimws),
         Genus = word(Species, 1))

# Each species must belong to exactly one guild for guild-colored links.
species_guild <- data %>% distinct(Species, Guild)
stopifnot(!anyDuplicated(species_guild$Species))

# Guild -> Species; links inherit their source guild color.
links1 <- data %>%
  group_by(Guild, Species) %>%
  summarise(value = sum(IRI, na.rm = TRUE), .groups = "drop") %>%
  transmute(source = Guild, target = Species, value,
            link_group = Guild)

# Species -> Prey; links inherit the same guild color.
links2 <- data %>%
  group_by(Species, Prey) %>%
  summarise(value = sum(IRI, na.rm = TRUE), .groups = "drop") %>%
  left_join(species_guild, by = "Species") %>%
  transmute(source = Species, target = Prey, value,
            link_group = Guild)

links <- bind_rows(links1, links2)
nodes <- data.frame(name = unique(c(links$source, links$target)),
                    stringsAsFactors = FALSE)

# Preserve the existing node color grouping: guild / species genus / prey.
nodes$group <- NA_character_
nodes$group[nodes$name %in% data$Guild] <- nodes$name[nodes$name %in% data$Guild]
for (i in seq_len(nrow(data))) {
  nodes$group[nodes$name == data$Species[i]] <- data$Genus[i]
}
nodes$group[nodes$name %in% data$Prey] <- nodes$name[nodes$name %in% data$Prey]
stopifnot(!anyNA(nodes$group))

# networkD3 requires zero-based node indices.
links$source <- match(links$source, nodes$name) - 1L
links$target <- match(links$target, nodes$name) - 1L

guild_colors <- c(
  "Crab_feeders" = "#0072B2",
  "Shrimp & fish_predators" = "#D55E00",
  "Piscivores" = "#009E73",
  "Clam & shrimp_feeders" = "#CC79A7"
)
genus_list <- unique(data$Genus)
genus_colors <- setNames(hue_pal(l = 70, c = 40)(length(genus_list)), genus_list)
prey_list <- unique(data$Prey)
prey_colors <- setNames(
  colorRampPalette(brewer.pal(8, "Dark2"))(length(prey_list)), prey_list
)
all_groups <- unique(c(nodes$group, links$link_group))
all_colors <- setNames(
  colorRampPalette(brewer.pal(12, "Set3"))(length(all_groups)),
  all_groups
)
all_colors[names(guild_colors)] <- guild_colors
all_colors[names(genus_colors)] <- genus_colors
all_colors[names(prey_colors)] <- prey_colors

# Correctly escape names (including '&') in the JavaScript color scale.
colourScale <- paste0(
  'd3.scaleOrdinal().domain(',
  jsonlite::toJSON(names(all_colors), auto_unbox = FALSE),
  ').range(',
  jsonlite::toJSON(unname(all_colors), auto_unbox = FALSE),
  ')'
)

p <- sankeyNetwork(
  Links = as.data.frame(links), Nodes = as.data.frame(nodes),
  Source = "source", Target = "target", Value = "value",
  NodeID = "name", NodeGroup = "group", LinkGroup = "link_group",
  fontSize = 14, nodeWidth = 40, sinksRight = FALSE,
  colourScale = colourScale, width = 800, height = 500
)

# Italicize only scientific names; guild and prey labels stay upright.
# Keep the original flow colors; 0.45 opacity makes the colored links softer.
p <- htmlwidgets::onRender(
  p,
  "function(el, x, speciesNames) {
     d3.select(el).selectAll('.node text')
       .filter(function(d) { return speciesNames.indexOf(d.name) !== -1; })
       .style('font-style', 'italic');
     d3.select(el).selectAll('.link')
       .style('stroke-opacity', 0.45);
   }",
  data = unique(data$Species)
)

p

# New names prevent overwriting your original manuscript figure.
htmlwidgets::saveWidget(p, "Sankey_guild_colored_italic.html", selfcontained = TRUE)
webshot2::webshot("Sankey_guild_colored_italic.html",
                  file = "Sankey_guild_colored_italic.png",
                  vwidth = 800, vheight = 500, delay = 2)
img <- magick::image_read("Sankey_guild_colored_italic.png")
magick::image_write(img, path = "Sankey_guild_colored_italic.tiff",
                    format = "tiff", density = "300x300", compression = "LZW")
