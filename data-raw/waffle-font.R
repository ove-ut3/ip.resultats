# install.packages("extrafontdb")
# 
# extrafont::font_import(pattern = "fontawesome")
# extrafont::fonts()
# extrafont::fonts()[grep("Awesome", extrafont::fonts())]

dt_etudes <- ip.resultats::donnees %>% dplyr::filter(parcours %in% c("Poursuite d'études directe", "Reprise d'études"))

levels <- c("Niveau Bac+5", "Niveau Bac+3", "Diplôme de niveau inférieur et autre")

palette <- dt_etudes %>% 
  dplyr::pull(niveau_diplome_vise) %>% 
  unique() %>% 
  length() %>% 
  RColorBrewer::brewer.pal("Set2")

extrafont::loadfonts(device = "win")

plot <- dt_etudes %>% 
  dplyr::mutate_at("niveau_diplome_vise", factor, levels = levels) %>% 
  dplyr::pull(niveau_diplome_vise) %>% 
  table() %>% 
  {. / nrow(dt_etudes) * 100} %>% 
  divr::round_100() %>% 
  waffle::waffle(rows = 2, colors = palette, size = 10, use_glyph = "university", glyph_size = 8, legend_pos = "bottom", legend_size = 20,
                 glyph_font = "FontAwesome",
                 glyph_font_family = "FontAwesome")

plotly::ggplotly(plot)
