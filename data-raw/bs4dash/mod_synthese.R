# Module UI
  
#' @title   mod_synthese_ui and mod_synthese_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_synthese
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_synthese_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow( # Pour éviter le background vide en scrollant
      div("<br>", style = "height: 50px;"),
      box(title = "Après l'obtention du diplôme, poursuite d'études ou emploi ?", width = 12,
          fluidRow(
            valueBoxOutput(ns("etudes"), width = 6),
            valueBoxOutput(ns("vie_active_durable"), width = 6)
          ),
          footer = "* Aucune poursuite d'études pendant les 30 mois consécutifs à l'obtention du diplôme. Les inactifs sont inclus."
      ),
      bs4Box(title = "Poursuite d'études : le dernier niveau de diplôme visé", width = 12,
          plotOutput(ns("niveau_diplome"), height = "150px")
      ),
      bs4Box(title = "Vie active durable : les caractéristiques d'emploi", width = 12,
             fluidRow(
               valueBoxOutput(ns("emploi_premier_duree_recherche"), width = 6),
               valueBoxOutput(ns("emploi_n2_insertion"), width = 6)
             ),
          bs4Box(title = "Emploi occupé à 30 mois", width = 12,
                 fluidRow(
                   column(width = 6,
                          plotly::plotlyOutput(ns("emploi_n2_niveau"))
                   ),
                   column(width = 6,
                          valueBoxOutput(ns("salaire"), width = 12),
                          valueBoxOutput(ns("cdi_assimiles"), width = 12),
                          valueBoxOutput(ns("occitanie"), width = 12)
                   )
                 )
          ),
          footer = HTML("<sup>1</sup> Diplômés en emploi / Diplômés en recherche d'emploi<br>
                      <sup>2</sup> Primes incluses, pour un emploi à temps plein en France. Le salaire médian est celui qui coupe en deux les répondants : 50% ont un salaire inférieur et 50% un salaire supérieur.<br>
                      <sup>3</sup> CDI, fonctionnaire, profession libérale, indépendant, chef-fe d'entreprise")
      ),
      bs4Box(title = "Taux de réponse", width = 12,
             fluidRow(
               valueBoxOutput(ns("diplomes"), width = 4),
               valueBoxOutput(ns("repondants"), width = 4),
               valueBoxOutput(ns("tx_reponse"), width = 4)
             )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_synthese
#' @export
#' @keywords internal
    
mod_synthese_server <- function(input, output, session, dt_diplomes, dt_reponses, dt_etudes, dt_vad){
  ns <- session$ns
  
  output$etudes <- renderValueBox({
    valueBox(
      caractr::str_percent_fr(nrow(dt_etudes()) / nrow(dt_reponses())),
      "Poursuite d'études", icon = "university", color = "yellow"
    )
  })
  
  output$vie_active_durable <- renderValueBox({
    valueBox(
      caractr::str_percent_fr(nrow(dt_vad()) / nrow(dt_reponses())),
      "Vie active durable*", icon = "user-tie", color = "yellow"
    )
  })
  
  output$niveau_diplome <- renderPlot({
    
    if (golem::get_golem_options("type_diplome") %in% c("DUT", "LP")) {
      levels <- c("Niveau Bac+5", "Niveau Bac+3", "Diplôme de niveau inférieur et autre")
    } else if (golem::get_golem_options("type_diplome") == "Master") {
      levels <- c("Doctorat", "Niveau Bac+5", "Diplôme de niveau inférieur et autre")
    }
    
    palette <- dt_etudes() %>% 
      dplyr::pull(niveau_diplome_vise) %>% 
      unique() %>% 
      length() %>% 
      RColorBrewer::brewer.pal("Set2")
    
    extrafont::loadfonts(device = "win")
    
    dt_etudes() %>% 
      dplyr::mutate_at("niveau_diplome_vise", factor, levels = levels) %>% 
      dplyr::pull(niveau_diplome_vise) %>% 
      table() %>% 
      {. / nrow(dt_etudes()) * 100} %>% 
      divr::round_100() %>% 
      waffle::waffle(rows = 2, colors = palette, size = 10, use_glyph = "university", glyph_size = 8, legend_pos = "bottom", legend_size = 15,
                     glyph_font = "FontAwesome",
                     glyph_font_family = "FontAwesome")
    
    
  })
  
  output$emploi_premier_duree_recherche <- renderValueBox({
    emploi_premier_duree_recherche <- dt_vad() %>% 
      tidyr::drop_na(emploi_premier_duree_recherche)
    valueBox(
      caractr::str_percent_fr(
        nrow(dplyr::filter(emploi_premier_duree_recherche, emploi_premier_duree_recherche <= 3)) / nrow(emploi_premier_duree_recherche)
        ),
      "Taux d'accès au 1er emploi en 3 mois ou moins", icon = "clock", status = "primary"
    )
  })
  
  output$emploi_n2_insertion <- renderValueBox({
    emploi_n2_insertion <- dt_vad() %>% 
      dplyr::filter(situation_pro_n2 %in% c("En emploi", "En recherche d'emploi", "Promesse d'embauche"))
    valueBox(
      caractr::str_percent_fr(
        nrow(dplyr::filter(emploi_n2_insertion, situation_pro_n2 == "En emploi")) / nrow(emploi_n2_insertion),
        suffix = FALSE
        ),
      HTML("Taux d'insertion<sup>1</sup>"), icon = "percent", status = "primary"
    )
  })
  
  output$emploi_n2_niveau <- plotly::renderPlotly({
    
    palette <- dt_vad( )%>% 
      tidyr::drop_na(emploi_n2_niveau) %>% 
      dplyr::pull(emploi_n2_niveau) %>% 
      unique() %>% 
      length() %>% 
      RColorBrewer::brewer.pal("Set1")
    
    dt_vad() %>%  
      tidyr::drop_na(emploi_n2_niveau) %>% 
      dplyr::count(emploi_n2_niveau) %>% 
      dplyr::group_by() %>% 
      dplyr::mutate(text = n / sum(n)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(text = dplyr::if_else(text < 0.005, round(text, digits = 3), round(text, 2))) %>% 
      dplyr::mutate_at("text", caractr::str_percent_fr, digits = NULL) %>% 
      plotly::plot_ly(labels = ~emploi_n2_niveau, values = ~n, sort = FALSE, direction = "clockwise",
                      textinfo = "text",
                      text = ~text,
                      hoverinfo = "text",
                      hovertext = ~paste("Effectif: ", n)
      ) %>%
      plotly::add_pie(hole = 0.6) %>%
      plotly::layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     annotations = list(text = "<b>Niveau d'emploi</b>", font = list(size = 15), showarrow = FALSE),
                     legend = list(y = 0.5))
    
  })
  
  output$salaire <- renderValueBox({
    emploi_n2_insertion <- dt_vad() %>% 
      dplyr::filter(situation_pro_n2 == "En emploi", emploi_n2_temoin_temps_partiel == "Non") %>% 
      tidyr::drop_na(emploi_n2_salaire)
    valueBox(
      median(emploi_n2_insertion$emploi_n2_salaire) %>% 
        round() %>% 
        caractr::str_number_fr(),
      HTML("Salaire net médian<sup>2</sup>"), icon = "euro-sign", status = "primary"
    )
  })
  
  output$cdi_assimiles <- renderValueBox({
    cdi_assimiles <- dt_vad() %>% 
      dplyr::filter(situation_pro_n2 == "En emploi")
    valueBox(
      caractr::str_percent_fr(
        nrow(dplyr::filter(cdi_assimiles, emploi_n2_type %in% c("CDI", "Profession libérale, indépendant, chef-fe d'entreprise, auto-entrepreneur", "Fonctionnaire"))) / nrow(cdi_assimiles),
        suffix = FALSE
      ),
      HTML("Taux de CDI et assimilés<sup>3</sup>"), icon = "percent", status = "primary"
    )
  })
  
  output$occitanie <- renderValueBox({
    occitanie <- dt_vad() %>% 
      dplyr::filter(situation_pro_n2 == "En emploi")
    valueBox(
      caractr::str_percent_fr(
        nrow(dplyr::filter(occitanie, emploi_n2_region == "76")) / nrow(occitanie)
      ),
      HTML("Taux d'emploi en région Occitanie"), icon = "map-marker-alt", status = "primary"
    )
  })
  
  output$diplomes <- renderValueBox({
    valueBox(
      nrow(dt_diplomes()), "Diplômés", icon = "user-graduate", status = "primary"
    )
  })
  
  output$repondants <- renderValueBox({
    valueBox(
      nrow(dt_reponses()), "Répondants", icon = "clipboard-check", status = "primary"
    )
  })
  
  output$tx_reponse <- renderValueBox({
    valueBox(
      caractr::str_percent_fr(nrow(dt_reponses()) / nrow(dt_diplomes()), suffix = FALSE), "Taux de réponse", icon = "percent", status = "primary"
    )
  })
  
}
