# Module UI
  
#' @title   mod_emploi_premier_ui and mod_emploi_premier_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_emploi_premier
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_emploi_premier_ui <- function(id){
  ns <- NS(id)
  
  ui <- tagAppendChildren(
    tagList(),
    list = list(
      
      fluidRow(
        tabBox(
          title = "Accès au premier emploi", width = 12,
          tabPanel(
            "Valeur",
            fluidRow(
              valueBoxOutput(ns("vie_active_durable"), width = 4),
              valueBoxOutput(ns("emploi_premier"), width = 4),
              valueBoxOutput(ns("tx_emploi_premier"), width = 4)
            )
          ),
          tabPanel(
            "\u00C9volution",
            plotly::plotlyOutput(ns("tx_emploi_premier_histo"))
          )
        ),
        tabBox(
          title = "Temps d'accès au premier emploi", width = 6,
          tabPanel(
            "Valeur",
            fluidRow(
              valueBoxOutput(ns("emploi_premier_duree_recherche"), width = 12)
            )
          ),
          tabPanel(
            "Graphique",
            plotly::plotlyOutput(ns("emploi_premier_duree_recherche_graph"))
          ),
          tabPanel(
            "\u00C9volution",
            plotly::plotlyOutput(ns("emploi_premier_duree_recherche_histo"))
          )
        ),
        tabBox(
          title = "Localisation du premier emploi", width = 6,
          tabPanel(
            "Valeur",
            plotly::plotlyOutput(ns("emploi_premier_localisation"))
          ),
          tabPanel(
            "\u00C9volution",
            plotly::plotlyOutput(ns("emploi_premier_localisation_histo"))
          )
        ),
        box(
          title = "Moyen d'accès au premier emploi", width = 6,
          echarts4r::echarts4rOutput(ns("emploi_premier_moyen"))
        ),
        tabBox(
          title = "Difficultés d'accès rencontrées", width = 6,
          tabPanel(
            "Taux de diplômés concernés",
            fluidRow(
              valueBoxOutput(ns("emploi_premier_tx_difficultes"), width = 12)
            )
          ),
          tabPanel(
            "Détail",
            echarts4r::echarts4rOutput(ns("emploi_premier_difficultes"))
          )
        )
      )
      
    )
  )
  
  ui <- ui$children[[1]]
  
  if (golem::get_golem_options("diplome") == "DUT") {
  
    # Suppression de la localisation, moyen et difficultés d'accès
    ui$children[3:5] <- NULL
    
  }
  
  ui
}
    
# Module Server
    
#' @rdname mod_emploi_premier
#' @export
#' @keywords internal
    
mod_emploi_premier_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$vie_active_durable <- renderValueBox({
    valueBox(
      nrow(rv$dt_vad()) %>% caractr::str_number_fr(),
      "Vie active durable", icon = icon("users")
    )
  })
  
  output$emploi_premier <- renderValueBox({
    valueBox(
      nrow(rv$dt_emploi_occupe()) %>% caractr::str_number_fr(),
      "Accès à un premier emploi", icon = icon("user-tie")
    )
  })
  
  output$tx_emploi_premier <- renderValueBox({
    valueBox(
      caractr::str_percent_fr(nrow(rv$dt_emploi_occupe()) / nrow(rv$dt_vad()), suffix = FALSE),
      "Taux d'accès à un premier emploi", icon = icon("percent")
    )
  })
  
  output$tx_emploi_premier_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(parcours == "Vie active durable") %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate_at("emploi_occupe", tolower) %>%
      dplyr::count(annee, emploi_occupe) %>% 
      tidyr::spread(emploi_occupe, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non) * 100)
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Année universitaire", title_y = "Taux d'accès à un premier emploi",
      hovertext = paste("Taux d'accès à un premier emploi: ", caractr::str_percent_fr(data$pct / 100))
    )
    
  })
  
  output$emploi_premier_duree_recherche <- renderValueBox({
    
    emploi_premier_duree_recherche <- rv$dt_emploi_occupe() %>% 
      tidyr::drop_na(emploi_premier_duree_recherche)
    
    valueBox(
      caractr::str_percent_fr(
        nrow(dplyr::filter(emploi_premier_duree_recherche, emploi_premier_duree_recherche <= 3)) / nrow(emploi_premier_duree_recherche)
      ),
      "Taux d'accès au 1er emploi en 3 mois ou moins", icon = icon("clock")
    )
  })
  
  output$emploi_premier_duree_recherche_graph <- plotly::renderPlotly({
    
    data <- rv$dt_emploi_occupe() %>% 
      tidyr::drop_na(emploi_premier_duree_recherche)
    
    data <- data %>% 
      dplyr::count(emploi_premier_duree_recherche) %>% 
      dplyr::group_by() %>% 
      dplyr::mutate(en_emploi = cumsum(n)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(non_emploi = nrow(data) - en_emploi) %>% 
      tidyr::gather("var", "n", en_emploi, non_emploi) %>% 
      dplyr::mutate_at("var", dplyr::recode, "en_emploi" = "En emploi", "non_emploi" = "En recherche d'emploi") %>% 
      dplyr::mutate_at("var", factor, levels = c("En emploi", "En recherche d'emploi")) %>% 
      dplyr::mutate(list = purrr::map(n, ~ 1:.)) %>% 
      tidyr::unnest_legacy()
    
    graphr::shiny_areas_evolution(data$emploi_premier_duree_recherche, data$var)
    
  })
  
  output$emploi_premier_duree_recherche_histo <- plotly::renderPlotly({

    data <- rv$dt_evolution() %>%
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::filter(parcours == "Vie active durable",
                    emploi_occupe == "Oui") %>% 
      tidyr::drop_na(emploi_premier_duree_recherche) %>%
      dplyr::mutate(emploi_premier_duree_3mois = dplyr::if_else(emploi_premier_duree_recherche <= 3, "oui", "non")) %>%
      dplyr::count(annee, emploi_premier_duree_3mois) %>%
      tidyr::spread(emploi_premier_duree_3mois, n, fill = 0) %>%
      dplyr::mutate(pct = oui / (oui + non) * 100)

    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Année universitaire", title_y = "Taux d'accès au premier emploi en 3 mois ou moins",
      hovertext = paste("Taux d'accès au premier emploi en 3 mois ou moins: ", caractr::str_percent_fr(data$pct / 100))
    )

  })
  
  output$emploi_premier_localisation <- plotly::renderPlotly({
    
    rv$dt_emploi_occupe() %>%
      tidyr::drop_na(emploi_premier_localisation) %>% 
      dplyr::pull(emploi_premier_localisation) %>%
      graphr::shiny_donut(alpha = 0.67)
    
  })
  
  output$emploi_premier_localisation_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(parcours == "Vie active durable",
                    emploi_occupe == "Oui") %>% 
      tidyr::drop_na(emploi_premier_localisation) %>% 
      dplyr::mutate_at("annee", as.character)
    
    graphr::shiny_areas_evolution(data$annee, data$emploi_premier_localisation, title_x = "Année universitaire")
    
  })
  
  output$emploi_premier_moyen <- echarts4r::renderEcharts4r({
    
    rv$dt_emploi_occupe() %>%
      tidyr::drop_na(emploi_premier_moyen) %>% 
      dplyr::pull(emploi_premier_moyen) %>%
      graphr::shiny_treemap(alpha = 0.67)
    
  })
  
  output$emploi_premier_tx_difficultes <- renderValueBox({
    
    data <- rv$dt_emploi_occupe() %>% 
      tidyr::unnest_legacy(emploi_premier_difficulte_acces) %>% 
      tidyr::drop_na(emploi_premier_difficulte_acces) %>% 
      dplyr::count(identifiant)
    
    valueBox(
      caractr::str_percent_fr(
        nrow(data) / nrow(rv$dt_emploi_occupe()),
        suffix = FALSE
      ),
      "Taux de diplômés ayant rencontré des difficultés", icon = icon("percent")
    )
    
  })
  
  output$emploi_premier_difficultes <- echarts4r::renderEcharts4r({
    
    rv$dt_emploi_occupe() %>%
      tidyr::unnest_legacy(emploi_premier_difficulte_acces) %>% 
      tidyr::drop_na(emploi_premier_difficulte_acces) %>% 
      dplyr::pull(emploi_premier_difficulte_acces) %>%
      graphr::shiny_treemap(alpha = 0.67)
    
  })
  
}
