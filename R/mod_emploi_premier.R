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
          title = "Acc\u00e8s au premier emploi", width = 12,
          tabPanel(
            "Ann\u00e9es s\u00e9lectionn\u00e9es",
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
          title = "Temps d'acc\u00e8s au premier emploi", width = 6,
          tabPanel(
            "Ann\u00e9es s\u00e9lectionn\u00e9es",
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
            "Ann\u00e9es s\u00e9lectionn\u00e9es",
            plotly::plotlyOutput(ns("emploi_premier_localisation"))
          ),
          tabPanel(
            "\u00C9volution",
            plotly::plotlyOutput(ns("emploi_premier_localisation_histo"))
          )
        ),
        box(
          title = "Moyen d'acc\u00e8s au premier emploi", width = 6,
          plotly::plotlyOutput(ns("emploi_premier_moyen"))
        ),
        tabBox(
          title = "Difficult\u00e9s d'acc\u00e8s rencontr\u00e9es", width = 6,
          tabPanel(
            "Taux de dipl\u00f4m\u00e9s concern\u00e9s",
            fluidRow(
              valueBoxOutput(ns("emploi_premier_tx_difficultes"), width = 12)
            )
          ),
          tabPanel(
            "D\u00e9tail",
            plotly::plotlyOutput(ns("emploi_premier_difficultes"))
          )
        )
      )
      
    )
  )
  
  ui <- ui$children[[1]]
  
  if (golem::get_golem_options("diplome") == "DUT") {
  
    # Suppression de la localisation, moyen et difficult\u00e9s d'acc\u00e8s
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
      nrow(rv$dt_vad()) %>% scales::number(big.mark = "\u202F"),
      "Vie active durable", icon = icon("users"), color = "black"
    )
  })
  
  output$emploi_premier <- renderValueBox({
    valueBox(
      nrow(rv$dt_emploi_occupe()) %>% scales::number(big.mark = "\u202F"),
      "Acc\u00e8s \u00e0 un premier emploi", icon = icon("user-tie"), color = "black"
    )
  })
  
  output$tx_emploi_premier <- renderValueBox({
    valueBox(
      scales::percent(nrow(rv$dt_emploi_occupe()) / nrow(rv$dt_vad()), suffix = NULL),
      "Taux d'acc\u00e8s \u00e0 un premier emploi", icon = icon("percent"), color = "black"
    )
  })
  
  output$tx_emploi_premier_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(.data$parcours == "Vie active durable") %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate_at("emploi_occupe", tolower) %>%
      dplyr::count(.data$annee, .data$emploi_occupe) %>% 
      tidyr::spread(.data$emploi_occupe, .data$n, fill = 0) %>% 
      dplyr::mutate(pct = .data$oui / (.data$oui + .data$non))
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Ann\u00e9e universitaire", title_y = "Taux d'acc\u00e8s \u00e0 un premier emploi",
      hovertext = paste("Taux d'acc\u00e8s \u00e0 un premier emploi: ", scales::percent(data$pct, suffix = "\u202F%", accuracy = 0.1, decimal.mark = ",")), 
      color = "#585858",
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$emploi_premier_duree_recherche <- renderValueBox({
    
    data <- rv$dt_emploi_occupe() %>% 
      tidyr::drop_na(.data$emploi_premier_duree_recherche)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    valueBox(
      scales::percent(
        nrow(dplyr::filter(data, .data$emploi_premier_duree_recherche <= 3)) / nrow(data), 
        suffix = "\u202F%"
      ),
      "Taux d'acc\u00e8s au 1er emploi en 3 mois ou moins", icon = icon("clock"), color = "black"
    )
  })
  
  output$emploi_premier_duree_recherche_graph <- plotly::renderPlotly({
    
    data <- rv$dt_emploi_occupe() %>% 
      tidyr::drop_na(.data$emploi_premier_duree_recherche)
    
    data <- data %>% 
      dplyr::count(.data$emploi_premier_duree_recherche) %>% 
      dplyr::group_by() %>% 
      dplyr::mutate(en_emploi = cumsum(.data$n)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(non_emploi = nrow(data) - .data$en_emploi) %>% 
      tidyr::gather("var", "n", .data$en_emploi, .data$non_emploi) %>% 
      dplyr::mutate_at("var", dplyr::recode, "en_emploi" = "En emploi", "non_emploi" = "En recherche d'emploi") %>% 
      dplyr::mutate_at("var", factor, levels = c("En emploi", "En recherche d'emploi")) %>% 
      dplyr::mutate(list = purrr::map(.data$n, ~ 1:.)) %>% 
      tidyr::unnest(list) %>% 
      dplyr::filter(.data$n != 0)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_areas_evolution(
      data$emploi_premier_duree_recherche, data$var,
      title_x = "Nombre de mois",
      colors = c("#313131", "#7e7e7e"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$emploi_premier_duree_recherche_histo <- plotly::renderPlotly({

    data <- rv$dt_evolution() %>%
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::filter(
        .data$parcours == "Vie active durable",
        .data$emploi_occupe == "Oui"
      ) %>% 
      tidyr::drop_na(.data$emploi_premier_duree_recherche) %>%
      dplyr::mutate(emploi_premier_duree_3mois = dplyr::if_else(.data$emploi_premier_duree_recherche <= 3, "oui", "non")) %>%
      dplyr::count(.data$annee, .data$emploi_premier_duree_3mois) %>%
      tidyr::spread(.data$emploi_premier_duree_3mois, .data$n, fill = 0) %>%
      dplyr::mutate(pct = .data$oui / (.data$oui + .data$non))

    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Ann\u00e9e universitaire", title_y = "Taux d'acc\u00e8s au premier emploi en 3 mois ou moins",
      hovertext = paste("Taux d'acc\u00e8s au premier emploi en 3 mois ou moins: ", scales::percent(data$pct, suffix = "\u202F%", accuracy = 0.1, decimal.mark = ",")), 
      color = "#585858",
      font_family = golem::get_golem_options("graph_font_family")
    )

  })
  
  output$emploi_premier_localisation <- plotly::renderPlotly({
    
    data <- rv$dt_emploi_occupe() %>%
      tidyr::drop_na(.data$emploi_premier_localisation)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    data %>% 
      dplyr::pull(.data$emploi_premier_localisation) %>%
      graphr::shiny_barplot_horizontal(
        color = "#585858", 
        alpha = 0.8,
        font_family = golem::get_golem_options("graph_font_family")
      )
    
  })
  
  output$emploi_premier_localisation_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(
        .data$parcours == "Vie active durable",
        .data$emploi_occupe == "Oui"
      ) %>% 
      tidyr::drop_na(.data$emploi_premier_localisation) %>% 
      dplyr::mutate_at("annee", as.character)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_areas_evolution(
      data$annee, 
      data$emploi_premier_localisation, 
      title_x = "Ann\u00e9e universitaire",
      font_family = golem::get_golem_options("graph_font_family"),
      color = c("#313131", "#4b4b4b", "#646464", "#7e7e7e", "#9a9a9a")
    )
    
  })
  
  output$emploi_premier_moyen <- plotly::renderPlotly({
    
    data <- rv$dt_emploi_occupe() %>%
      tidyr::drop_na(.data$emploi_premier_moyen)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    data %>% 
      dplyr::pull(.data$emploi_premier_moyen) %>%
      as.character() %>% 
      graphr::shiny_barplot_horizontal(
        color = "#585858", 
        alpha = 0.8,
        font_family = golem::get_golem_options("graph_font_family")
      )
    
  })
  
  output$emploi_premier_tx_difficultes <- renderValueBox({
    
    data <- rv$dt_emploi_occupe() %>% 
      tidyr::unnest(.data$emploi_premier_difficulte_acces) %>% 
      tidyr::drop_na(.data$emploi_premier_difficulte_acces) %>% 
      dplyr::count(.data$annee, .data$code_etudiant)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    valueBox(
      scales::percent(
        nrow(data) / nrow(rv$dt_emploi_occupe()),
        suffix = NULL
      ),
      "Taux de dipl\u00f4m\u00e9s ayant rencontr\u00e9 des difficult\u00e9s", icon = icon("percent"), color = "black"
    )
    
  })
  
  output$emploi_premier_difficultes <- plotly::renderPlotly({
    
    data <- rv$dt_emploi_occupe() %>%
      tidyr::unnest(.data$emploi_premier_difficulte_acces) %>% 
      tidyr::drop_na(.data$emploi_premier_difficulte_acces)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    data %>% 
      dplyr::pull(.data$emploi_premier_difficulte_acces) %>%
      graphr::shiny_barplot_horizontal(
        color = "#585858", 
        alpha = 0.8,
        font_family = golem::get_golem_options("graph_font_family")
      )
    
  })
  
}
