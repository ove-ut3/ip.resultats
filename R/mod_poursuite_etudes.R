# Module UI
  
#' @title   mod_poursuite_etudes_ui and mod_poursuite_etudes_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_poursuite_etudes
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_poursuite_etudes_ui <- function(id){
  ns <- NS(id)
  
  ui <- tagAppendChildren(
    tagList(),
    list = list(
      
      fluidRow(
        box(
          width = 12,
          tabBox(
            title = "Poursuite d'études", width = 12,
            tabPanel(
              "Années sélectionnées",
              fluidRow(
                valueBoxOutput(ns("diplomes"), width = 4),
                valueBoxOutput(ns("poursuite_etudes"), width = 4),
                valueBoxOutput(ns("tx_poursuite_etudes"), width = 4)
              )
            ),
            tabPanel(
              "\u00C9volution",
              plotly::plotlyOutput(ns("poursuite_etudes_histo"))
            )
          ),
          footer = HTML("<sup>1</sup> Sont retenus les diplômés n'ayant pas interrompu deux ans ou plus leurs études entre le baccalauréat et l'obtention du diplôme à l'Université Toulouse III - Paul Sabatier. Les résultats présentés concernent le public assimilé à la formation initiale.")
        ),
        box(
          width = 12,
          tabBox(
            title = "Poursuite d'études directe ou reprise d'études ?", width = 12,
            tabPanel(
              "Pourcentages",
              fluidRow(
                valueBoxOutput(ns("pourcentage_poursuite_etudes_directe"), width = 6),
                valueBoxOutput(ns("pourcentage_reprise_etudes"), width = 6)
              )
            ),
            tabPanel(
              "Effectifs",
              fluidRow(
                valueBoxOutput(ns("effectif_poursuite_etudes_directe"), width = 6),
                valueBoxOutput(ns("effectif_reprise_etudes"), width = 6)
              )
            ),
            tabPanel(
              "\u00C9volution",
              plotly::plotlyOutput(ns("type_poursuite_etudes_histo"))
            )
          ),
          footer = HTML("<sup>2</sup> Poursuite d'études réalisée directement après le diplôme obtenu à l'Université Toulouse III - Paul Sabatier.<br>
        <sup>3</sup> Entrée sur le marché du travail pendant au moins une année puis reprise d'études.")
        ),
        box(
          title = "Quel est le dernier niveau de diplôme visé ?", width = 12,
          uiOutput(ns("input_poursuite_etudes")),
          plotly::plotlyOutput(ns("plot_poursuite_etudes"))
        ),
        box(
          title = "Quelles raisons pour la poursuite d'études ?", width = 12,
          plotly::plotlyOutput(ns("raisons_poursuite_etudes"))
        )
      )
      
    )
  )
  
  ui <- ui$children[[1]]
  
  if (golem::get_golem_options("diplome") == "DUT") {
    
    # Suppression des raisons de la poursuite d'études
    ui$children[[4]] <- NULL
    
  }
  
  ui
}
    
# Module Server
    
#' @rdname mod_poursuite_etudes
#' @export
#' @keywords internal
    
mod_poursuite_etudes_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$diplomes <- renderValueBox({
    valueBox(
      nrow(rv$dt_reponses()) %>% scales::number(big.mark = "\u202F"),
      HTML("Diplômés répondants<sup>1<sup>"), icon = icon("user-graduate"), color = "purple"
    )
  })
  
  output$poursuite_etudes <- renderValueBox({
    valueBox(
      nrow(rv$dt_etudes()) %>% scales::number(),
      "Poursuite d'études", icon = icon("university"), color = "purple"
    )
  })
  
  output$tx_poursuite_etudes <- renderValueBox({
    valueBox(
      scales::percent(nrow(rv$dt_etudes()) / nrow(rv$dt_reponses()), suffix = NULL),
      "Taux de poursuite d'études", icon = icon("percent"), color = "purple"
    )
  })
  
  output$poursuite_etudes_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(repondant == 1) %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate(poursuite_etudes = dplyr::if_else(parcours %in% c("Poursuite d'études directe", "Reprise d'études"), "oui", "non")) %>% 
      dplyr::count(annee, poursuite_etudes) %>% 
      tidyr::spread(poursuite_etudes, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non))
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Année universitaire", title_y = "Taux de poursuites d'études",
      hovertext = paste("Taux de poursuites d'études: ", scales::percent(data$pct, suffix = "\u202F%", accuracy = 0.1, decimal.mark = ",")),
      color = "#605ca8"
    )
    
  })
  
  output$pourcentage_poursuite_etudes_directe <- renderValueBox({
    valueBox(
      scales::percent(
        nrow(dplyr::filter(rv$dt_etudes(), parcours == "Poursuite d'études directe")) / nrow(rv$dt_etudes()),
        suffix = NULL),
      HTML("Taux de poursuite d'études directes<sup>2</sup>"), icon = icon("percent"), color = "purple"
    )
  })
  
  output$pourcentage_reprise_etudes <- renderValueBox({
    valueBox(
      scales::percent(
        nrow(dplyr::filter(rv$dt_etudes(), parcours == "Reprise d'études")) / nrow(rv$dt_etudes()),
        suffix = NULL),
      HTML("Taux de reprise d'études<sup>3</sup>"), icon = icon("percent"), color = "purple"
    )
  })
  
  output$effectif_poursuite_etudes_directe <- renderValueBox({
    valueBox(
      nrow(dplyr::filter(rv$dt_etudes(), parcours == "Poursuite d'études directe")) %>% scales::number(big.mark = "\u202F"),
      HTML("Nombre de poursuites d'études directes<sup>2</sup>"), icon = icon("users"), color = "purple"
    )
  })
  
  output$effectif_reprise_etudes <- renderValueBox({
    valueBox(
      nrow(dplyr::filter(rv$dt_etudes(), parcours == "Reprise d'études")) %>% scales::number(big.mark = "\u202F"),
      HTML("Nombre de reprises d'études<sup>3</sup>"), icon = icon("users"), color = "purple"
    )
  })
  
  output$type_poursuite_etudes_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(parcours %in% c("Poursuite d'études directe", "Reprise d'études")) %>% 
      dplyr::mutate_at("annee", as.character) %>% 
      dplyr::mutate_at("parcours", droplevels)
      
    levels(data$parcours) <- c("Poursuite d'études directe<sup>2</sup>", "Reprise d'études<sup>3</sup>")
    
    graphr::shiny_areas_evolution(
      data$annee, data$parcours,
      title_x = "Année universitaire",
      colors = c("#434078", "#918ec3")
    )
    
  })
  
  output$input_poursuite_etudes <- renderUI({
    
    selectInput(ns("filtre_type_poursuite_etudes"), label = "Type de poursuite d'études :", choices = c("Poursuite d'études directe", "Reprise d'études"), selected = c("Poursuite d'études directe", "Reprise d'études"), multiple = TRUE, selectize = TRUE)
    
  })
  
  output$plot_poursuite_etudes <- plotly::renderPlotly({
    
    req(input$filtre_type_poursuite_etudes)
    
    data <- rv$dt_etudes() %>% 
      dplyr::filter(type_diplome == golem::get_golem_options("diplome")) %>% 
      dplyr::filter(parcours %in% c("Poursuite d'études directe", "Reprise d'études")) %>% 
      dplyr::filter(parcours %in% input$filtre_type_poursuite_etudes)
    
    graphr::shiny_treemap_bi(
      data$niveau_diplome_vise,
      data$diplome_vise, 
      colors = c("#434078", "#605ca8", "#918ec3"))

  })
  
  output$raisons_poursuite_etudes <- plotly::renderPlotly({
    
    data <- rv$dt_etudes() %>% 
      dplyr::filter(parcours == "Poursuite d'études directe") %>% 
      dplyr::select(pours_etud_n_n1_raison) %>% 
      tidyr::unnest_legacy() %>% 
      tidyr::drop_na(pours_etud_n_n1_raison)
    
    validate(
      need(nrow(data) >= 1, "Pas de données disponibles avec les filtres sélectionnés")
    )
    
    data %>% 
      dplyr::pull(pours_etud_n_n1_raison) %>% 
      graphr::shiny_barplot_horizontal(colors = "#605ca8", alpha = 0.8)

  })
  
}
