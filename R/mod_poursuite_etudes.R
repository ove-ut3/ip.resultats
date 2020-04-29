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
        tabBox_footer(
          title = "Poursuite d'\u00e9tudes", width = 12,
          tabPanel(
            "Ann\u00e9es s\u00e9lectionn\u00e9es",
            fluidRow(
              valueBoxOutput(ns("diplomes"), width = 4),
              valueBoxOutput(ns("poursuite_etudes"), width = 4),
              valueBoxOutput(ns("tx_poursuite_etudes"), width = 4)
            )
          ),
          tabPanel(
            "\u00C9volution",
            plotly::plotlyOutput(ns("poursuite_etudes_histo"))
          ),
          footer = HTML("<sup>1</sup> Sont retenus les dipl\u00f4m\u00e9s n'ayant pas interrompu deux ans ou plus leurs \u00e9tudes entre le baccalaur\u00e9at et l'obtention du dipl\u00f4me \u00e0 l'Universit\u00e9 Toulouse III - Paul Sabatier.<br>Les r\u00e9sultats pr\u00e9sent\u00e9s concernent le public assimil\u00e9 \u00e0 la formation initiale.")
        ),
        tabBox_footer(
          title = "Poursuite d'\u00e9tudes directe ou reprise d'\u00e9tudes ?", width = 12,
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
          ),
          footer = HTML("<sup>2</sup> Poursuite d'\u00e9tudes r\u00e9alis\u00e9e directement apr\u00e8s le dipl\u00f4me obtenu \u00e0 l'Universit\u00e9 Toulouse III - Paul Sabatier.<br>
        <sup>3</sup> Entr\u00e9e sur le march\u00e9 du travail pendant au moins une ann\u00e9e puis reprise d'\u00e9tudes.")
        ),
        box(
          title = "Quel est le dernier niveau de dipl\u00f4me vis\u00e9 ?", width = 12,
          uiOutput(ns("input_poursuite_etudes")),
          plotly::plotlyOutput(ns("plot_poursuite_etudes"))
        ),
        box(
          title = "Quelles raisons pour la poursuite d'\u00e9tudes ?", width = 12,
          plotly::plotlyOutput(ns("raisons_poursuite_etudes"))
        )
      )
      
    )
  )
  
  ui <- ui$children[[1]]
  
  if (golem::get_golem_options("diplome") == "DUT") {
    
    # Suppression des raisons de la poursuite d'\u00e9tudes
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
      HTML("Dipl\u00f4m\u00e9s r\u00e9pondants<sup>1<sup>"), icon = icon("user-graduate"), color = "yellow"
    )
  })
  
  output$poursuite_etudes <- renderValueBox({
    valueBox(
      nrow(rv$dt_etudes()) %>% scales::number(),
      "Poursuite d'\u00e9tudes", icon = icon("university"), color = "yellow"
    )
  })
  
  output$tx_poursuite_etudes <- renderValueBox({
    valueBox(
      scales::percent(nrow(rv$dt_etudes()) / nrow(rv$dt_reponses()), suffix = NULL),
      "Taux de poursuite d'\u00e9tudes", icon = icon("percent"), color = "yellow"
    )
  })
  
  output$poursuite_etudes_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(repondant == 1) %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate(poursuite_etudes = dplyr::if_else(parcours %in% c("Poursuite d'\u00e9tudes directe", "Reprise d'\u00e9tudes"), "oui", "non")) %>% 
      dplyr::count(annee, poursuite_etudes) %>% 
      tidyr::spread(poursuite_etudes, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non))
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Ann\u00e9e universitaire", title_y = "Taux de poursuites d'\u00e9tudes",
      hovertext = paste("Taux de poursuites d'\u00e9tudes: ", scales::percent(data$pct, suffix = "\u202F%", accuracy = 0.1, decimal.mark = ",")),
      color = "#fbca00",
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$pourcentage_poursuite_etudes_directe <- renderValueBox({
    valueBox(
      scales::percent(
        nrow(dplyr::filter(rv$dt_etudes(), parcours == "Poursuite d'\u00e9tudes directe")) / nrow(rv$dt_etudes()),
        suffix = NULL),
      HTML("Taux de poursuite d'\u00e9tudes directes<sup>2</sup>"), icon = icon("percent"), color = "yellow"
    )
  })
  
  output$pourcentage_reprise_etudes <- renderValueBox({
    valueBox(
      scales::percent(
        nrow(dplyr::filter(rv$dt_etudes(), parcours == "Reprise d'\u00e9tudes")) / nrow(rv$dt_etudes()),
        suffix = NULL),
      HTML("Taux de reprise d'\u00e9tudes<sup>3</sup>"), icon = icon("percent"), color = "yellow"
    )
  })
  
  output$effectif_poursuite_etudes_directe <- renderValueBox({
    valueBox(
      nrow(dplyr::filter(rv$dt_etudes(), parcours == "Poursuite d'\u00e9tudes directe")) %>% scales::number(big.mark = "\u202F"),
      HTML("Nombre de poursuites d'\u00e9tudes directes<sup>2</sup>"), icon = icon("users"), color = "yellow"
    )
  })
  
  output$effectif_reprise_etudes <- renderValueBox({
    valueBox(
      nrow(dplyr::filter(rv$dt_etudes(), parcours == "Reprise d'\u00e9tudes")) %>% scales::number(big.mark = "\u202F"),
      HTML("Nombre de reprises d'\u00e9tudes<sup>3</sup>"), icon = icon("users"), color = "yellow"
    )
  })
  
  output$type_poursuite_etudes_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(parcours %in% c("Poursuite d'\u00e9tudes directe", "Reprise d'\u00e9tudes")) %>% 
      dplyr::mutate_at("annee", as.character) %>% 
      dplyr::mutate_at("parcours", droplevels)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    levels(data$parcours) <- c("Poursuite d'\u00e9tudes directe<sup>2</sup>", "Reprise d'\u00e9tudes<sup>3</sup>")
    
    graphr::shiny_areas_evolution(
      data$annee, data$parcours,
      title_x = "Ann\u00e9e universitaire",
      colors = c("#af8c00", "#ffdb49"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$input_poursuite_etudes <- renderUI({
    
    selectInput(ns("filtre_type_poursuite_etudes"), label = "Type de poursuite d'\u00e9tudes :", choices = c("Poursuite d'\u00e9tudes directe", "Reprise d'\u00e9tudes"), selected = c("Poursuite d'\u00e9tudes directe", "Reprise d'\u00e9tudes"), multiple = TRUE, selectize = TRUE)
    
  })
  
  output$plot_poursuite_etudes <- plotly::renderPlotly({
    
    req(input$filtre_type_poursuite_etudes)
    
    data <- rv$dt_etudes() %>% 
      dplyr::filter(type_diplome == golem::get_golem_options("diplome")) %>% 
      dplyr::filter(parcours %in% c("Poursuite d'\u00e9tudes directe", "Reprise d'\u00e9tudes")) %>% 
      dplyr::filter(parcours %in% input$filtre_type_poursuite_etudes)
    
    graphr::shiny_treemap_bi(
      data$niveau_diplome_vise,
      data$diplome_vise, 
      colors = c("#af8c00", "#fbca00", "#ffdb49"),
      #colors = c("#434078", "#605ca8", "#918ec3"),
      font_family = golem::get_golem_options("graph_font_family")
    )

  })
  
  output$raisons_poursuite_etudes <- plotly::renderPlotly({
    
    data <- rv$dt_etudes() %>% 
      dplyr::filter(parcours == "Poursuite d'\u00e9tudes directe") %>% 
      dplyr::select(pours_etud_n_n1_raison) %>% 
      tidyr::unnest_legacy() %>% 
      tidyr::drop_na(pours_etud_n_n1_raison)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    data %>% 
      dplyr::pull(pours_etud_n_n1_raison) %>% 
      graphr::shiny_barplot_horizontal(
        colors = "#fbca00", 
        alpha = 0.8,
        font_family = golem::get_golem_options("graph_font_family")
      )

  })
  
}
