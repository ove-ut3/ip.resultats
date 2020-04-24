# Module UI

#' @title   mod_diplomes_ui and mod_diplomes_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_diplomes
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_diplomes_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        title = "Diplômés", width = 12, offset = 3,
        tabBox(
          tabPanel(
            "Années sélectionnées",
            fluidRow(
              valueBoxOutput(ns("diplomes"), width = 12)
            )
          ),
          tabPanel(
            "\u00C9volution",
            plotly::plotlyOutput(ns("diplomes_histo"))
          )
        )
      )
    ),
    fluidRow(
      tabBox(
        title = "Sexe",
        tabPanel(
          "Années sélectionnées",
          fluidRow(
            valueBoxOutput(ns("femmes"), width = 12)
          )
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("femmes_histo"))
        )
      ),
      tabBox(
        title = "Régime d'inscription",
        tabPanel(
          "Années sélectionnées",
          plotly::plotlyOutput(ns("regime"))
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("regime_histo"))
        )
      )
    ),
    fluidRow(
      tabBox(
        title = "Nationalité",
        tabPanel(
          "Années sélectionnées",
          fluidRow(
            valueBoxOutput(ns("etranger"), width = 12)
          )
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("etranger_histo"))
        )
      ),
      tabBox(
        title = "Boursier",
        tabPanel(
          "Années sélectionnées",
          fluidRow(
            valueBoxOutput(ns("boursier"), width = 12)
          )
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("boursier_histo"))
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_diplomes
#' @export
#' @keywords internal

mod_diplomes_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$diplomes <- renderValueBox({
    
    valueBox(
      nrow(rv$dt_reponses()) %>% scales::number(big.mark = "\u202F"),
      "Nombre de diplômés", icon = icon("user-graduate")
    )
    
  })
  
  output$diplomes_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::mutate_at("annee", as.character) %>% 
      dplyr::count(annee)
    
    graphr::shiny_line_base100(
      data$annee, data$n,
      title_x = "Année universitaire", title_y = "Nombre de diplômés <sup>1</sup>",
      note_base100 = paste("<sup>1</sup> Base 100, année", data$annee[1])
    )
    
    
  })
  
  output$femmes <- renderValueBox({
    
    valueBox(
      scales::percent(
        nrow(dplyr::filter(rv$dt_reponses(), sexe == "F")) / nrow(rv$dt_reponses()), 
        suffix = "\u202F%"
      ),
      "Taux de femmes", icon = icon("female")
    )
    
  })
  
  output$femmes_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::count(annee, sexe) %>% 
      tidyr::spread(sexe, n, fill = 0) %>% 
      dplyr::mutate(pct = `F` / (`F` + M))
    
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Année universitaire", title_y = "Taux de femmes",
      hovertext = paste("Taux de femmes: ", scales::percent(data$pct, suffix = "\u202F%", accuracy = 0.1, decimal.mark = ","))
    )

  })
  
  output$regime <- plotly::renderPlotly({
    
    rv$dt_reponses() %>% 
      dplyr::pull(regime_inscription) %>% 
      graphr::shiny_pie(alpha = 0.67, donut = TRUE, donut_title = "Régime d'inscription")
    
  })
  
  output$regime_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::mutate_at("annee", as.character)
    
    graphr::shiny_areas_evolution(data$annee, data$regime_inscription, title_x = "Année universitaire")
    
  })
  
  output$etranger <- renderValueBox({
    
    valueBox(
      scales::percent(
        nrow(dplyr::filter(rv$dt_reponses(), code_nationalite != "100")) / nrow(rv$dt_reponses()), 
        suffix = "\u202F%"
      ),
      "Taux de nationalité étrangère", icon = icon("globe")
    )
    
  })
  
  output$etranger_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate(nationalite_etr = dplyr::if_else(code_nationalite != "100", "oui", "non")) %>% 
      dplyr::count(annee, nationalite_etr) %>% 
      tidyr::spread(nationalite_etr, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non))
    
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Année universitaire", title_y = "Taux de nationalité étrangère",
      hovertext = paste("Taux de nationalité étrangère: ", scales::percent(data$pct, suffix = "\u202F%", accuracy = 0.1, decimal.mark = ","))
    )
    
  })
  
  output$boursier <- renderValueBox({
    
    valueBox(
      scales::percent(
        nrow(dplyr::filter(rv$dt_reponses(), !is.na(code_bourse))) / nrow(rv$dt_reponses()), 
        suffix = "\u202F%"
      ),
      "Taux de boursiers", icon = icon("percent")
    )
    
  })
  
  output$boursier_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate(boursier = dplyr::if_else(!is.na(code_bourse), "oui", "non")) %>% 
      dplyr::count(annee, boursier) %>% 
      tidyr::spread(boursier, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non))
    
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Année universitaire", title_y = "Taux de boursiers",
      hovertext = paste("Taux de boursiers: ", scales::percent(data$pct, suffix = "\u202F%", accuracy = 0.1, decimal.mark = ","))
    )
    
  })
  
  
}
