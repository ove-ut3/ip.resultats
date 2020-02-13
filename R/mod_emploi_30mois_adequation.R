# Module UI
  
#' @title   mod_emploi_30mois_adequation_ui and mod_emploi_30mois_adequation_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_emploi_30mois_adequation
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_emploi_30mois_adequation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12, offset = 3,
        box(
          title = "Diplômés en emploi",
          valueBoxOutput(ns("nombre_emploi"), width = 12)
        )
      )
    ),
    fluidRow(
      tabBox(
        title = "Adéquation de l'emploi à 30 mois",
        tabPanel(
          "Valeur",
          plotly::plotlyOutput(ns("emploi_30mois_adequation"))
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("emploi_30mois_adequation_histo"))
        )
      ),
      tabBox(
        title = "Satisfaction dans l'emploi à 30 mois",
        tabPanel(
          "Valeur",
          plotly::plotlyOutput(ns("emploi_30mois_satisfaction"))
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("emploi_30mois_satisfaction_histo"))
        )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_emploi_30mois_adequation
#' @export
#' @keywords internal
    
mod_emploi_30mois_adequation_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$nombre_emploi <- renderValueBox({
    valueBox(
      nrow(rv$dt_emploi_30mois()) %>% caractr::str_number_fr(),
      "Nombre de diplômés en emploi à 30 mois", icon = icon("user-tie")
    )
  })
  
  output$emploi_30mois_adequation <- plotly::renderPlotly({
    
    data <- rv$dt_emploi_30mois() %>% 
      dplyr::select(emploi_n2_adequation_niveau, emploi_n2_adequation_spe) %>% 
      tidyr::gather("champ", "valeur", na.rm = TRUE) %>% 
      dplyr::mutate_at("champ", dplyr::recode, 
                       "emploi_n2_adequation_niveau" = "Niveau d'études",
                       "emploi_n2_adequation_spe" = "Spécialité du diplôme") %>% 
      dplyr::mutate_at("champ", factor, levels = c("Niveau d'études", "Spécialité du diplôme")) %>% 
      dplyr::mutate_at("valeur", factor, levels = c("Tout à fait", "Plutôt", "Peu", "Pas du tout"))
    
    graphr::shiny_barplot_horizontal_multi(data$champ, data$valeur, alpha = 0.67)
    
  })
  
  output$emploi_30mois_adequation_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(parcours == "Vie active durable",
                    situation_pro_n2 == "En emploi") %>% 
      dplyr::select(annee, emploi_n2_adequation_niveau, emploi_n2_adequation_spe) %>% 
      tidyr::gather("champ", "valeur", -annee, na.rm = TRUE) %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate_at("champ", dplyr::recode, 
                       "emploi_n2_adequation_niveau" = "Niveau d'études",
                       "emploi_n2_adequation_spe" = "Spécialité du diplôme") %>% 
      dplyr::mutate_at("champ", factor, levels = c("Niveau d'études", "Spécialité du diplôme")) %>% 
      dplyr::mutate(adequation_ok = dplyr::if_else(valeur %in% c("Tout à fait", "Plutôt"), "oui", "non")) %>% 
      dplyr::count(annee, champ, adequation_ok) %>% 
      tidyr::spread(adequation_ok, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non) * 100)
    
    
    graphr::shiny_line_percent_multi(
      data$annee, data$champ, data$pct,
      title_x = "Année universitaire", 
      title_y = "Taux d'adéquation (tout à fait ou plutôt)"
    )
    
  })
  
  output$emploi_30mois_satisfaction <- plotly::renderPlotly({
    
    data <- rv$dt_emploi_30mois() %>% 
      dplyr::select(dplyr::starts_with("emploi_n2_satis")) %>% 
      tidyr::gather("champ", "valeur", na.rm = TRUE) %>% 
      dplyr::mutate_at("champ", dplyr::recode, 
                       "emploi_n2_satis_missions" = "Nature des missions",
                       "emploi_n2_satis_resp" = "Niveau de responsabilité",
                       "emploi_n2_satis_salaire" = "Montant du salaire") %>% 
      dplyr::mutate_at("champ", factor, levels = c("Nature des missions", "Niveau de responsabilité", "Montant du salaire")) %>% 
      dplyr::mutate_at("valeur", factor, levels = c("Tout à fait", "Plutôt", "Peu", "Pas du tout"))
    
    graphr::shiny_barplot_horizontal_multi(data$champ, data$valeur, alpha = 0.67)
    
  })
  
  output$emploi_30mois_satisfaction_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(parcours == "Vie active durable",
                    situation_pro_n2 == "En emploi") %>% 
      dplyr::select(annee, dplyr::starts_with("emploi_n2_satis")) %>% 
      tidyr::gather("champ", "valeur", -annee, na.rm = TRUE) %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate_at("champ", dplyr::recode, 
                       "emploi_n2_satis_missions" = "Nature des missions",
                       "emploi_n2_satis_resp" = "Niveau de responsabilité",
                       "emploi_n2_satis_salaire" = "Montant du salaire") %>% 
      dplyr::mutate_at("champ", factor, levels = c("Nature des missions", "Niveau de responsabilité", "Montant du salaire")) %>% 
      dplyr::mutate(satisfaction_ok = dplyr::if_else(valeur %in% c("Tout à fait", "Plutôt"), "oui", "non")) %>% 
      dplyr::count(annee, champ, satisfaction_ok) %>% 
      tidyr::spread(satisfaction_ok, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non) * 100)
    
    
    graphr::shiny_line_percent_multi(
      data$annee, data$champ, data$pct,
      title_x = "Année universitaire", 
      title_y = "Taux de satisfaction (tout à fait ou plutôt)"
    )
    
  })
  
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
