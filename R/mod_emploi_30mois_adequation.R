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
          title = "Dipl\u00f4m\u00e9s en emploi",
          valueBoxOutput(ns("nombre_emploi"), width = 12)
        )
      )
    ),
    fluidRow(
      tabBox(
        title = "Ad\u00e9quation de l'emploi \u00e0 30 mois",
        tabPanel(
          "Ann\u00e9es s\u00e9lectionn\u00e9es",
          plotly::plotlyOutput(ns("emploi_30mois_adequation"))
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("emploi_30mois_adequation_histo"))
        )
      ),
      tabBox(
        title = "Satisfaction dans l'emploi \u00e0 30 mois",
        tabPanel(
          "Ann\u00e9es s\u00e9lectionn\u00e9es",
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
      nrow(rv$dt_emploi_30mois()) %>% scales::number(big.mark = "\u202F"),
      "Nombre de dipl\u00f4m\u00e9s en emploi \u00e0 30 mois", icon = icon("user-tie"), color = "black"
    )
  })
  
  output$emploi_30mois_adequation <- plotly::renderPlotly({
    
    data <- rv$dt_emploi_30mois() %>% 
      dplyr::select(emploi_n2_adequation_niveau, emploi_n2_adequation_spe) %>% 
      tidyr::gather("champ", "valeur", na.rm = TRUE) %>% 
      dplyr::mutate_at(
        "champ", dplyr::recode, 
        "emploi_n2_adequation_niveau" = "Niveau d'\u00e9tudes",
        "emploi_n2_adequation_spe" = "Sp\u00e9cialit\u00e9 du dipl\u00f4me"
      ) %>% 
      dplyr::mutate_at("champ", factor, levels = c("Niveau d'\u00e9tudes", "Sp\u00e9cialit\u00e9 du dipl\u00f4me")) %>% 
      dplyr::mutate_at("valeur", factor, levels = c("Tout \u00e0 fait", "Plut\u00f4t", "Peu", "Pas du tout"))
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_barplot_horizontal_multi(
      data$champ, data$valeur,
      colors = c("#313131", "#4b4b4b", "#646464", "#7e7e7e"),
      alpha = 0.8,
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$emploi_30mois_adequation_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(
        parcours == "Vie active durable",
        situation_pro_n2 == "En emploi"
      ) %>% 
      dplyr::select(annee, emploi_n2_adequation_niveau, emploi_n2_adequation_spe) %>% 
      tidyr::gather("champ", "valeur", -annee, na.rm = TRUE) %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate_at(
        "champ", dplyr::recode, 
        "emploi_n2_adequation_niveau" = "Niveau d'\u00e9tudes",
        "emploi_n2_adequation_spe" = "Sp\u00e9cialit\u00e9 du dipl\u00f4me"
      ) %>% 
      dplyr::mutate_at("champ", factor, levels = c("Niveau d'\u00e9tudes", "Sp\u00e9cialit\u00e9 du dipl\u00f4me")) %>% 
      dplyr::mutate(adequation_ok = dplyr::if_else(valeur %in% c("Tout \u00e0 fait", "Plut\u00f4t"), "oui", "non")) %>% 
      dplyr::count(annee, champ, adequation_ok) %>% 
      tidyr::spread(adequation_ok, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non) * 100)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_line_percent_multi(
      data$annee, data$champ, data$pct,
      title_x = "Ann\u00e9e universitaire", 
      title_y = "Taux d'ad\u00e9quation (tout \u00e0 fait ou plut\u00f4t)",
      colors = c("#313131", "#4b4b4b", "#646464", "#7e7e7e"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$emploi_30mois_satisfaction <- plotly::renderPlotly({
    
    data <- rv$dt_emploi_30mois() %>% 
      dplyr::select(dplyr::starts_with("emploi_n2_satis")) %>% 
      tidyr::gather("champ", "valeur", na.rm = TRUE) %>% 
      dplyr::mutate_at(
        "champ", 
        dplyr::recode, 
        "emploi_n2_satis_missions" = "Nature des missions",
        "emploi_n2_satis_resp" = "Niveau de responsabilit\u00e9",
        "emploi_n2_satis_salaire" = "Montant du salaire"
      ) %>% 
      dplyr::mutate_at("champ", factor, levels = c("Nature des missions", "Niveau de responsabilit\u00e9", "Montant du salaire")) %>% 
      dplyr::mutate_at("valeur", factor, levels = c("Tout \u00e0 fait", "Plut\u00f4t", "Peu", "Pas du tout"))
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_barplot_horizontal_multi(
      data$champ, data$valeur, 
      alpha = 0.8,
      colors = c("#313131", "#4b4b4b", "#646464", "#7e7e7e"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$emploi_30mois_satisfaction_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(
        parcours == "Vie active durable",
        situation_pro_n2 == "En emploi"
      ) %>% 
      dplyr::select(annee, dplyr::starts_with("emploi_n2_satis")) %>% 
      tidyr::gather("champ", "valeur", -annee, na.rm = TRUE) %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate_at(
        "champ", dplyr::recode, 
        "emploi_n2_satis_missions" = "Nature des missions",
        "emploi_n2_satis_resp" = "Niveau de responsabilit\u00e9",
        "emploi_n2_satis_salaire" = "Montant du salaire"
      ) %>% 
      dplyr::mutate_at("champ", factor, levels = c("Nature des missions", "Niveau de responsabilit\u00e9", "Montant du salaire")) %>% 
      dplyr::mutate(satisfaction_ok = dplyr::if_else(valeur %in% c("Tout \u00e0 fait", "Plut\u00f4t"), "oui", "non")) %>% 
      dplyr::count(annee, champ, satisfaction_ok) %>% 
      tidyr::spread(satisfaction_ok, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non) * 100)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_line_percent_multi(
      data$annee, data$champ, data$pct,
      title_x = "Ann\u00e9e universitaire", 
      title_y = "Taux de satisfaction (tout \u00e0 fait ou plut\u00f4t)",
      colors = c("#313131", "#4b4b4b", "#646464", "#7e7e7e"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
}
