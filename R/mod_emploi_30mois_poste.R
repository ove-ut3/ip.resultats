# Module UI
  
#' @title   mod_emploi_30mois_poste_ui and mod_emploi_30mois_poste_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_emploi_30mois_poste
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_emploi_30mois_poste_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      tabBox_footer(
        title = "Insertion professionnelle \u00e0 30 mois", width = 12,
        tabPanel(
          "Ann\u00e9es s\u00e9lectionn\u00e9es",
          fluidRow(
            valueBoxOutput(ns("nombre_emploi"), width = 6),
            valueBoxOutput(ns("tx_insertion_pro"), width = 6)
          )
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("tx_insertion_pro_histo"))
        ),
        footer = HTML("<sup>1</sup> Dipl\u00f4m\u00e9s en emploi / Dipl\u00f4m\u00e9s en recherche d'emploi")
      )
    ),
    fluidRow(
      tabBox(
        title = "Niveau d'emploi", width = 4,
        tabPanel(
          "Ann\u00e9es s\u00e9lectionn\u00e9es",
          plotly::plotlyOutput(ns("emploi_30mois_niveau"))
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("emploi_30mois_niveau_histo"))
        )
      ),
      tabBox_footer(
        title = HTML("Salaire<sup>2</sup>"), width = 4,
        tabPanel(
          "Ann\u00e9es s\u00e9lectionn\u00e9es",
          fluidRow(
            valueBoxOutput(ns("emploi_30mois_salaire"), width = 12)
          )
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("emploi_30mois_salaire_histo"))
        ),
        footer = HTML("<sup>2</sup> Primes incluses, pour un emploi \u00e0 temps plein en France. Le salaire m\u00e9dian est celui qui coupe en deux les r\u00e9pondants : 50% ont un salaire inf\u00e9rieur et 50% un salaire sup\u00e9rieur.")
      ),
      tabBox(
        title = "Type de contrat", width = 4,
        tabPanel(
          "Ann\u00e9es s\u00e9lectionn\u00e9es",
          plotly::plotlyOutput(ns("emploi_30mois_type"))
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("emploi_30mois_type_histo"))
        )
      )
    ),
    fluidRow(
      box(
        width = 8,
        title = "Domaine d'emploi",
        plotly::plotlyOutput(ns("emploi_30mois_domaine"))
      ),
      box(
        width = 4,
        title = "Intitul\u00e9 d'emploi",
        htmlOutput(ns("emploi_30mois_intitule"))
      )
    )
  )
  
}
    
# Module Server
    
#' @rdname mod_emploi_30mois_poste
#' @export
#' @keywords internal
    
mod_emploi_30mois_poste_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$nombre_emploi <- renderValueBox({
    valueBox(
      nrow(rv$dt_emploi_30mois()) %>% scales::number(big.mark = "\u202F"),
      "Nombre de dipl\u00f4m\u00e9s en emploi \u00e0 30 mois", icon = icon("user-tie"), color = "black"
    )
  })
  
  output$tx_insertion_pro <- renderValueBox({
    
    recherche <- rv$dt_vad() %>% 
      dplyr::filter(.data$situation_pro_n2 %in% c("En emploi", "En recherche d'emploi", "Promesse d'embauche"))
    
    valueBox(
      scales::percent(nrow(rv$dt_emploi_30mois()) / nrow(recherche), suffix = NULL),
      HTML("Taux d'insertion professionnelle \u00e0 30 mois<sup>1</sup>"), icon = icon("percent"), color = "black"
    )
    
  })
  
  output$tx_insertion_pro_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(
        .data$parcours == "Vie active durable",
        .data$situation_pro_n2 %in% c("En emploi", "En recherche d'emploi", "Promesse d'embauche")
      ) %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate(insertion_pro = dplyr::recode(.data$situation_pro_n2, "En emploi" = "oui", "En recherche d'emploi" = "non", "Promesse d'embauche" = "non")) %>% 
      dplyr::count(.data$annee, .data$insertion_pro) %>% 
      tidyr::spread(.data$insertion_pro, .data$n, fill = 0) %>% 
      dplyr::mutate(pct = .data$oui / (.data$oui + .data$non))
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Ann\u00e9e universitaire", title_y = "Taux d'insertion professionnelle \u00e0 30 mois",
      hovertext = paste("Taux d'insertion professionnelle \u00e0 30 mois: ", scales::percent(data$pct, suffix = "\u202F%", accuracy = 0.1, decimal.mark = ",")), 
      color = "#585858",
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$emploi_30mois_niveau <- plotly::renderPlotly({
    
    data <- rv$dt_emploi_30mois() %>%  
      tidyr::drop_na(.data$emploi_n2_niveau)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    data %>% 
      dplyr::pull(.data$emploi_n2_niveau) %>% 
      graphr::shiny_pie(
        alpha = 0.8, 
        donut = TRUE, 
        colors = c("#313131", "#4b4b4b", "#646464", "#7e7e7e"),
        font_family = golem::get_golem_options("graph_font_family")
      )
    
  })
  
  output$emploi_30mois_niveau_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(
        .data$parcours == "Vie active durable",
        .data$situation_pro_n2 == "En emploi"
      ) %>% 
      tidyr::drop_na(.data$emploi_n2_niveau) %>% 
      dplyr::mutate_at("annee", as.character)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_areas_evolution(
      data$annee, data$emploi_n2_niveau,
      title_x = "Ann\u00e9e universitaire",
      colors = c("#313131", "#4b4b4b", "#646464", "#7e7e7e"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$emploi_30mois_salaire <- renderValueBox({
    
    data <- rv$dt_emploi_30mois() %>% 
      dplyr::filter(
        .data$situation_pro_n2 == "En emploi",
        .data$emploi_n2_temoin_temps_partiel == "Non",
        .data$emploi_n2_departement != "99"
      ) %>% 
      tidyr::drop_na(.data$emploi_n2_salaire)
    
    validate(
      need(nrow(data) >= 3, "Il n'y a pas suffisamment d'observations pour afficher cette valeur.")
    )

    valueBox(
      value <- stats::median(data$emploi_n2_salaire) %>% 
        round() %>% 
        scales::number(big.mark = "\u202F"),
      "Salaire net m\u00e9dian", icon = icon("euro"), color = "black"
    )
    
  })
  
  output$emploi_30mois_salaire_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(
        .data$parcours == "Vie active durable",
        .data$situation_pro_n2 == "En emploi",
        .data$emploi_n2_temoin_temps_partiel == "Non",
        .data$emploi_n2_departement != "99"
      ) %>% 
      dplyr::mutate_at("annee", as.character) %>% 
      dplyr::group_by(.data$annee) %>% 
      dplyr::summarise(
        emploi_n2_salaire = stats::median(.data$emploi_n2_salaire, na.rm = TRUE),
        n = dplyr::n()
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate_at("emploi_n2_salaire", round)
    
    validate(
      need(all(data$n >= 3), "Il n'y a pas suffisamment d'observations pour tracer le graphique."),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_line_base100(
      data$annee, data$emploi_n2_salaire,
      title_x = "Ann\u00e9e", title_y = "Salaire net m\u00e9dian <sup>3</sup>",
      note_base100 = paste("<sup>3</sup> Base 100, ann\u00e9e", data$annee[1]),
      color = "#585858",
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$emploi_30mois_type <- plotly::renderPlotly({

    data <- rv$dt_emploi_30mois() %>%  
      tidyr::drop_na(.data$emploi_n2_type)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    data %>% 
      dplyr::pull(.data$emploi_n2_type) %>% 
      graphr::shiny_barplot_horizontal(
        color = c("#313131", "#4b4b4b", "#646464", "#7e7e7e", "#9a9a9a"),
        alpha = 0.8,
        font_family = golem::get_golem_options("graph_font_family")
      )
    
  })
  
  output$emploi_30mois_type_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(
        .data$parcours == "Vie active durable",
        .data$situation_pro_n2 == "En emploi"
      ) %>% 
      tidyr::drop_na(.data$emploi_n2_type) %>% 
      dplyr::mutate_at("annee", as.character)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_areas_evolution(
      data$annee, data$emploi_n2_type,
      title_x = "Ann\u00e9e universitaire",
      colors = c("#313131", "#4b4b4b", "#646464", "#7e7e7e"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$emploi_30mois_domaine <- plotly::renderPlotly({

    validate(
      need(!is.null(rv$inputs[["filtre-donnees-formation"]]), "Au moins une formation doit \u00eatre s\u00e9lectionn\u00e9e")
    )
    
    data <- rv$dt_emploi_occupe() %>%
      tidyr::drop_na(.data$emploi_n2_fonctions)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    data %>% 
      dplyr::pull(.data$emploi_n2_fonctions) %>%
      graphr::shiny_barplot_horizontal(
        colors = "#585858", 
        alpha = 0.8,
        font_family = golem::get_golem_options("graph_font_family")
      )

  })

  output$emploi_30mois_intitule <- renderText({
    
    validate(
      need(!is.null(rv$inputs[["filtre-donnees-formation"]]), "Au moins une formation doit \u00eatre s\u00e9lectionn\u00e9e")
    )
    
    data <- rv$dt_emploi_occupe() %>%
      tidyr::drop_na(.data$emploi_n2_intitule)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    data <- dplyr::pull(data, .data$emploi_n2_intitule)
    
    if (length(data) > 18) {
      data <- c(sample(data, 18), "...")
    }
    
    data %>% 
      paste(collapse = "</li><li>") %>% 
      paste0("<ul><li>", ., "</li></ul>")

  })
  
}
