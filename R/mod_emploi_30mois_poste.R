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
      tabBox(
        title = "Insertion professionnelle à 30 mois", width = 12,
        tabPanel(
          "Années sélectionnées",
          fluidRow(
            valueBoxOutput(ns("nombre_emploi"), width = 6),
            valueBoxOutput(ns("tx_insertion_pro"), width = 6)
          )
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("tx_insertion_pro_histo"))
        )
      )
    ),
    fluidRow(
      tabBox(
        title = "Niveau d'emploi", width = 4,
        tabPanel(
          "Années sélectionnées",
          plotly::plotlyOutput(ns("emploi_30mois_niveau"))
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("emploi_30mois_niveau_histo"))
        )
      ),
      tabBox(
        title = HTML("Salaire<sup>2</sup>"), width = 4,
        tabPanel(
          "Années sélectionnées",
          fluidRow(
            valueBoxOutput(ns("emploi_30mois_salaire"), width = 12)
          )
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("emploi_30mois_salaire_histo"))
        )
      ),
      tabBox(
        title = "Type de contrat", width = 4,
        tabPanel(
          "Années sélectionnées",
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
        title = "Domaine d'emploi",
        plotly::plotlyOutput(ns("emploi_30mois_domaine"))
      ),
      box(
        title = "Intitulé d'emploi",
        htmlOutput(ns("emploi_30mois_intitule"))
      )
    ),
    htmlOutput(ns("notes"))
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
      "Nombre de diplômés en emploi à 30 mois", icon = icon("user-tie")
    )
  })
  
  output$tx_insertion_pro <- renderValueBox({
    
    recherche <- rv$dt_vad() %>% 
      dplyr::filter(situation_pro_n2 %in% c("En emploi", "En recherche d'emploi", "Promesse d'embauche"))
    
    valueBox(
      scales::percent(nrow(rv$dt_emploi_30mois()) / nrow(recherche), suffix = NULL),
      HTML("Taux d'insertion professionnelle à 30 mois<sup>1</sup>"), icon = icon("percent")
    )
    
  })
  
  output$tx_insertion_pro_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(parcours == "Vie active durable",
                    situation_pro_n2 %in% c("En emploi", "En recherche d'emploi", "Promesse d'embauche")) %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate(insertion_pro = dplyr::recode(situation_pro_n2, "En emploi" = "oui", "En recherche d'emploi" = "non", "Promesse d'embauche" = "non")) %>% 
      dplyr::count(annee, insertion_pro) %>% 
      tidyr::spread(insertion_pro, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non))
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Année universitaire", title_y = "Taux d'insertion professionnelle à 30 mois",
      hovertext = paste("Taux d'insertion professionnelle à 30 mois: ", scales::percent(data$pct, suffix = "\u202F%", accuracy = 0.1, decimal.mark = ","))
    )
    
  })
  
  output$emploi_30mois_niveau <- plotly::renderPlotly({
    
    rv$dt_emploi_30mois() %>%  
      tidyr::drop_na(emploi_n2_niveau) %>% 
      dplyr::pull(emploi_n2_niveau) %>% 
      graphr::shiny_donut(alpha = 0.67)
    
  })
  
  output$emploi_30mois_niveau_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(parcours == "Vie active durable",
                    situation_pro_n2 == "En emploi") %>% 
      tidyr::drop_na(emploi_n2_niveau) %>% 
      dplyr::mutate_at("annee", as.character)
    
    graphr::shiny_areas_evolution(data$annee, data$emploi_n2_niveau, title_x = "Année universitaire")
    
  })
  
  output$emploi_30mois_salaire <- renderValueBox({
    
    dt <- rv$dt_emploi_30mois() %>% 
      dplyr::filter(situation_pro_n2 == "En emploi",
                    emploi_n2_temoin_temps_partiel == "Non",
                    emploi_n2_departement != "99") %>% 
      tidyr::drop_na(emploi_n2_salaire)
    
    validate(
      need(nrow(dt) >= 3, "Il n'y a pas suffisamment d'observations pour afficher cette valeur.")
    )

    valueBox(
      value <- median(dt$emploi_n2_salaire) %>% 
        round() %>% 
        scales::number(big.mark = "\u202F"),
      "Salaire net médian", icon = icon("euro")
    )
    
  })
  
  output$emploi_30mois_salaire_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(parcours == "Vie active durable",
                    situation_pro_n2 == "En emploi",
                    emploi_n2_temoin_temps_partiel == "Non",
                    emploi_n2_departement != "99") %>% 
      dplyr::mutate_at("annee", as.character) %>% 
      dplyr::group_by(annee) %>% 
      dplyr::summarise(emploi_n2_salaire = median(emploi_n2_salaire, na.rm = TRUE),
                       n = dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate_at("emploi_n2_salaire", round)
    
    validate(
      need(all(data$n >= 3), "Il n'y a pas suffisamment d'observations pour tracer le graphique.")
    )
    
    graphr::shiny_line_base100(data$annee, data$emploi_n2_salaire,
                                 title_x = "Année", title_y = "Salaire net médian <sup>3</sup>",
                                 note_base100 = paste("<sup>3</sup> Base 100, année", data$annee[1]))
    
  })
  
  output$emploi_30mois_type <- plotly::renderPlotly({

    rv$dt_emploi_30mois() %>%  
      tidyr::drop_na(emploi_n2_type) %>% 
      dplyr::pull(emploi_n2_type) %>% 
      graphr::shiny_donut(alpha = 0.67)
    
  })
  
  output$emploi_30mois_type_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(parcours == "Vie active durable",
                    situation_pro_n2 == "En emploi") %>% 
      tidyr::drop_na(emploi_n2_type) %>% 
      dplyr::mutate_at("annee", as.character)
    
    graphr::shiny_areas_evolution(data$annee, data$emploi_n2_type, title_x = "Année universitaire")
    
  })
  
  output$emploi_30mois_domaine <- plotly::renderPlotly({

    validate(
      need(!is.null(rv$inputs[["filtre-donnees-formation"]]), "Au moins une formation doit être sélectionnée")
    )
    
    rv$dt_emploi_occupe() %>%
      tidyr::drop_na(emploi_n2_fonctions) %>%
      dplyr::pull(emploi_n2_fonctions) %>%
      graphr::shiny_treemap(alpha = 0.67)

  })

  output$emploi_30mois_intitule <- renderText({
    
    validate(
      need(!is.null(rv$inputs[["filtre-donnees-formation"]]), "Au moins une formation doit être sélectionnée")
    )
    
    data <- rv$dt_emploi_occupe() %>%
      tidyr::drop_na(emploi_n2_intitule) %>% 
      dplyr::pull(emploi_n2_intitule)
    
    if (length(data) > 18) {
      data <- c(sample(data, 18), "...")
    }
    
    data %>% 
      paste(collapse = "</li><li>") %>% 
      paste0("<ul><li>", ., "</li></ul>")

  })
  
  output$notes <- renderText({
    
    paste(
      c(
        "<sup>1</sup> Diplômés en emploi / Diplômés en recherche d'emploi",
        "<sup>2</sup> Primes incluses, pour un emploi à temps plein en France. Le salaire médian est celui qui coupe en deux les répondants : 50% ont un salaire inférieur et 50% un salaire supérieur."
      ),
      collapse = "<br>"
    )
    
  })
    
}
