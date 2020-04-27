# Module UI
  
#' @title   mod_chiffres_cles_ui and mod_chiffres_cles_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_chiffres_cles
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_chiffres_cles_ui <- function(id){
  ns <- NS(id)

  ui <- tagAppendChildren(
    tagList(),
    list = list(
      
      fluidRow(
        column(
          width = 12, offset = 3,
          box(
            title = "Diplômés assimilés à la formation initiale",
            valueBoxOutput(ns("repondants_analyse"), width = 12),
            footer = HTML("<sup>1</sup> Sont retenus les diplômés n'ayant pas interrompu deux ans ou plus leurs études entre le baccalauréat et l'obtention du diplôme à l'Université Toulouse III - Paul Sabatier. Les résultats présentés concernent le public assimilé à la formation initiale.")
          )
        ),
        box(
          title = "Après l'obtention du diplôme, poursuite d'études ou emploi ?", width = 12,
          valueBoxOutput(ns("etudes"), width = 6),
          valueBoxOutput(ns("vie_active_durable"), width = 6),
          footer = HTML("<sup>2</sup> Aucune poursuite d'études pendant les 30 mois consécutifs à l'obtention du diplôme. Les inactifs sont inclus.")
        ),
        column(
          width = 6,
          fluidRow(
            box(
              title = HTML("<b>Poursuite d'études :</b> le dernier niveau de diplôme visé"), width = 12,
              plotly::plotlyOutput(ns("niveau_diplome"), height = "710px")
            )
          )
          
        ),
        column(
          width = 6,
          fluidRow(
            box(
              title = HTML("<b>Vie active durable :</b> les caractéristiques d'emploi"), width = 12,
              valueBoxOutput(ns("emploi_premier_duree_recherche"), width = 6),
              valueBoxOutput(ns("emploi_n2_insertion"), width = 6),
              box(
                title = "Emploi occupé à 30 mois", width = 12,
                column(
                  width = 6,
                  plotly::plotlyOutput(ns("emploi_30mois_niveau"))
                ),
                column(
                  width = 6,
                  valueBoxOutput(ns("salaire"), width = 12),
                  valueBoxOutput(ns("cdi_assimiles"), width = 12),
                  valueBoxOutput(ns("occitanie"), width = 12)
                )
              ),
              footer = HTML("<sup>3</sup> Diplômés en emploi / Diplômés en emploi ou en recherche d'emploi<br>
                        <sup>4</sup> Primes incluses, pour un emploi à temps plein en France. Le salaire médian est celui qui coupe en deux les répondants : 50% ont un salaire inférieur et 50% un salaire supérieur.<br>
                        <sup>5</sup> CDI, fonctionnaire, profession libérale, indépendant, chef-fe d'entreprise")
            )
          )
        )
      )
      
    )
  )
  
  ui <- ui$children[[1]]
  
  if (golem::get_golem_options("diplome") %in% c("LP", "Master")) {
    
    # Inversion VAD et PE dans le segment 2
    ui$children[[2]]$children[[1]]$children[[2]]$children <- list(
      ui$children[[2]]$children[[1]]$children[[2]]$children[[2]],
      ui$children[[2]]$children[[1]]$children[[2]]$children[[1]]
    )
    
    # Segment VAD avant segment PE
    ui$children[3:4] <- list(
      ui$children[[4]],
      ui$children[[3]]
    )
  }
  
  ui
}
    
# Module Server
    
#' @rdname mod_chiffres_cles
#' @export
#' @keywords internal
    
mod_chiffres_cles_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$repondants_analyse <- renderValueBox({
    
    valueBox(
      nrow(rv$dt_reponses()) %>% scales::number(big.mark = "\u202F"),
      HTML("Diplômés répondants<sup>1</sup>"), icon = icon("user-graduate"), color = "light-blue"
    )
    
  })
  
  output$etudes <- renderValueBox({
    
    valueBox(
      scales::percent(nrow(rv$dt_etudes()) / nrow(rv$dt_reponses()), suffix = "\u202F%"),
      subtitle = "Poursuite d'études",
      icon = icon("university"),
      color = "purple"
    )
    
  })
  
  output$vie_active_durable <- renderValueBox({
    
    valueBox(
      scales::percent(nrow(rv$dt_vad()) / nrow(rv$dt_reponses()), suffix = "\u202F%"),
      subtitle = HTML("Vie active durable<sup>2</sup>"),
      icon = icon("user-tie"),
      color = "orange"
    )
    
  })
  
  output$niveau_diplome <- plotly::renderPlotly({
    
    if (golem::get_golem_options("diplome") %in% c("DUT", "LP")) {
      levels <- c("Niveau Bac+5", "Niveau Bac+3", "Diplôme de niveau inférieur et autre")
    } else if (golem::get_golem_options("diplome") == "Master") {
      levels <- c("Doctorat", "Niveau Bac+5", "Diplôme de niveau inférieur et autre")
    }
    
    rv$dt_etudes() %>%
      dplyr::mutate_at("niveau_diplome_vise", factor, levels = levels) %>%
      tidyr::drop_na(niveau_diplome_vise) %>%
      dplyr::pull(niveau_diplome_vise) %>%
      graphr::shiny_treemap(alpha = 0.8, colors = c("#434078", "#605ca8", "#918ec3"))
    
  })
  
  output$emploi_premier_duree_recherche <- renderValueBox({
    
    emploi_premier_duree_recherche <- rv$dt_vad() %>% 
      tidyr::drop_na(emploi_premier_duree_recherche)
    
    valueBox(
      scales::percent(
        nrow(dplyr::filter(emploi_premier_duree_recherche, emploi_premier_duree_recherche <= 3)) / nrow(emploi_premier_duree_recherche), 
        suffix = "\u202F%"
        ),
      "Taux d'accès au 1er emploi en 3 mois ou moins", icon = icon("clock"), color = "orange"
    )
    
  })
  
  output$emploi_n2_insertion <- renderValueBox({
    
    emploi_n2_insertion <- rv$dt_vad() %>% 
      dplyr::filter(situation_pro_n2 %in% c("En emploi", "En recherche d'emploi", "Promesse d'embauche"))
    
    valueBox(
      scales::percent(
        nrow(dplyr::filter(emploi_n2_insertion, situation_pro_n2 == "En emploi")) / nrow(emploi_n2_insertion),
        suffix = NULL
        ),
      HTML("Taux d'insertion à 30 mois<sup>3</sup>"), icon = icon("percent"), color = "orange"
    )
    
  })
  
  output$emploi_30mois_niveau <- plotly::renderPlotly({
    
    rv$dt_vad() %>%  
      tidyr::drop_na(emploi_n2_niveau) %>% 
      dplyr::pull(emploi_n2_niveau) %>% 
      graphr::shiny_pie(
        alpha = 0.8,
        donut = TRUE,
        donut_title = "Niveau d'emploi",
        legend_position = "bottom",
        colors = c("#ce6000", "#ff7701", "#ff9335", "#ffae68")
      )
    
  })
  
  output$salaire <- renderValueBox({
    
    emploi_n2_insertion <- rv$dt_vad() %>% 
      dplyr::filter(
        situation_pro_n2 == "En emploi",
        emploi_n2_temoin_temps_partiel == "Non",
        emploi_n2_departement != "99"
      )
    
    valueBox(
      median(emploi_n2_insertion$emploi_n2_salaire, na.rm = TRUE) %>% 
        round() %>% 
        scales::number(big.mark = "\u202F"),
      HTML("Salaire net médian<sup>4</sup>"), icon = icon("euro"), color = "orange"
    )
    
  })
  
  output$cdi_assimiles <- renderValueBox({
    
    cdi_assimiles <- rv$dt_vad() %>% 
      dplyr::filter(situation_pro_n2 == "En emploi")
    
    valueBox(
      scales::percent(
        nrow(dplyr::filter(cdi_assimiles, emploi_n2_type %in% c("CDI", "Profession libérale, indépendant, chef-fe d'entreprise, auto-entrepreneur", "Fonctionnaire"))) / nrow(cdi_assimiles),
        suffix = NULL
      ),
      HTML("Taux de CDI et assimilés<sup>5</sup>"), icon = icon("percent"), color = "orange"
    )
    
  })
  
  output$occitanie <- renderValueBox({
    
    occitanie <- rv$dt_vad() %>% 
      dplyr::filter(situation_pro_n2 == "En emploi")
    
    valueBox(
      scales::percent(
        nrow(dplyr::filter(occitanie, emploi_n2_departement %in% c("009", "011", "012", "030", "031", "032", "034", "046", "048", "065", "066", "081", "082"))) / nrow(occitanie), 
        suffix = "\u202F%"
      ),
      HTML("Taux d'emploi en région Occitanie"), icon = icon("map-marker-alt"), color = "orange"
    )
    
  })

}
