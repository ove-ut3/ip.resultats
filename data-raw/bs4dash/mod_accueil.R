# Module UI
  
#' @title   mod_accueil_ui and mod_accueil_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_accueil
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_accueil_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      div("", style = "height: 40px;"),
      div(style = "margin-left: 15px; margin-right: 10px;",
        div(a(href = "http://www.univ-tlse3.fr",
              tags$img(src = "https://upload.wikimedia.org/wikipedia/fr/a/a4/Logo_UT3.jpg", height = "55px", width = "163px")), style = "text-align: left; display: inline-block; width: 15%;"),
        div(h1("Enquêtes d'insertion professionnelle des diplômés"), style = "text-align: center; display: inline-block; width: 70%;"),
        div(a(href = "http://www.univ-tlse3.fr/devenir-des-diplomes/",
              tags$img(src = "http://www.univ-tlse3.fr/medias/photo/img_1290420257773.jpg", height = "55px", width = "150px")), style = "text-align: right; display: inline-block; width: 14%;")
        
      ),
      div("", style = "height: 20px;"),
      column(width = 6,
             box(title = "Les données source", width = 12,
                 textOutput(ns("source"))
             ),
             box(title = "La méthodologie", width = 12,
                 textOutput(ns("methodologie"))
             )
      ),
      column(width = 6,
             box(title = "Mode d'emploi du tableau de bord", width = 12,
                 textOutput(ns("mode_emploi"))
             )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_accueil
#' @export
#' @keywords internal
    
mod_accueil_server <- function(input, output, session){
  ns <- session$ns
  
  # Source
  # Enquête ministérielle
  # nationale
  # Enquête à 30 mois : exemple les derniers diplômés interrogés au 1er décembre 2018 sont ceux de la promotion 2015-16
  # Seuls les diplômés sont concernés
  # Type de diplôme concernés : professionnalisant
  
  # Méthodologie
  # Service de traitement OVE
  # Enquête par email et téléphone du 1er décembre au 31 mars
  # Résultats : Approche formation initiale == Diplômés de moins de 30 ans sans interruption d'études de plus de 2 ans entre le bac et le diplôme UT3
  
  # Mode d'emploi
  # Menus à gauche
  # Filtres à droite (ou à gauche si déplacement)
  # Type_diplome parfois obligatoire
  
  output$source <- renderText({
    ""
  })
  
  output$methodologie <- renderText({
    ""
  })
  
  output$mode_emploi <- renderText({
    ""
  })

  
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
