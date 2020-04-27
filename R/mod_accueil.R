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
      div(
        style = "margin-left: 30px; margin-right: 10px;",
        div(
          a(
            href = "http://www.univ-tlse3.fr",
            tags$img(src = "https://upload.wikimedia.org/wikipedia/fr/a/a4/Logo_UT3.jpg", height = "55px", width = "163px")
          ),
          style = "text-align: left; display: inline-block; width: 15%;"
        ),
        div(
          h1(
            HTML(
              paste0("<b>Devenir des diplômés de ", golem::get_golem_options("diplome"), "</b>")
            )
          ),
          style = "text-align: center; display: inline-block; width: 70%;"),
        div(
          a(
            href = "http://www.univ-tlse3.fr/devenir-des-diplomes/",
            tags$img(src = "http://www.univ-tlse3.fr/medias/photo/img_1290420257773.jpg", height = "55px", width = "150px")
          ),
          style = "text-align: right; display: inline-block; width: 13%;"
        )
      )
    ),
    div("", style = "height: 20px;"),
    fluidRow(
      column(
        width = 5,
        box(
          title = HTML("<b>Les données source</b>"), width = 12,
          htmlOutput(ns("source")
          )
        ),
        box(title = HTML("<b>La méthodologie</b>"), width = 12,
            htmlOutput(ns("methodologie"))
        )
      ),
      column(
        width = 7,
        box(
          title = HTML("<b>Mode d'emploi du tableau de bord</b>"), width = 12, height = 650,
          imageOutput(ns("mode_emploi"), width = "47%")
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
  
  output$source <- renderUI({
    
    annee_debut <- golem::get_golem_options("data") %>% 
      dplyr::pull(annee) %>% 
      levels() %>% 
      tail(1)
    
    annee_fin <- golem::get_golem_options("data") %>% 
      dplyr::pull(annee) %>% 
      levels() %>% 
      head(1)
    
    div(style = "font-size: 12pt;",
      p(HTML("Dans le cadre de l'enquête nationale ministérielle sur la <b>situation à 30 mois</b> des diplômé-e-s, l'observatoire de la vie étudiante interroge chaque année les étudiant-e-s ayant validé leur <b>DUT</b>, <b>Licence professionnelle</b> ou <b>Master</b>.")),
      p("A partir des réponses données par les anciens diplômé-e-s, ces enquêtes ont pour objectif d’informer sur les débouchés accessibles à l’issue des formations proposées par l’Université Toulouse III - Paul Sabatier."),
      p("Le questionnaire permet de décrire le parcours post-diplôme :",
        tags$ul(
          tags$li("soit la poursuite d’études engagée, et donc de connaître le dernier diplôme visé après l’obtention du diplôme à UT3"),
          tags$li("soit la situation professionnelle de l’étudiant-e s’étant présenté-e sur le marché du travail : taux d’insertion, caractéristiques du poste de travail, de l’employeur et adéquation de l’emploi occupé avec le diplômé obtenu")
          )
      ),
      p(HTML(glue::glue("Le temps d’enquête de 30 mois après la diplômation explique le décalage assez important entre l’année universitaire en cours et la dernière promotion interrogée. L'historique présenté commence à l'année {annee_debut} et la dernière promotion interrogée concerne les diplômés de <b>l’année universitaire {annee_fin}</b>.")))
    )
    
  })
  
  output$methodologie <- renderUI({
    
    div(style = "font-size: 12pt;",
        p(HTML("Les chiffres présentés dans cette application correspondent au public assimilé à la <b>formation initiale</b>. Cela signifie que les diplômés ayant interrompu leur études deux ans ou plus entre le baccalauréat et l'obention du diplôme à UT3 sont écartés de l'analyse.")),
        p("Par ailleurs, la description de la situation professionnelle des diplômés concerne uniquement les étudiants n'ayant fait aucune poursuite d'études dans les 30 mois écoulés après la diplômation. En effet, l'analyse a pour objectif de caractériser le pouvoir insérant des formations de l'Université Toulouse III - Paul Sabatier.")
    )
    
  })
  
  output$mode_emploi <- renderImage(deleteFile = FALSE, {
    
    filename <- system.file('app', package = 'ip.resultats') %>% 
      list.files(pattern = "home_new_draw_47\\.png$", full.names = TRUE)
    
    list(src = filename)
    
  })
  
}
