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
            tags$img(src = "www/logo_UT3_RVB_web.png", height = "55px", width = "188px")
          ),
          style = "text-align: left; display: inline-block; width: 15%;"
        ),
        div(
          h1(
            HTML(
              paste0("<b>Devenir des dipl\u00f4m\u00e9s de ", golem::get_golem_options("diplome"), "</b>")
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
          title = HTML("<b>Les donn\u00e9es source</b>"), width = 12,
          htmlOutput(ns("source")
          )
        ),
        box(title = HTML("<b>La m\u00e9thodologie</b>"), width = 12,
            htmlOutput(ns("methodologie"))
        )
      ),
      column(
        width = 7,
        box(
          title = HTML("<b>Mode d'emploi du tableau de bord</b>"), width = 12, height = 680,
          imageOutput(ns("mode_emploi"))
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
      utils::tail(1)
    
    annee_fin <- golem::get_golem_options("data") %>% 
      dplyr::pull(annee) %>% 
      levels() %>% 
      utils::head(1)
    
    div(style = "font-size: 12pt;",
      p(HTML("Dans le cadre de l'enqu\u00eate nationale minist\u00e9rielle sur la <b>situation \u00e0 30 mois</b> des dipl\u00f4m\u00e9-e-s, l'observatoire de la vie \u00e9tudiante interroge chaque ann\u00e9e les \u00e9tudiant-e-s ayant valid\u00e9 leur <b>DUT</b>, <b>Licence professionnelle</b> ou <b>Master</b>.")),
      p("A partir des r\u00e9ponses donn\u00e9es par les anciens dipl\u00f4m\u00e9-e-s, ces enqu\u00eates ont pour objectif d\u0092informer sur les d\u00e9bouch\u00e9s accessibles \u00e0 l\u0092issue des formations propos\u00e9es par l\u0092Universit\u00e9 Toulouse III - Paul Sabatier."),
      p("Le questionnaire permet de d\u00e9crire le parcours post-dipl\u00f4me :",
        tags$ul(
          tags$li("soit la poursuite d\u0092\u00e9tudes engag\u00e9e, et donc de conna\u00eetre le dernier dipl\u00f4me vis\u00e9 apr\u00e8s l\u0092obtention du dipl\u00f4me \u00e0 UT3"),
          tags$li("soit la situation professionnelle de l\u0092\u00e9tudiant-e s\u0092\u00e9tant pr\u00e9sent\u00e9-e sur le march\u00e9 du travail : taux d\u0092insertion, caract\u00e9ristiques du poste de travail, de l\u0092employeur et ad\u00e9quation de l\u0092emploi occup\u00e9 avec le dipl\u00f4m\u00e9 obtenu")
          )
      ),
      p(HTML(glue::glue("Le temps d\u0092enqu\u00eate de 30 mois apr\u00e8s la dipl\u00f4mation explique le d\u00e9calage assez important entre l\u0092ann\u00e9e universitaire en cours et la derni\u00e8re promotion interrog\u00e9e. L'historique pr\u00e9sent\u00e9 commence \u00e0 l'ann\u00e9e {annee_debut} et la derni\u00e8re promotion interrog\u00e9e concerne les dipl\u00f4m\u00e9s de <b>l\u0092ann\u00e9e universitaire {annee_fin}</b>.")))
    )
    
  })
  
  output$methodologie <- renderUI({
    
    div(style = "font-size: 12pt;",
        p(HTML("Les chiffres pr\u00e9sent\u00e9s dans cette application correspondent au public assimil\u00e9 \u00e0 la <b>formation initiale</b>. Cela signifie que les dipl\u00f4m\u00e9s ayant interrompu leur \u00e9tudes deux ans ou plus entre le baccalaur\u00e9at et l'obention du dipl\u00f4me \u00e0 UT3 sont \u00e9cart\u00e9s de l'analyse.")),
        p("Par ailleurs, la description de la situation professionnelle des dipl\u00f4m\u00e9s concerne uniquement les \u00e9tudiants n'ayant fait aucune poursuite d'\u00e9tudes dans les 30 mois \u00e9coul\u00e9s apr\u00e8s la dipl\u00f4mation. En effet, l'analyse a pour objectif de caract\u00e9riser le pouvoir ins\u00e9rant des formations de l'Universit\u00e9 Toulouse III - Paul Sabatier.")
    )
    
  })
  
  output$mode_emploi <- renderImage(deleteFile = FALSE, {
    
    filename <- system.file('app', package = 'ip.resultats') %>% 
      list.files(pattern = paste0("home_", tolower(golem::get_golem_options("diplome")), "_draw"), full.names = TRUE)
    
    list(src = filename)
    
  })
  
}
