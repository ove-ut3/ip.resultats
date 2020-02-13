#' @import bs4Dash shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    bs4DashPage(
      navbar = bs4DashNavbar(
        status = "white",
        div("Enquêtes d'insertion professionnelle des diplômés", style = "font-size: 22px;")
      ),
      sidebar = bs4DashSidebar(
        tags$head(
          tags$style(
            #HTML(".layout-top-nav { position: fixed; width: 10%; }"),
            HTML(".main-header { position: fixed; width: 50%; }"),
            HTML(".sidebar { position: fixed; width: 230px; }"),
            HTML(".control-sidebar { position: fixed; }")
          )
        ),
        # tags$script('window.onload = function() {
        #           function fixBodyHeight() {
        #           var el = $(document.getElementsByClassName("content-wrapper")[0]);
        #           var h = el.height();
        #           el.css("min-height", h + 50 + "px");
        #           };
        #           window.addEventListener("resize", fixBodyHeight);
        #           fixBodyHeight();};'),
        skin = "light",
        status = "primary",
        title = tags$a(href = "http://www.univ-tlse3.fr",
                               tags$img(src = "https://upload.wikimedia.org/wikipedia/fr/a/a4/Logo_UT3.jpg", height = "55px", width = "163px")),
        brandColor = "primary",
        url = "http://www.univ-tlse3.fr",
        #src = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
        elevation = 3,
        opacity = 0.8,
        bs4SidebarMenu(
          bs4SidebarMenuItem("Accueil", tabName = "tab_home", icon = "home"),
          bs4SidebarMenuItem("Chiffres-clés", tabName = "tab_synthese", icon = "tachometer-alt"),
          bs4SidebarMenuItem("Diplômés", tabName = "tab_diplomes", icon = "user-graduate"),
          bs4SidebarMenuItem("Taux de réponse", tabName = "tab_repondants", icon = "clipboard-check"),
          bs4SidebarMenuItem("Poursuite d'études", tabName = "tab_poursuite_etudes", icon = "university"),
          bs4SidebarMenuItem("Vie active durable", tabName = "tab_vie_active durable", icon = "building", startExpanded = TRUE,
                             bs4SidebarMenuSubItem("Accès au premier emploi", tabName = "tab_emploi", icon = "user-tie"),
                             bs4SidebarMenuSubItem("Emploi à 30 mois", tabName = "tab_emploi", icon = "user-tie"),
                             bs4SidebarMenuSubItem("Employeur à 30 mois", tabName = "tab_employeur", icon = "building"),
                             bs4SidebarMenuSubItem("Satisfaction à 30 mois", tabName = "tab_adequation", icon = "smile"))
          
        )
      ),
      bs4DashBody(
        bs4TabItems(
          bs4TabItem(tabName = "tab_home",
                     mod_accueil_ui("accueil_ui_1")
          ),
          bs4TabItem(tabName = "tab_synthese",
                     mod_synthese_ui("synthese_ui_1")
          ),
          bs4TabItem(tabName = "tab_diplomes",
                     mod_diplomes_ui("diplomes_ui_1")
          )
          
        )
      ),
      controlbar = bs4DashControlbar(
        .items = mod_filtre_right_side_bar_ui("filtre_right_side_bar_ui_1")
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'ip.resultats')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
