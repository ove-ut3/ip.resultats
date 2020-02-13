# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module( name = "filtre_right_side_bar" ) # Name of the module
golem::add_module( name = "accueil" ) # Name of the module
golem::add_module( name = "synthese" ) # Name of the module
golem::add_module( name = "diplomes" ) # Name of the module
golem::add_module( name = "repondants" ) # Name of the module
golem::add_module( name = "poursuite_etudes" ) # Name of the module
golem::add_module( name = "vie_active_durable" ) # Name of the module
golem::add_module( name = "emploi_premier" ) # Name of the module
golem::add_module( name = "emploi_30mois_poste" ) # Name of the module
golem::add_module( name = "emploi_30mois_employeur" ) # Name of the module
golem::add_module( name = "emploi_30mois_adequation" ) # Name of the module
golem::add_module( name = "temoignages" ) # Name of the module

## 2.2 Add dependencies

usethis::use_package( "thinkr" ) # To call each time you need a new package

## 2.3 Add tests

usethis::use_test( "app" )

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("ip.resultats")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
