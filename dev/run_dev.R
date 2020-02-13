# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

credentials <- data.frame(
  user = c("rguilet", "admin"),
  password = c("cfvu_test", "T&Tala!"),
  stringsAsFactors = FALSE
)

diplome <- "Master"

# Run the application
ip.resultats::run_app(
  credentials = credentials,
  diplome = diplome
)
