convertMenuItem <- function(tabName, mi) {
  
  # https://stackoverflow.com/questions/48210709/show-content-for-menuitem-when-menusubitems-exist-in-shiny-dashboard
  
  mi$children[[1]]$attribs['data-toggle'] <- "tab"
  mi$children[[1]]$attribs['data-value'] <- tabName
  if (length(mi$attribs$class) > 0 && mi$attribs$class == "treeview") {
    mi$attribs$class <- NULL
  }
  
  # change to angle-down icon
  mi$children[[1]]$children[[3]]$attribs['class'] <- "fa fa-angle-down pull-right"
  
  # Set class to custom
  mi$children[[2]]$attribs['class'] <- "treeview-menu-custom"
  
  mi
  
}