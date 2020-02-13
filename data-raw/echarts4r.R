#

tribble(
  ~diploma_parent, ~diploma, ~n,
  "Niveau Bac+5",   "Master",                                              2831,
  "Niveau Bac+5",   "Ecole d'ingénieurs",                                  2055,
  "Niveau Bac+5",   "Ecole de commerce",                                   2831,
  "Niveau Bac+3",   "Licence professionnelle",                             1932,
  "Niveau Bac+3",   "Licence générale",                                     594,
  "Niveau Bac+3",   "DCG, DECF",                                             80,
  "Diplôme de niveau inférieur et autre",   "Diplôme de niveau inférieur",   86,
  "Diplôme de niveau inférieur et autre",   "Année de césure",              137,
  "Diplôme de niveau inférieur et autre",   "Autre",                        560
)

library(echarts4r)

ip.resultats::donnees %>% 
  dplyr::filter(type_diplome == "Diplôme universitaire de technologie",
                parcours %in% c("Poursuite d'études directe", "Reprise d'études")) %>% 
  dplyr::count(niveau_diplome_vise, diplome_vise) %>% 
  dplyr::group_by() %>% 
  dplyr::mutate(pct = n / sum(n) * 100) %>% 
  dplyr::ungroup() %>% 
  echarts4r::e_charts() %>% 
  echarts4r::e_treemap(niveau_diplome_vise, diplome_vise, n) %>% 
  #echarts4r::e_tooltip()
  echarts4r::e_tooltip_item_formatter("percent")

  echarts4r::e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return('<strong>' + params.name + 
                                        '</strong><br />wt: ' + params.value[0] + 
                                        '<br />mpg: ' +  params.value[1] +
                                        '<br />qsec: ' +  params.value[2] )   }  "))

ip.resultats::donnees %>% 
  dplyr::filter(type_diplome == "Diplôme universitaire de technologie",
                parcours %in% c("Poursuite d'études directe", "Reprise d'études")) %>% 
  dplyr::count(niveau_diplome_vise, diplome_vise) %>% 
  dplyr::group_by() %>% 
  dplyr::mutate(pct = n / sum(n) * 100) %>% 
  dplyr::ungroup() %>% 
  echarts4r::e_charts(pct) %>% 
  echarts4r::e_treemap(niveau_diplome_vise, diplome_vise, n) %>% 
  #echarts4r::e_tooltip() %>% 
  echarts4r::e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return('<strong>' + params.name + 
                                        '</strong><br />wt: ' + params.value[1] )}"))

library(echarts4r)
iris %>% 
  dplyr::filter(Species == "setosa") %>% 
  dplyr::slice(1:5) %>% 
  e_charts(Sepal.Length) %>% 
  e_scatter(
    Petal.Length, 
    Sepal.Width,
    label = list(
      show = TRUE,
      color = "black",
      position = list(
        "80%",
        "80%"
      ),
      formatter = "x: {@[0]}\ny: {@[1]}\nz: {@[2]}" # JavaScript array start at 0
    )
  ) %>% 
  e_x_axis(min = 4, max = 5.5)  %>% 
  e_y_axis(min = .9, max = 1.6)

tm <- data.frame(
  parent = c("earth", "earth", "earth", "mars", "mars"), 
  child = c("forest", "ocean", "iceberg", "elon", "curiosity"),
  value = ceiling(rnorm(5, 10, 2))
)

tm %>% 
  e_charts() %>% 
  e_treemap(
    parent, child, value,
    label = list(
      show = TRUE,
      formatter = htmlwidgets::JS("
        function(params){
          return params.name + ': ' +
          params.value;
        }
      ")
    )
  )

library(dplyr)
library(echarts4r)
mtcars %>%  
  tibble::rownames_to_column("model") %>% 
  e_charts(wt) %>% 
  e_scatter(mpg, qsec, bind=model) %>% # pass qsec as size
  e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return('<strong>' + params.name + 
                                        '</strong><br />wt: ' + params.value[0] + 
                                        '<br />mpg: ' +  params.value[1] +
                                        '<br />qsec: ' +  params.value[2] )   }  "))

library(echarts4r)
mtcars %>% 
  head() %>% 
  dplyr::mutate(model = row.names(.)) %>% 
  e_charts(model) %>% 
  e_pie(carb, radius = c("50%", "70%"), legend = FALSE) %>% 
  e_title("Donut chart", left = "center", top = "center") %>% 
  e_tooltip(formatter = e_tooltip_pie_formatter(style = "percent")) %>% 
  e_inspect()

