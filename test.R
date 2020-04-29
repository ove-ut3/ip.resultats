replace_non_ascii_with_unicode <- function(r_file, r_file_output = NULL) {
  
  r_file <- "R/app_ui.R"
  r_file_output <- "app_ui.R"
  
  if (is.null(r_file_output)) {
    
    r_file_output <- r_file
    
  }
  
  dplyr::tibble(
    code = readLines(r_file, encoding = "UTF-8", warn = FALSE)
  ) %>% 
    dplyr::mutate(line = dplyr::row_number()) %>% 
    dplyr::mutate_at("code", strsplit, "") %>% 
    tidyr::unnest(code) %>% 
    dplyr::group_by(line) %>% 
    dplyr::mutate(char = dplyr::row_number()) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      code_ascii = dplyr::if_else(
        !stringi::stri_enc_isascii(code),
        purrr::map_chr(
          code,
          ~ iconv(., from = "UTF-8", toRaw = TRUE) %>% 
            unlist() %>% 
            as.character() %>% 
            paste0(collapse = "") %>% 
            { paste0('\\u', .) }
        ),
        code,
        code
      )
    ) %>% 
    dplyr::group_by(line) %>% 
    dplyr::summarise_at("code_ascii", paste0, collapse = "") %>% 
    dplyr::ungroup() %>% 
    dplyr::pull(code_ascii) %>% 
    readr::write_lines(r_file_output)
  
}
