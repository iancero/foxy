tsi_validity_and_clinical_scales_raw <- function(pdf_path) {
  pdf_page <- pdf_path |>
    pdftools::pdf_text() |>
    getElement(3)

  tbl_row_strings <- pdf_page |>
    stringr::str_extract_all('.*\\d+\\s+\\d+\\s+\\d+\n') |>
    unlist() |>
    trimws() |>
    stringr::str_remove('\n')

  tbl_dataframe <- tbl_row_strings |>
    stringr::str_split('\\s{2,}') |>
    purrr::map(as.list) |>
    purrr::map(~ purrr::set_names(.x, c('scale', 'raw', 't', 'percentile'))) |>
    dplyr::bind_rows()

  tbl_dataframe
}

tsi_validity_scale_names <- function() {
  c('Response Level (RL)', 'Atypical Response (ATR)')
}

tsi_validity_scales_raw <- function(pdf_path) {
  pdf_path |>
    tsi_validity_and_clinical_scales() |>
    dplyr::filter(scale %in% tsi_validity_scale_names())
}

tsi_clinical_scales_raw <- function(pdf_path, remove_subscales = TRUE) {
  tbl_dataframe <- pdf_path |>
    tsi_validity_and_clinical_scales() |>
    dplyr::filter(!scale %in% tsi_validity_scale_names())

  if(remove_subscales){
    tbl_dataframe <- tbl_dataframe |>
      dplyr::filter( ! stringr::str_detect(scale, '\\(\\w+-\\w+\\)'))
  }

  tbl_dataframe
}

tsi_factor_scales_raw <- function(pdf_path){
  pdf_page <- pdf_path |>
    pdftools::pdf_text() |>
    getElement(3)

  tbl_row_strings <- pdf_page |>
    stringr::str_extract_all('.*\\d+\\s+\\d+\\s+\\d+\n') |>
    unlist() |>
    trimws() |>
    stringr::str_remove('\n')

  tbl_dataframe <- tbl_row_strings |>
    stringr::str_split('\\s{2,}') |>
    purrr::map(as.list) |>
    purrr::map(~ purrr::set_names(.x, c('scale', 'raw', 't', 'percentile'))) |>
    dplyr::bind_rows()

  tbl_dataframe
}
