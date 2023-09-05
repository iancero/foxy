gad7_scale_raw <- function(pdf_path) {
  pdf_page <- pdf_path |>
    pdftools::pdf_text()

  tbl_rows <- pdf_page |>
    stringr::str_extract_all('\\d\\..*\\(\\d\\)') |>
    unlist() |>
    stringr::str_remove_all('\\d\\. ')

  tbl_dataframe <- tbl_rows |>
    stringr::str_split(':') |>
    purrr::map(~ purrr::set_names(.x, c('item', 'response_str'))) |>
    dplyr::bind_rows() |>
    tidyr::separate_wider_delim(
      cols = response_str,
      delim = '(',
      names = c('anchor', 'score')) |>
    dplyr::mutate(
      anchor = tolower(anchor),
      score = as.numeric(stringr::str_extract_all(score, '\\d')))

  tbl_dataframe
}

gad7_total_score <- function(pdf_path){
  tbl_dataframe <- gad7_scale_raw(pdf_path)

  tbl_dataframe |>
    dplyr::summarise(score = sum(score)) |>
    dplyr::pull(score)
}

gad7_severity <- function(pdf_path){

  # Spitzer RL, Kroenke K, Williams JBW, Löwe B. A Brief Measure for Assessing
  # Generalized Anxiety Disorder: The GAD-7. Arch Intern Med.
  # 2006;166(10):1092–1097. doi:10.1001/archinte.166.10.1092

  raw_score <- gad7_total_score(pdf_path)

  severity_level <- dplyr::case_match(
    raw_score,
    1:4 ~ 'minimal',
    5:9 ~ 'mild',
    10:14 ~ 'moderate',
    15:21 ~ 'severe'
  )

  severity_level
}

