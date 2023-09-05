phq9_scale_raw <- function(pdf_path) {
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

phq9_total_score <- function(pdf_path){
  tbl_dataframe <- phq9_scale_raw(pdf_path)

  tbl_dataframe |>
    dplyr::summarise(score = sum(score)) |>
    dplyr::pull(score)
}

phq9_severity <- function(pdf_path){

  # Kroenke K, Spitzer RL, Williams JB. The PHQ-9: validity of a brief
  # depression severity measure. J Gen Intern Med. 2001 Sep;16(9):606-13.
  # doi: 10.1046/j.1525-1497.2001.016009606.x. PMID: 11556941;
  # PMCID: PMC1495268.

  raw_score <- phq9_total_score(pdf_path)

  severity_level <- dplyr::case_match(
    raw_score,
    1:4 ~ 'minimal',
    5:9 ~ 'mild',
    10:14 ~ 'moderate',
    15:19 ~ 'moderately severe',
    20:27 ~ 'severe'
  )

  severity_level
}

