aceq_scale_raw <- function(pdf_path){
  pdf_page <- pdf_path |>
    pdftools::pdf_text() |>
    getElement(1)

  tbl_rows <- pdf_page |>
    stringr::str_replace_all('\n+', ' ') |>
    stringr::str_extract_all('1\\. .*\\)') |>
    stringr::str_replace('1\\.', '') |>
    stringr::str_split('\\d+\\.') |>
    unlist()

  tbl_dataframe <- tbl_rows |>
    stringr::str_split('•') |>
    purrr::map(~ purrr::set_names(.x, c('item', 'response_str'))) |>
    dplyr::bind_rows() |>
    tidyr::separate_wider_delim(
      cols = response_str,
      delim = '(',
      names = c('anchor', 'score')) |>
    dplyr::mutate(
      anchor = anchor |>
        trimws() |>
        tolower(),
      score = as.numeric(stringr::str_extract_all(score, '\\d')))

  tbl_dataframe
}

aceq_total_score <- function(pdf_path){
  tbl_dataframe <- aceq_scale_raw(pdf_path)

  tbl_dataframe |>
    dplyr::summarise(score = sum(score)) |>
    dplyr::pull(score)
}

aceq_severity <- function(pdf_path, stress_cond_present){

  # is a "stress_cond_present"? see pages 4 and 8 below for help

  # https://www.acesaware.org/about/
  # https://www.acesaware.org/wp-content/uploads/2019/12/ACE-Clinical-Workflows-Algorithms-and-ACE-Associated-Health-Conditions-ADA.pdf
  # see page 4 for pediatric
  # see page 8 for adult

  raw_score <- aceq_total_score(pdf_path)

  risk_level <- dplyr::case_when(
    raw_score == 0 ~ 'low risk',
    raw_score %in% 0:3 & !stress_cond_present ~ 'low risk',
    raw_score %in% 1:3 & stress_cond_present ~ 'intermediate risk',
    raw_score >= 4 ~ 'high risk'
  ) |>
    paste('for toxic stress physiology')

  risk_level
}


significant_endorsements <- function(pdf_path) {
  endorsements <- pdf_path |>
    aceq_scale_raw()



}


aceq_verbage <- function(pdf_path, patient_first = NULL, stress_cond_present){
  template_str <- readr::read_file('inst/extdata/aceq_template.txt')

  total_score <- aceq_total_score(pdf_path)
  severity <- aceq_severity(pdf_path, stress_cond_present)

  if(is.null(patient_first)){
    patient_first <- 'patient_first'
  }

  interpolated_verbage <- glue::glue(
    template_str,
    total_score = total_score,
    severity = severity)

  interpolated_verbage
}


