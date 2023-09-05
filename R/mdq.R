# Note: the Mood Disorders Questionnaire is listed as Mood Questionnaire on
# Meghan's SimplePractice website.

mdq_items <- function(pdf_path){
  pdf_page <- pdf_path |>
    pdftools::pdf_text()

  start_phrases <- c('Has there', 'If you checked YES', 'How much of a problem',
                     'Have any of your blood', 'Has a health professional')

  body_pattern <- '.*[\n]*.*'

  search_pattern <- paste(start_phrases, body_pattern, sep = '', collapse = '|')

  items <- pdf_page |>
    stringr::str_extract_all(search_pattern) |>
    unlist() |>
    stringr::str_replace('\n', ' ') |>
    stringr::str_remove_all('•.*')

  items
}

mdq_responses <- function(pdf_path){
  item_responses <- pdf_page |>
    stringr::str_extract_all('• \\w+') |>
    unlist() |>
    stringr::str_remove('•') |>
    trimws() |>
    tolower()

  item_responses
}

mdq_scale_raw <- function(pdf_path){

  items = mdq_items(pdf_path)
  responses = mdq_responses(pdf_path)

  tbl_dataframe <- dplyr::tibble(item = items, anchor = responses) |>
    dplyr::mutate(
      anchor = tolower(anchor),
      score = dplyr::case_when(
        dplyr::row_number() > 13 ~ NA_real_,
        anchor == 'no' ~ 0,
        anchor == 'yes' ~ 1,
        TRUE ~ NA_real_
      )
    )

  tbl_dataframe
}


mdq_total_score <- function(pdf_path){
  tbl_dataframe <- mdq_scale_raw(pdf_path)

  tbl_dataframe |>
    filter(dplyr::row_number() <= 13) |> # only items 1-13 are scored
    dplyr::summarise(score = sum(score)) |>
    dplyr::pull(score)
}


mdq_severity <- function(pdf_path){

  # https://ajp.psychiatryonline.org/doi/full/10.1176/appi.ajp.157.11.1873

  tbl_dataframe <- mdq_scale_raw(pdf_path)
  same_time_period <- tolower(tbl_dataframe[[14, 'anchor']]) == 'yes'
  func_impairment <- stringr::str_detect(
    string = tbl_dataframe[[14, 'anchor']],
    pattern = 'moderate|serious'
  )

  raw_score <- mdq_total_score(pdf_path)

  risk_level <- dplyr::if_else(
    condition = raw_score >= 7 & same_time_period & func_impairment,
    true = 'positive screen',
    false = 'negative screen'
  )

  risk_level
}
