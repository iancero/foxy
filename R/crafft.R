crafft_scale_raw <- function(pdf_path){

  pdf_page <- pdf_path |>
    pdftools::pdf_text()

  # the CRAFFT has two different groups of items: 1-3 and 4-9

  tbl_rows_1_to_3 <- pdf_page |>
    stringr::str_replace_all('\n+', ' ') |>
    stringr::str_extract('1. During.*') |>
    stringr::str_remove('READ THESE INSTRUCTIONS BEFORE CONTINUING.*') |>
    stringr::str_replace('1\\.', '') |>
    stringr::str_split('[1-3]\\.') |>
    unlist()

  tbl_dataframe_1_to_3 <- tbl_rows_1_to_3 |>
    stringr::str_split('\\.') |>
    purrr::map(~ purrr::set_names(.x, c('item', 'anchor'))) |>
    dplyr::bind_rows() |>
    dplyr::mutate(anchor = tolower(anchor))

  tbl_rows_4_to_9 <- pdf_page |>
    stringr::str_replace_all('\n+', ' ') |>
    stringr::str_extract('4\\..*') |>
    stringr::str_remove('NOTICE TO CLINIC STAFF.*') |>
    stringr::str_replace('4\\.', '') |>
    stringr::str_split('[4-9]\\.') |>
    unlist()

  tbl_dataframe_4_to_9 <- tbl_rows |>
    stringr::str_split('•') |>
    purrr::map(~ purrr::set_names(.x, c('item', 'anchor'))) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      anchor = anchor |>
        tolower() |>
        trimws(),
      score = dplyr::case_when(
        anchor == 'no' ~ 0,
        anchor == 'yes' ~ 1,
        TRUE ~ NA_real_
      )
    )

  tbl_dataframe <- dplyr::bind_rows(tbl_dataframe_1_to_3, tbl_dataframe_4_to_9)

  tbl_dataframe
}

crafft_total_score <- function(pdf_path){
  tbl_dataframe <- crafft_scale_raw(pdf_path)

  tbl_dataframe |>
    dplyr::summarise(score = sum(score, na.rm = T)) |>
    dplyr::pull(score)
}

crafft_severity <- function(pdf_path){

  # https://crafft.org/wp-content/uploads/2018/08/FINAL-CRAFFT-2.1_provider_manual_with-CRAFFTN_2018-04-23.pdf

  tbl_dataframe <- crafft_scale_raw(pdf_path)

  freq_question_df <- tbl_dataframe |>
    dplyr::slice(1:3)

  crafft_question_df <- tbl_dataframe |>
    dplyr::slice(4:9)

  car_question <- as.numeric(crafft_question_df[1, 'score'])
  total_score <- crafft_total_score(pdf_path)

  positive_screen <- freq_question_df |>
    dplyr::summarize(needs_followup_qs = sum(anchor == '0') < 3) |>
    dplyr::pull(needs_followup_qs)

  risk_level <- dplyr::case_when(
    !positive_screen & total_score == 0 ~ 'low risk',
    !positive_screen & car_question == 1 ~ 'medium risk',
    positive_screen & total_score <= 2 ~ 'medium risk',
    positive_screen & total_score > 2 ~ 'high risk',
    TRUE ~ NA_character_
  )

  risk_level
}






















