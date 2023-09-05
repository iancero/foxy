wsas_anchors <- function(pdf_path){
  pdf_page <- pdf_path |>
    pdftools::pdf_text()

  anchor_rows <- pdf_page |>
    stringr::str_extract_all('\\d\\s{0,1}= .*') |>
    unlist()

  anchor_tbl <- anchor_rows |>
    stringr::str_split('=') |>
    purrr::map(~ purrr::set_names(.x, c('score', 'anchor'))) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      anchor = tolower(anchor),
      score = as.numeric(score)) |>
    dplyr::right_join(y = data.frame(score = 0:8), by = 'score') |>
    dplyr::arrange(score) |>
    dplyr::mutate(anchor = ifelse(
      test = is.na(anchor),
      yes = paste(dplyr::lag(anchor), dplyr::lead(anchor), sep = ' to '),
      no = anchor))

  anchor_tbl
}

wsas_scale_raw <- function(pdf_path) {
  item_anchors <- wsas_anchors(pdf_path)

  pdf_page <- pdf_path |>
    pdftools::pdf_text()

  tbl_rows <- pdf_page |>
    stringr::str_extract_all('Because .*\n*.*\n*[ •]*\\d') |>
    unlist() |>
    stringr::str_remove_all('\n')

  tbl_dataframe <- tbl_rows |>
    stringr::str_split(' •') |>
    purrr::map(~ purrr::set_names(.x, c('item', 'score'))) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      anchor = tolower(anchor),
      score = as.numeric(score)) |>
    dplyr::left_join(y = item_anchors, by = 'score') |>
    dplyr::select(item, anchor, score)

  tbl_dataframe
}


wsas_total_score <- function(pdf_path){
  tbl_dataframe <- wsas_scale_raw(pdf_path)

  tbl_dataframe |>
    dplyr::summarise(score = sum(score)) |>
    dplyr::pull(score)
}

wsas_severity <- function(pdf_path){

  # see page 3 "A WSAS score above 20 appears to suggest moderately severe..."
  # Mundt, J., Marks, I., Shear, M., & Greist, J. (2002). The Work and Social
  # Adjustment Scale: A simple measure of impairment in functioning. The British
  # Journal of Psychiatry, 180(5), 461-464. doi:10.1192/bjp.180.5.461

  raw_score <- wsas_total_score(pdf_path)

  severity_level <- dplyr::case_match(
    raw_score,
    0:9 ~ 'impairment consistent with subclinical populations',
    10:20 ~ paste(
      'consistent with significant functional impairment,',
      'but less severe clinical symptoms'),
    20:40 ~ 'suggestive of moderately severe or worse psychopathology',
  )

  severity_level
}























