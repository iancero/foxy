comma_list <- function(list_items){
  if(length(list_items) == 1){
    return(list_items)
  }

  if(length(list_items) == 2){
    list_items <- paste(list_items, collapse = ' and ')
    return(list_items)
  }

  initial_items <- list_items[1:(length(list_items) - 1)] |>
    paste(', ', sep = '')

  last_item <- list_items[length(list_items)]
  last_item <- paste('and', last_item)

  initial_items |>
    paste(collapse = ' ') |>
    paste(last_item)
}

pronouns <- function(gender){
  if (gender == 'boy_man'){
    words <- list(
      'he_she' = 'he',
      'his_her' = 'his',
      'him_her' = 'him',
      'his_hers' = 'his'
    )

  } else if (gender == 'girl_woman') {
    words <- list(
      'he_she' = 'she',
      'his_her' = 'her',
      'him_her' = 'her',
      'his_hers' = 'hers'
    )
  } else if (gender == 'nonbinary') {
    words <- list(
      'he_she' = 'they',
      'his_her' = 'their',
      'him_her' = 'them',
      'his_hers' = 'theirs'
    )
  } else {
    stop('unrecognized pronouns')
  }

  words
}

