#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param n_runs
make_mccv_folds <- function(data, train_proportion = 3/4, n_runs = 100) {

  n_rows <- nrow(data)

  output <- vector(mode = 'list', length = n_runs)

  for(i in seq_along(output)){

    # training indices sampled here
    output[[i]] <- sample(x = n_rows,
                          replace = FALSE,
                          size = train_proportion * n_rows)

  }

  enframe(output, name = 'run', value = 'training_index')

}
