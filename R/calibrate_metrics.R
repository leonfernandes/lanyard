#' Calculates a set of metrics
#'
#' @param object a data.frame or numeric vector containing residuals
#' @param metrics a list of `lanyard` metrics.
#' @param resid_col column name corresponding to residuals.
#' @param ... unused.
#' @export
calibrate_metrics <-
    function(object, ...) {
        UseMethod("calibrate_metrics")
    }

#' @rdname calibrate_metrics
#' @export
calibrate_metrics.data.frame <-
    function(
        object, resid_col, metrics, lags = 2:vctrs::vec_size(object) - 1, ...
    ) {
        object <-
            object |>
            # add dummy column of zeroes
            dplyr::mutate(.temp = 0)
        metrics |>
            purrr::map(
                ~ .x(data = object, truth = resid_col, .temp, lags = lags) |>
                    generics::tidy()
                ) |>
            purrr::list_rbind() |>
            tibble::new_tibble(class = "metric_tbl")
    }

#' @rdname calibrate_metrics
#' @export
calibrate_metrics.numeric <-
    function(object, metrics, lags = 2:vctrs::vec_size(object) - 1, ...) {
        metrics |>
            purrr::map(
                ~ .x(data = object, lags) |>
                    generics::tidy()
            ) |>
            purrr::list_rbind() |>
            tibble::new_tibble(class = "metric_tbl")
    }