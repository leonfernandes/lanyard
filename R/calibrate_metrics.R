#' Calculates a set of metrics
#'
#' @param object a data.frame containing residuals.
#' @param metrics a list of `lanyard` metrics.
#' @param resid_col column name corresponding to residuals.
#' @export
calibrate_metrics <-
    function(object, ...) {
        UseMethod("calibrate_metrics")
    }

#' @rdname calibrate_metrics
#' @export
calibrate_metrics.data.frame <-
    function(object, resid_col, metrics) {
        object <-
            object |>
            # add dummy column of zeroes
            dplyr::mutate(.temp = 0)
        metrics |>
            purrr::map(
                ~ .x(object, resid_col, .temp) |>
                    generics::tidy()
                ) |>
            purrr::list_rbind() |>
            tibble::new_tibble(class = "metric_tbl")
    }