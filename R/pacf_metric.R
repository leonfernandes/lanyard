#' Partial autocorrelation metric
#'
#' @inheritParams adcv_metric
#' @returns A `tibble` with columns `.metric`, `.estimator` and `.estimate` and
#' 1 row of values. For `pacf_metric_vec`, a single `numeric` value (or `NA`).
#' @export
pacf_metric <- function(data, ...) {
    UseMethod("pacf_metric")
}

#' @rdname pacf_metric
#' @export
pacf_metric.data.frame <- function(
    data,
    truth,
    ...,
    lag = 2:vctrs::vec_size(data) - 1,
    na_rm = TRUE,
    case_weights = NULL
) {
    result <- yardstick::curve_metric_summarizer(
        name = "pacf_metric",
        fn = pacf_metric_vec,
        data = data,
        truth = !!rlang::enquo(truth),
        ...,
        na_rm = na_rm,
        case_weights = !!rlang::enquo(case_weights),
        fn_options = list(lag = lag)
    )
    curve_finalize(result, data, "pacf_df", "grouped_pacf_df")
}

#' @rdname pacf_metric
#' @export
pacf_metric_vec <- function(
    truth, estimate, lag = 0:1, na_rm = TRUE, case_weights = NULL, ...
) {
    yardstick::check_numeric_metric(truth, estimate, case_weights)
    if (na_rm) {
        result <- yardstick::yardstick_remove_missing(
            truth, estimate, case_weights
        )
        truth <- result$truth
        estimate <- result$estimate
        case_weights <- result$case_weights
    } else if (
        yardstick::yardstick_any_missing(truth, estimate, case_weights)
    ) {
        return(NA_real_)
    }
    pacf_metric_impl(truth, estimate, lag, case_weights)
}

pacf_metric_impl <- function(truth, estimate, lag, case_weights = NULL) {
    z <- estimate - truth
    tibble::tibble(
        lag = 1:max(lag),
        pacf = stats::acf(
            z, lag.max = max(lag), type = "partial", plot = FALSE,
            demean = FALSE
        )$acf |>
            as.numeric()
    ) |>
    tibble::new_tibble(class = "pacf_tbl")
}
