#' Autocorrelation metric
#'
#' @inheritParams adcv_metric
#' @returns A `tibble` with columns `.metric`, `.estimator` and `.estimate` and
#' 1 row of values. For `acf_metric_vec`, a single `numeric` value (or `NA`).
#' @export
acf_metric <- function(data, ...) {
    UseMethod("acf_metric")
}

#' @rdname acf_metric
#' @export
acf_metric.data.frame <- function(
    data,
    truth,
    ...,
    lag = 2:vctrs::vec_size(data) - 1,
    na_rm = TRUE,
    case_weights = NULL
) {
    result <- yardstick::curve_metric_summarizer(
        name = "acf_metric",
        fn = acf_metric_vec,
        data = data,
        truth = !!rlang::enquo(truth),
        ...,
        na_rm = na_rm,
        case_weights = !!rlang::enquo(case_weights),
        fn_options = list(lag = lag)
    )
    curve_finalize(result, data, "acf_df", "grouped_acf_df")
}

#' @rdname acf_metric
#' @export
acf_metric_vec <- function(
    truth, estimate, lag = 1, na_rm = TRUE, case_weights = NULL, ...
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
    acf_metric_impl(truth, estimate, lag, case_weights)
}

acf_metric_impl <- function(truth, estimate, lag, case_weights = NULL) {
    z <- estimate - truth
    tibble::tibble(
        lag = 0:max(lag),
        acv = stats::acf(
            z, lag.max = max(lag), type = "correlation", plot = FALSE,
            demean = FALSE
        )$acf |>
            as.numeric()
    ) |>
        dplyr::filter(lag > 0) |>
        tibble::new_tibble(class = "acf_tbl")
}