#' Auto distance covariance metric
#'
#' @inheritParams yardstick::pr_curve
#' @param truth The column identifier for the true results (that is `numeric`).
#' @param estimate The column identifier for the predicted results (that is also
#' `numeric`)
#' @param lag Vector of positive integers. This corresponds to the lag at
#' which distance covariance should be evaluated.
#' @returns A `tibble` with columns `.metric`, `.estimator` and `.estimate` and
#' 1 row of values. For `adcf_metric_vec`, a single `numeric` value (or `NA`).
#' @export
adcf_metric <- function(data, ...) {
    UseMethod("adcf_metric")
}

#' @rdname adcf_metric
#' @export
adcf_metric.data.frame <- function(
    data,
    truth,
    ...,
    lag = 2:vctrs::vec_size(data) - 1,
    na_rm = TRUE,
    case_weights = NULL) {
    result <- yardstick::curve_metric_summarizer(
        name = "adcf_metric",
        fn = adcf_metric_vec,
        data = data,
        truth = !!rlang::enquo(truth),
        ...,
        na_rm = na_rm,
        case_weights = !!rlang::enquo(case_weights),
        fn_options = list(lag = lag)
    )
    curve_finalize(result, data, "adcf_df", "grouped_adcf_df")
}

#' @rdname adcf_metric
#' @export
adcf_metric_vec <-
    function(
        truth, estimate, lag = 2:vctrs::vec_size(truth) - 1, na_rm = TRUE,
        case_weights = NULL, ...
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
    adcf_metric_impl(truth, estimate, lag, case_weights)
}

adcf_metric_impl <- function(truth, estimate, lag, case_weights = NULL) {
    z <- estimate - truth
    adcf::adcf(z, lag)
}