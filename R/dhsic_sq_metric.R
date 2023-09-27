#' Multiavariate Hilbert Schmidt Independence Criterion metric
#'
#' @inheritParams acf_metric
#' @returns A `tibble` of class `dhsic_tbl` with columns `lag`, `dhsic` and
#'      `dhsic_scaled`.
#' @export
#' @examples
#' # dhsic ---------------------------------------------------------------------
#' data <- data.frame(t = rnorm(100), e = rnorm(100))
#' dhsic_sq_metric(data, t, e)
dhsic_sq_metric <-
    function(data, ...) {
        UseMethod("dhsic_sq_metric")
    }

#' @rdname dhsic_sq_metric
#' @export
dhsic_sq_metric.data.frame <-
    function(
        data,
        truth,
        ...,
        lags = 2:vctrs::vec_size(data) - 1,
        na_rm = TRUE,
        case_weights = NULL
    ) {
        result <- yardstick::curve_metric_summarizer(
            name = "dhsic_sq_metric",
            fn = dhsic_sq_metric_vec,
            data = data,
            truth = !!rlang::enquo(truth),
            ...,
            na_rm = na_rm,
            case_weights = !!rlang::enquo(case_weights),
            fn_options = list(lags = lags)
        )
        curve_finalize(result, data, "dhsic_df", "grouped_dhsic_df")
    }

#' @rdname dhsic_sq_metric
#' @export
dhsic_sq_metric_vec <-
    function(
        truth, estimate, lags = 0:1, na_rm = TRUE, case_weights = NULL, ...
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
        dhsic_sq_metric_impl(truth, estimate, lags, case_weights)
    }

#' @rdname dhsic_sq_metric
#' @export
dhsic_sq_metric.numeric <-
    function(data, lags = 1, case_weights = NULL, ...) {
        estimate <- numeric(vctrs::vec_size(data))
        dhsic_sq_metric_impl(data, estimate, lags, case_weights)
    }

dhsic_sq_metric_impl <-
    function(truth, estimate, lags, case_weights = NULL) {
        z <- (estimate - truth)^2
        dhsic_impl(z - mean(z), lags)
    }