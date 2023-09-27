#' Auto-distance covariance metric
#'
#' @inheritParams acf_metric
#' @returns A `tibble` of class `adcf_tbl` with columns `lag`,
#'      `auto_dist_covariance` and `auto_dist_correlation`.
#' @export
#' @examples
#' # adcf ----------------------------------------------------------------------
#' data <- data.frame(t = rnorm(100), e = rnorm(100))
#' adcf_sq_metric(data, t, e)
adcf_sq_metric <-
    function(data, ...) {
        UseMethod("adcf_sq_metric")
    }

#' @rdname adcf_sq_metric
#' @export
adcf_sq_metric.data.frame <-
    function(
        data,
        truth,
        ...,
        lags = 2:vctrs::vec_size(data) - 1,
        na_rm = TRUE,
        case_weights = NULL
    ) {
        result <- yardstick::curve_metric_summarizer(
            name = "adcf_sq_metric",
            fn = adcf_sq_metric_vec,
            data = data,
            truth = !!rlang::enquo(truth),
            ...,
            na_rm = na_rm,
            case_weights = !!rlang::enquo(case_weights),
            fn_options = list(lags = lags)
        )
        curve_finalize(result, data, "adcf_df", "grouped_adcf_df")
    }

#' @rdname adcf_sq_metric
#' @export
adcf_sq_metric_vec <-
    function(
        truth, estimate, lags = 2:vctrs::vec_size(truth) - 1, na_rm = TRUE,
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
        adcf_sq_metric_impl(truth, estimate, lags, case_weights)
    }

#' @rdname adcf_sq_metric
#' @export
adcf_sq_metric.numeric <-
    function(data, lags = 1, case_weights = NULL, ...) {
        estimate <- numeric(vctrs::vec_size(data))
        adcf_sq_metric_impl(data, estimate, lags, case_weights)
    }

adcf_sq_metric_impl <-
    function(truth, estimate, lags, case_weights = NULL) {
        auto_dist_covariance <- rlang::sym("auto_dist_covariance")
        auto_dist_correlation <- rlang::sym("auto_dist_correlation")
        z <- (estimate - truth)^2
        adcf::adcf(z - mean(z), lags) |>
            dplyr::rename(
                auto_dist_covariance_sq = auto_dist_covariance,
                auto_dist_correlation_sq = auto_dist_correlation
            ) |>
            tibble::new_tibble(class = "srl_dep_tbl")
    }