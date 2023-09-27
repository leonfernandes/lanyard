#' Autocorrelation metric for squares
#'
#' @inheritParams acf_metric
#' @export
#' @examples
#' # acf -----------------------------------------------------------------------
#' data <- data.frame(t = rnorm(100), e = rnorm(100))
#' acf_sq_metric(data, t, e)
acf_sq_metric <-
    function(data, ...) {
        UseMethod("acf_sq_metric")
    }

#' @rdname acf_sq_metric
#' @export
acf_sq_metric.data.frame <-
    function(
        data,
        truth,
        ...,
        lags = 2:vctrs::vec_size(data) - 1,
        na_rm = TRUE,
        case_weights = NULL
    ) {
        result <-
            yardstick::curve_metric_summarizer(
                name = "acf_sq_metric",
                fn = acf_sq_metric_vec,
                data = data,
                truth = !!rlang::enquo(truth),
                ... = ...,
                na_rm = na_rm,
                case_weights = !!rlang::enquo(case_weights),
                fn_options = list(lags = lags)
            )
        curve_finalize(result, data, "acf_df", "grouped_acf_df")
    }

#' @rdname acf_sq_metric
#' @export
acf_sq_metric_vec <-
    function(
        truth, estimate, lags = 1, na_rm = TRUE, case_weights = NULL, ...
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
        acf_sq_metric_impl(truth, estimate, lags, case_weights)
    }

#' @rdname acf_sq_metric
#' @export
acf_sq_metric.numeric <-
    function(data, lags = 1, case_weights = NULL, ...) {
        estimate <- numeric(vctrs::vec_size(data))
        acf_sq_metric_impl(data, estimate, lags, case_weights)
    }

acf_sq_metric_impl <-
    function(truth, estimate, lags, case_weights = NULL) {
        z <- estimate - truth
        lag_max <- max(lags)
        # get raw values for acfs upto lag_max
        acv_raw <-
            stats::acf(
                z^2, lag.max = lag_max, type = "covariance", plot = FALSE,
                demean = TRUE
            )$acf |>
            as.numeric()
        # use autocovariance to get autocorrelation
        acf_raw <- acv_raw / acv_raw[1]
        lag <- rlang::sym("lag")
        # return as tibble
        tibble::tibble(
            lag = 0:max(lags),
            autocovariance_sq = acv_raw,
            autocorrelation_sq = acf_raw
        ) |>
            dplyr::filter(lag %in% lags) |>
            tibble::new_tibble(class = "srl_dep_tbl")
    }