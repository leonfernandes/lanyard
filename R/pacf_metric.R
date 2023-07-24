#' Partial autocorrelation metric
#'
#' @inheritParams acf_metric
#' @returns A `tibble` of class `pacf_tbl` with columns `lags` and
#'      `partial_autocorrelation`.
#' @export
#' @examples
#' # pacf ----------------------------------------------------------------------
#' data <- data.frame(t = rnorm(100), e = rnorm(100))
#' pacf_metric(data, t, e)
pacf_metric <-
    function(data, ...) {
        UseMethod("pacf_metric")
    }

#' @rdname pacf_metric
#' @export
pacf_metric.data.frame <-
    function(
        data,
        truth,
        ...,
        lags = 2:vctrs::vec_size(data) - 1,
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
            fn_options = list(lags = lags)
        )
        curve_finalize(result, data, "pacf_df", "grouped_pacf_df")
    }

#' @rdname pacf_metric
#' @export
pacf_metric_vec <-
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
        pacf_metric_impl(truth, estimate, lags, case_weights)
    }

pacf_metric_impl <-
    function(truth, estimate, lags, case_weights = NULL) {
        z <- estimate - truth
        pacf_raw <- stats::acf(
            z,
            lag.max = max(lags),
            type = "partial",
            plot = FALSE,
            demean = FALSE
        )$acf |>
            as.numeric()
        # return as tibble
        lag <- rlang::sym("lag")
        tibble::tibble(
            lag = 1:max(lags),
            partial_autocorrelation = pacf_raw
        ) |>
            dplyr::filter(lag %in% lags) |>
            tibble::new_tibble(class = "srl_dep_tbl")
    }
