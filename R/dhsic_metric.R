#' Multiavariate Hilbert Schmidt Independence Criterion metric
#'
#' @inheritParams acf_metric
#' @returns A `tibble` of class `dhsic_tbl` with columns `lag`, `dhsic` and
#'      `dhsic_scaled`.
#' @export
#' @examples
#' # dhsic ---------------------------------------------------------------------
#' data <- data.frame(t = rnorm(100), e = rnorm(100))
#' dhsic_metric(data, t, e)
dhsic_metric <-
    function(data, ...) {
        UseMethod("dhsic_metric")
    }

#' @rdname dhsic_metric
#' @export
dhsic_metric.data.frame <-
    function(
        data,
        truth,
        ...,
        lags = 2:vctrs::vec_size(data) - 1,
        na_rm = TRUE,
        case_weights = NULL
    ) {
        result <- yardstick::curve_metric_summarizer(
            name = "dhsic_metric",
            fn = dhsic_metric_vec,
            data = data,
            truth = !!rlang::enquo(truth),
            ...,
            na_rm = na_rm,
            case_weights = !!rlang::enquo(case_weights),
            fn_options = list(lags = lags)
        )
        curve_finalize(result, data, "dhsic_df", "grouped_dhsic_df")
    }

#' @rdname dhsic_metric
#' @export
dhsic_metric_vec <-
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
        dhsic_metric_impl(truth, estimate, lags, case_weights)
    }

dhsic_metric_impl <-
    function(truth, estimate, lags, case_weights = NULL) {
        z <- estimate - truth
        dhsic_impl(z, lags)
    }

dhsic_impl <-
    function(z, lags) {
        n <- vctrs::vec_size(z)
        my_lags <- lags
        if (lags[1] != 0) {
            my_lags <- c(0, lags)
        }
        dhsic_raw <-
            my_lags |>
            purrr::map_dbl(
                \(h) {
                    x <- vctrs::vec_slice(z, 1:(n - h)) |>
                        matrix(ncol = 1)
                    y <- vctrs::vec_slice(z, (h + 1):n) |>
                        matrix(ncol = 1)
                    dHSIC::dhsic(x, y)$dHSIC
                }
            )
        dhsic_scaled_raw <- dhsic_raw / dhsic_raw[1]
        lag <- rlang::sym("lag")
        # return as tibble
        tibble::tibble(
            lag = my_lags,
            dhsic = dhsic_raw,
            dhsic_scaled = dhsic_scaled_raw
        ) |>
            dplyr::filter(lag %in% lags) |>
            tibble::new_tibble(class = "srl_dep_tbl")
    }