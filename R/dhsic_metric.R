#' Multiavariate Hilbert Schmidt Independence Criterion metric
#'
#' @inheritParams adcv_metric
#' @returns A `tibble` with columns `.metric`, `.estimator` and `.estimate` and
#' 1 row of values. For `dhsic_metric_vec`, a single `numeric` value (or `NA`).
#' @export
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
        lag = 2:vctrs::vec_size(data) - 1,
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
            fn_options = list(lag = lag)
        )
        curve_finalize(result, data, "dhsic_df", "grouped_dhsic_df")
    }

#' @rdname dhsic_metric
#' @export
dhsic_metric_vec <-
    function(truth, estimate, lag = 1, na_rm = TRUE, case_weights = NULL, ...) {
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
        dhsic_metric_impl(truth, estimate, lag, case_weights)
    }

dhsic_metric_impl <-
    function(truth, estimate, lag, case_weights = NULL) {
        z <- estimate - truth
        dhsic_impl(z, lag)
    }

dhsic_impl <-
    function(z, lag) {
        n <- vctrs::vec_size(z)
        ret <-
            tibble::tibble(lag = lag) |>
                dplyr::mutate(dhsic = purrr::map_dbl(lag, \(h) {
                    x <- vctrs::vec_slice(z, 1:(n - h)) |>
                        matrix(ncol = 1)
                    y <- vctrs::vec_slice(z, (h + 1):n) |>
                        matrix(ncol = 1)
                    dHSIC::dhsic(x, y)$dHSIC
                })) |>
                tibble::new_tibble(class = "dhsic_tbl")
        class(ret) <- c("srl_dep", class(ret))
        ret
    }