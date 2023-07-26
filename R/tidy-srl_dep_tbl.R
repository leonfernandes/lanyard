#' Tidy method for serial dependence
#'
#' @inheritParams generics::tidy
#' @param x a `tibble` of class `srl_dep_tbl`.
#' @returns a `tibble` with columns `metric`, `estimator`, `lag` and
#'      `estimate`.
#' @rdname tidy_serial
#' @exportS3Method generics::tidy
tidy.srl_dep_tbl <-
    function(x, ...) {
        tidy_srl_dep_impl(x)
    }

tidy_srl_dep_impl <-
    function(object) {
        lag <- rlang::sym("lag")
        metric <- rlang::sym("metric")
        estimator <- rlang::sym("estimator")
        estimate <- rlang::sym("estimate")
        object |>
            tidyr::pivot_longer(
                cols = -lag, names_to = "metric", values_to = "estimate"
            ) |>
            dplyr::mutate(
                estimator = factor("serial_dependence"),
                metric = as.factor(metric)
            ) |>
            dplyr::relocate(estimator, .before = estimate) |>
            dplyr::relocate(lag, .before = estimate)
    }