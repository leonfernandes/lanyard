#' Tidy method for serial dependence
#'
#' @param object an `tibble` of class `srl_dep_tbl`.
#' @returns a `tibble` with columns `.metric`, `.estimator`, `lag` and
#'      `.estimate`.
#' @export
tidy.srl_dep_tbl <-
    function(object) {
        tidy_srl_dep_impl(object)
    }

tidy_srl_dep_impl <-
    function(object) {
        object |>
            tidyr::pivot_longer(
                cols = -lag, names_to = ".metric", values_to = ".estimate"
            ) |>
            dplyr::mutate(
                .estimator = factor("serial_dependence"),
                .metric = as.factor(.metric)
            ) |>
            dplyr::relocate(.estimator, .before = .estimate) |>
            dplyr::relocate(lag, .before = .estimate)
    }