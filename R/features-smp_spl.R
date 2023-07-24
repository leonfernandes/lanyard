#' Features for sample splitting
#'
#' @inheritParams fabletools::features
#' @param .var The variable to compute features on.
#' @rdname features_smp_spl
#' @exportS3Method fabletools::features
features.smp_spl <-
    function(.tbl, .var, features, ...) {
        .var <- rlang::enquo(.var)
        .tbl |>
            dplyr::mutate(
                .assessment = purrr::map(
                    !!.var,
                    ~ .x |>
                        dplyr::mutate(
                            .features = purrr::map(
                                .subresid,
                                function(.) {
                                    . |>
                                        fabletools::features(
                                            .var = .resid,
                                            features = features
                                        )
                                }
                            )
                        ) |>
                        dplyr::select(-.subresid)
                )
            ) |>
            tibble::new_tibble(class = "smp_spl_features")
    }
