#' Features for sample splitting
#'
#' @inheritParams fabletools::features
#' @export
features.smp_spl <-
    function(.tbl, features, ...) {
        .assessment <- rlang::sym(".assessment")
        .tbl |>
            dplyr::mutate(
                .assessment = purrr::map(
                    .assessment,
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
