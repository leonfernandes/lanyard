curve_finalize <- function(result, data, class, grouped_class) {
  # Packed `.estimate` curve data frame
  out <- dplyr::pull(result, ".estimate")

  if (!dplyr::is_grouped_df(data)) {
    class(out) <- c(class, class(out))
    return(out)
  }

  group_syms <- dplyr::groups(data)

  # Poor-man's `tidyr::unpack()`
  groups <- dplyr::select(result, !!!group_syms)
  out <- dplyr::bind_cols(groups, out)

  # Curve functions always return a result grouped by original groups
  out <- dplyr::group_by(out, !!!group_syms)

  class(out) <- c(grouped_class, class, class(out))

  out
}