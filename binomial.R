#' Binomial test for the probability of GRIM inconsistency
#'
#' @description Given the output of [`grim_map()`] or similar, `grim_binomial()`
#'   tests whether the rate of GRIM-consistent value sets is above chance.
#'
#'   Consistency represents "success", and the null hypothesis corresponds to
#'   random decimal digits for the means (or other fractions, such as
#'   percentages.) See details below.
#'
#' @param data Data frame returned by a GRIM mapper function, such as
#'   [`grim_map()`] or [`grim_map_seq()`].
#' @param alternative String (length 1). One of `"greater"`, `"less"`, and
#'   `"two.sided"`. Default is `"greater"`; see details.
#' @param conf.level Numeric (length 1). Confidence level for the confidence
#'   interval in the output. Default is `0.95`.
#'
#' @details This conducts a one-tailed test by default (`alternative =
#'   "greater"`), unlike the underlying [`stats::binom.test()`] function. The
#'   idea is to assess whether more means are GRIM-consistent with their sample
#'   sizes than expected, with the null hypothesis representing a random choice
#'   of decimal digits for the means.
#'
#'   As any correct value set is GRIM-consistent, the key question is whether a
#'   collection of them can be sufficiently distinguished from random given the
#'   baseline [probability of GRIM
#'   inconsistency](https://lhdjung.github.io/scrutiny/articles/grim.html#the-probability-of-grim-inconsistency).
#'   In this way, even if some value sets are inconsistent, there may still be a
#'   trend towards consistency because correct value sets would bias the
#'   distribution in this direction.
#'
#'   However, it is hard to see how *fewer* value sets should be consistent than
#'   expected at random. This suggests a one-tailed test.
#'
#' @returns Tibble (data frame). See [`broom::tidy.htest()`] for the meaning of
#'   column names. However, note that the present test is one-sided by default,
#'   and a `conf.level` column is added to mirror the `conf.level` argument.
#'
#' @export
#'
#' @examples
#' library(scrutiny)
#' pigs1 %>%
#'   grim_map() %>%
#'   grim_binomial()

grim_binomial <- function(data,
                          alternative = c("greater", "less", "two.sided"),
                          conf.level = 0.95) {
  
  if (!inherits(data, "scr_grim_map")) {
    cli::cli_abort("`data` must be output of a GRIM mapper function.")
  }
  
  alternative <- rlang::arg_match(alternative)
  
  n_consistent <- length(which(data$consistency))
  n_cases <- nrow(data)
  
  if (packageVersion("scrutiny") < "0.5.0") {
    p_consistent <- 1 - mean(dplyr::if_else(data$ratio < 0, 0, data$ratio))
  } else {
    p_consistent <- 1 - mean(data$probability)
  }
  
  n_consistent %>%
    stats::binom.test(
      n = n_cases,
      p = p_consistent,
      alternative = alternative,
      conf.level = conf.level
    ) %>%
    broom::tidy() %>%
    dplyr::mutate(conf.level, .after = parameter)
}
