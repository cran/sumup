#' Internal Text Cleaning Utilities
#'
#' Internal helper functions that replicate a minimal subset of the
#' functionality previously provided by the \pkg{textclean} package.
#'
#' These functions are not exported and are intended solely for
#' internal use within this package to avoid depending on the
#' archived CRAN package 'textclean'.
#'
#' @details
#' The following utilities are implemented:
#' \itemize{
#'   \item \code{replace_html_internal()} – Removes HTML tags and common HTML entities.
#'   \item \code{replace_white_internal()} – Normalizes whitespace.
#'   \item \code{replace_tokens_internal()} – Replaces specified tokens with a given replacement.
#'   \item \code{add_missing_endmark_internal()} – Adds sentence-ending punctuation if missing.
#' }
#'
#' These functions are intentionally lightweight and implement only the
#' behavior required by this package.
#'
#' @keywords internal
#' @noRd

replace_html_internal <- function(x, clean = TRUE) {
  if (length(x) == 0) return(x)

  x <- as.character(x)
  na_idx <- is.na(x)

  # Remove HTML tags
  x[!na_idx] <- gsub("<[^>]+>", " ", x[!na_idx])

  # Replace common HTML entities
  x[!na_idx] <- gsub("&nbsp;", " ", x[!na_idx], fixed = TRUE)
  x[!na_idx] <- gsub("&amp;", "&", x[!na_idx], fixed = TRUE)
  x[!na_idx] <- gsub("&lt;", "<", x[!na_idx], fixed = TRUE)
  x[!na_idx] <- gsub("&gt;", ">", x[!na_idx], fixed = TRUE)
  x[!na_idx] <- gsub("&quot;", "\"", x[!na_idx], fixed = TRUE)
  x[!na_idx] <- gsub("&#39;", "'", x[!na_idx], fixed = TRUE)

  x
}

replace_white_internal <- function(x) {
  if (length(x) == 0) return(x)

  x <- as.character(x)
  na_idx <- is.na(x)

  x[!na_idx] <- gsub("[[:space:]]+", " ", x[!na_idx])
  x[!na_idx] <- trimws(x[!na_idx])

  x
}

replace_tokens_internal <- function(x, tokens, replacement = " ") {
  if (length(x) == 0) return(x)

  x <- as.character(x)

  na_idx <- is.na(x)

  for (tok in tokens) {
    x[!na_idx] <- gsub(tok, replacement, x[!na_idx], fixed = TRUE)
  }

  x
}

add_missing_endmark_internal <- function(x, replacement = ".") {
  if (length(x) == 0) return(x)

  x <- as.character(x)
  na_idx <- is.na(x)

  x[!na_idx] <- trimws(x[!na_idx])

  needs_mark <- !grepl("[.!?]$", x) & !na_idx
  x[needs_mark] <- paste0(x[needs_mark], replacement)

  x
}
