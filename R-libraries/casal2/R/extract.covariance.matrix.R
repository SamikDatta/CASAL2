#' Utility extract.covariance.matrix function
#'
#' This function reads in the 'mpd.out' file created in estimation mode
#'
#' @param file the name of the file containing the estimated covariance matrix ('mpd.out')
#' @param path (optional) the path to the file
#' @param fileEncoding (optional) allows the R library to read in files that have been encoded in alternative UTF formats. See the User Manual for the error message that would indicate when to use this switch.
#' @param quiet suppress print or cat statements to screen.
#' @return covariance matrix
#' @export
#'
"extract.covariance.matrix" <- function(file, path = "", fileEncoding = "", quiet = FALSE) {
  filename <- file.path(path, file)
  lines <- convert.to.lines(filename, fileEncoding = fileEncoding, quiet = quiet)

  start_str <- "covariance_matrix"
  end_str <- "*end"

  if (length(grep(pattern = start_str, x = lines)) == 0) {
    stop("This model was not run with a minimiser that produced a covariance matrix. Please rerun this model with 'covariance true' in the @minimiser block.")
  }

  lines <- get.lines(lines, clip.to.match = start_str, clip.from.match = end_str)
  columns <- string.to.vector.of.words(lines[1])
  if (length(lines) < 2) {
    return(NA)
  }

  data <- matrix(0, length(columns), length(columns))
  for (i in 1:nrow(data)) {
    data[i, ] <- string.to.vector.of.numbers(lines[i])
  }
  data <- data[1:ncol(data), ]

  return(data)
}
