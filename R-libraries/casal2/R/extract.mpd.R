#' @title extract MPD function for readin in Casal2 output that has been generated from a -r, -e, -f, -p run mode.
#'
#' @description
#' An extract function that reads Casal2 output that are produced from a '-r' or '-e' or '-f' or '-p' model run. This function
#' also creates a 'casal2.mpd' class which can be used in plotting and summary functions. See the casal2 manual for more information.
#'
#' @author Dan Fu
#' @param file the name of the input file containing model output to extract
#' @param path Optionally, the path to the file
#' @param fileEncoding Optional, allows the R-library to read in files that have been encoded in alternative UTF formats, see the manual for the error message that would indicate when to use this switch.
#' @param quiet suppress print or cat statements to screen.
#' @return a 'casal2MPD' object which is essentially a list, that can be integrated using the str() function.
#' @export
#' @examples
#' \donttest{
#' library(casal2)
#' data <- extract.mpd(file = system.file("extdata", "MPD.log", package = "casal2"))
#' class(data)
#' }
"extract.mpd" <- function(file, path = "", fileEncoding = "", quiet = FALSE) {
  set.class <- function(object, new.class) {
    # use in the form
    #  object <- set.class(object,"new class")
    attributes(object)$class <- c(new.class, attributes(object)$class[attributes(object)$class != new.class])
    object
  }
  multi_input_args <- c("-i", "-I", "--input-force", "--input", "-s", "--simulation", "--profile", "-p", "--projection", "-f")
  filename <- make.filename(path = path, file = file)
  file <- convert.to.lines(filename, fileEncoding = fileEncoding, quiet = quiet)

  result <- list()
  if (substring(file[1], 1, 6) == "Casal2") {
    header <- list()
    header$call <- file[2]
    header$date <- file[3]
    header$seed <- file[4]
    header$version <- file[5]
    header$environment <- file[7]
    result[["header"]] <- header
    temp <- substr(result[["header"]]$version, 11, 15)
    if (temp != casal2.binary.version()) {
      cat("WARNING: The output file was generated with a different version than the R libray being used to read the output.\n")
      cat("This may cause compatibility issues. Please update the R package to be consistent with the version of Casal2 used to generate the output.\n")
      cat("The output was generated with Casal2 v", temp, "\n", sep = "")
      cat("The Casal2 R package is compatible with Casal2 v", casal2.binary.version(), "\n\n", sep = "")
    }
  }

  ## Check this isn't a tabular report by looking at the Call:
  if (grepl(pattern = "--tabular", x = file[2]) | grepl(pattern = "-t ", x = file[2])) {
    stop("This model was run with the command '--tabular' or '-t'. Please use the extract.tabular() function to import this model run.")
  }

  temp <- get.lines(file, starts.with = "\\*", fixed = F)
  if (length(temp) != 0) {
    if (!is.even(length(temp))) {
      ## find the report which doesn't have a *end
      nd_element <- seq(2, length(temp), by = 2)
      ndx <- which(temp[nd_element] != "*end")[1]
      stop(paste0("Each report section must begin with '*' and end with '*end'. The report beginning with '", temp[ndx], "' did not have a trailing '*end'."))
    }
    temp <- temp[is.odd(1:length(temp))]
    counter <- length(temp)
    ## iterate over all reports and see if this is a multi input run, this is identified by checking if -i in the header &
    ## if ALL reports are duplicated. Some reports will be duplicated because they are year based reports
    mult_input_run <- FALSE
    ## check if run is multi parameter
    for (i in 1:length(multi_input_args)) {
      if (grepl(pattern = multi_input_args[i], x = file[2])) {
        mult_input_run <- TRUE
        if (!quiet) {
          cat("loading a Casal2 output from a multi parameter input format\n")
        }
      }
    }


    multi_year_reports <- c("partition", "Partition", "partition_biomass", "PartitionBiomass", "partition_mean_weight", "PartitionMeanWeight", "age_length", "growth_increment", "selectivity_by_year")
    for (i in 1:counter) {
      header <- split.header(temp[i])
      label <- header[1]
      type <- tolower(header[2]) ## always lower case type.
      report <- get.lines(file, clip.to = temp[i])
      report <- get.lines(report, clip.from = "*end")
      report <- make.list(report)
      report$type <- type
      if (mult_input_run) {
        ## deal with the multi input run.
        if (!is.in(label, names(result))) {
          ## if first appearence of report register it as 1
          result[[label]][["1"]] <- list()
          if (type %in% multi_year_reports) {
            result[[label]][["1"]][[as.character(report$year)]] <- report
            result[[label]][["1"]][[as.character(report$year)]][["type"]] <- NULL
            result[[label]][["1"]][[as.character(report$year)]][["year"]] <- NULL

            result[[label]][["1"]][["type"]] <- type
          } else {
            result[[label]][["1"]] <- report
          }
        } else if (is.in(label, names(result)) & type %in% multi_year_reports) {
          ## if we have seen this report find out if we are adding to -i or year
          if (report$year %in% names(result[[label]][[as.character(length(result[[label]]))]])) {
            result[[label]][[as.character(length(result[[label]]) + 1)]][[as.character(report$year)]] <- report
            result[[label]][[as.character(length(result[[label]]))]][[as.character(report$year)]][["type"]] <- NULL
            result[[label]][[as.character(length(result[[label]]))]][[as.character(report$year)]][["year"]] <- NULL
            result[[label]][[as.character(length(result[[label]]))]][["type"]] <- type
          } else {
            result[[label]][[as.character(length(result[[label]]))]][[as.character(report$year)]] <- report
            result[[label]][[as.character(length(result[[label]]))]][[as.character(report$year)]][["type"]] <- NULL
            result[[label]][[as.character(length(result[[label]]))]][[as.character(report$year)]][["year"]] <- NULL
          }
        } else {
          ## a simple report
          result[[label]][[as.character(length(result[[label]]) + 1)]] <- report
        }
        file <- get.lines(file, clip.to = "*end")
      } else {
        ## deal with the single input run.
        if (type %in% multi_year_reports) {
          result[[label]][[as.character(report$year)]] <- report
          result[[label]][[as.character(report$year)]][["type"]] <- NULL
          result[[label]][[as.character(report$year)]][["year"]] <- NULL
          result[[label]][["type"]] <- type
        } else {
          result[[label]] <- report
        }
        file <- get.lines(file, clip.to = "*end")
      }
    }
    result <- set.class(result, "casal2MPD")
    result
  } else {
    warning("File is empty, no reports found")
  }
}
