#' Print content of r object
#'
#' @description Print content of r object and embedded experiment and simdesign objects to console
#' @param x r object to print
#' @param ... further arguments passed to or from other methods
#' @details
#' Print content of the provided r object in a readable format.
#'
#' @examples
#'
#' print(r_lhs)
#'
#' @aliases print.r
#' @rdname print.r
#'
#' @export
print.r <- function(x, ...)
{
  util_print.r(x)
  util_print.experiment(x@experiment)
  util_print.simdesign(x@simdesign)
  util_print.summary(x)
}

#' Print r object content
#'
#' @description Print r object content
#' @param x r object
#' @param ... further arguments passed to or from other methods
#' @details
#' Print content of r object in a nice formatted overview to console
#' @aliases util_print.r
#' @rdname util_print.r
#' @keywords internal
util_print.r <- function(x, ...) {

  # Styles:
  style_heading <- crayon::black$bold$bgWhite
  style_def <- crayon::green
  style_opt <- crayon::yellow
  style_na <- crayon::red


  cat(style_heading(paste0("\n", "   R OBJECT   ", "\n")))

  cat("model package = ")
  output <- paste0(x@modeltype, "\n")
  cat(ifelse(nchar(x@modeltype) > 0, style_def(output), style_na(output)))

}

#' Print r object summary
#'
#' @description Print r object summary
#' @param x r object
#' @param ... further arguments passed to or from other methods
#' @details
#' Print summary of r object and embedded experiment and simdesign objects in a nice formatted overview to console
#' @aliases util_print.summary
#' @rdname util_print.summary
#' @keywords internal
util_print.summary <- function(x, ...)
{
  # Styles:
  style_heading <- crayon::black$bold$bgWhite
  style_def <- crayon::green
  style_opt <- crayon::yellow
  style_na <- crayon::red

  cat(style_heading(paste0("\n", "   SUMMARY   ", "\n")))

  cat("valid experiment name: ")
  output <- ifelse(is.na(x@experiment@expname) | grepl("\\s", getexp(x, "expname")), style_na("\u2717"), style_def("\u2713"))
  cat(paste0(output, "\n"))

  cat("outpath exists on local system: ")
  output <- ifelse(dir.exists(x@experiment@outpath), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("variables defined: ")
  output <- ifelse(length(x@experiment@variables) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))


  cat("constants defined: ")
  output <- ifelse(length(x@experiment@constants) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("simdesign attached: ")
  output <- ifelse(!is.na(x@simdesign@simmethod), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("siminput parameter matrix: ")
  output <- ifelse(nrow(x@simdesign@siminput) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("number of siminputrows: ")
  output <- ifelse(nrow(x@simdesign@siminput) > 0, style_def(nrow(x@simdesign@siminput)), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("number of random seeds: ")
  output <- ifelse(!all(is.na(x@simdesign@simseeds)), style_def(length(x@simdesign@simseeds)), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("estimated number of runs: ")
  output <- ifelse(!all(nrow(x@simdesign@siminput) == 0, is.na(x@simdesign@simseeds)),
                   style_def(nrow(x@simdesign@siminput) * length(x@simdesign@simseeds)),
                   style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("simoutput results attached: ")
  output <- ifelse(nrow(x@simdesign@simoutput) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

}

#' Print experiment object content
#'
#' @description Print experiment object content
#' @param x experiment object
#' @param ... further arguments passed to or from other methods
#' @details
#' Print content of experiment object in a nice formatted overview to console
#' @aliases util_print.experiment
#' @rdname util_print.experiment
#' @keywords internal
util_print.experiment <- function(x, ...)
{
  # Styles:
  style_heading <- crayon::black$bold$bgWhite
  style_def <- crayon::green
  style_opt <- crayon::yellow
  style_na <- crayon::red

  cat(style_heading(paste0("\n", "   EXPERIMENT   ", "\n")))

  cat("model function         = ")
  output <- paste0(x@expname, "\n")
  cat(ifelse(!is.na(x@expname), style_def(output), style_na(output)))

  cat("Output path            = ")
  output <- paste0(x@outpath, "\n")
  cat(ifelse(!is.na(x@outpath), style_def(output), style_na(output)))

  cat("Runtime (ticks)        = ")
  output <- paste0(x@runtime, "\n")
  cat(ifelse(!is.na(x@runtime), style_def(output), style_na(output)))

  cat(paste0("\n", "Variable parameters (input)", "\n"))
  output <- paste0(paste(paste0("    ", names(x@variables)), x@variables, collapse="\n", sep=" = "), "\n")
  cat(ifelse(!all(is.na(x@variables)), style_def(output), style_opt(output)))

  cat(paste0("\n", "Constant parameters (input)", "\n"))
  output <- paste0(paste(paste0("    ", names(x@constants)), x@constants, sep=" = ", collapse="\n"), "\n")
  cat(ifelse(!all(is.na(x@constants)), style_def(output), style_opt(output)))

}

#' Print simdesign object content
#'
#' @description Print simdesign object content
#' @param x simdesign object
#' @param ... further arguments passed to or from other methods
#' @details
#' Print content of simdesign object in a nice formatted overview to console
#' @aliases util_print.simdesign
#' @rdname util_print.simdesign
#' @keywords internal
util_print.simdesign <- function(x, ...)
{
  # Styles:
  style_heading <- crayon::black$bold$bgWhite
  style_def <- crayon::green
  style_opt <- crayon::yellow
  style_na <- crayon::red

  cat(style_heading(paste0("\n", "   SIMDESIGN   ", "\n")))

  cat("Simulation method      = ")
  output <- paste0(x@simmethod, "\n")
  cat(ifelse(!identical(x@simmethod, character(0)), style_def(output), style_na(output)))

  cat("Simulation object      = ")
  output <- paste0(x@simobject, "\n")
  cat(ifelse(length(x@simmethod) > 0, style_def(output), style_opt(output)))

  cat("Generated random seeds = ")
  output <- paste0(paste(x@simseeds, collapse=", "), "\n")
  cat(ifelse(!all(is.na(x@simseeds)), style_def(output), style_opt(output)))

  cat(paste0("\n", "Parameter matrix (input)", "\n"))
  print(x@siminput, width = Inf)

  cat(paste0("\n", "Simulation results (output)", "\n"))
  print(x@simoutput, width = Inf)

}


