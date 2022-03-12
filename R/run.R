#'
#' @description Execute simulations depending on the object (nl or R object)
#'
#' @param obj nl or R object
#' @return tibble with simulation output results
#'
#' @aliases run_all
#' @rdname run_all
#'
#' @export

run_all <- function(obj) {

  ## Check if 'object' is a valid object type:
  if (obj@obj_type == "nl_obj") {
    print(paste("nl object choosen"))
    return ( run_nl_all(obj) )
  }

  if (obj@obj_type == "r_obj") {
    print(paste("R object choosen"))
    return ( run_r_all(obj) )
  }

  else {
    print(paste("The transfered object is not a valid object. Please transmit an nl or R object."))
  }

}
