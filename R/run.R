

#'
#' nl, später aber object
#'
#' @description Execute simulations from R object
#'
#' @param nl nl object
#' @return tibble with simulation output results
#'
#' @aliases run_all
#' @rdname run_all
#'
#' @export

run_all <- function(used_obj) {

  # test, welches Objekt übergeben wurde muss eingebaut werden

  ## Check if 'object' is a valid object:
  if (used_obj@obj_type == "nl_obj") {
    print(paste("nl object choosen"))
    return ( run_nl_all(used_obj) )
    # return ( run_nl_one(used_obj) )
    # return ( run_nl_dyn(used_obj) )
  }

  if (used_obj@obj_type == "r_obj") {
    print(paste("R object choosen"))
    return ( run_r_all(used_obj) )
  }

  else {
    print(paste("The transfered object is not a valid object. Please transmit an nl or R object."))
  }

}
