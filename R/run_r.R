
#' Execute all R simulations from an R object
#'
#' @description Execute all R simulations from an R object with a defined experiment and simdesign
#'
#' @param r r object
#' @param split number of parts the job should be split into
#' @return tibble with simulation output results
#' @details
#'
#' run_r_all executes all simulations ....
#'
#'
#' @examples
#' \dontrun{
#'
#' # Load nl object from test data:
#' r <- r_lhs
#'
#' # Execute all simulations from an r object with properly attached simdesign.
#' results <- run_r_all(r)
#'
#' # Run in parallel on local machine:
#' library(future)
#' plan(multisession)
#' results <- run_r_all(r)
#'
#' }
#' @aliases run_r_all
#' @rdname run_r_all
#'
#' @export

run_r_all <- function(r, split = 1) {

  ## Store the number of siminputrows
  siminput_nrow <- nrow(getsim(r, "siminput"))
  ## Check if split parameter is valid:
  if (siminput_nrow %% split != 0) {
    stop(
      "Modulo of split parameter and number of rows of the siminput matrix is
      not 0. Please adjust split parameter to a valid value!",
      call. = FALSE
    )
  }

  ## Calculate size of one part:
  n_per_part <- siminput_nrow / split

  ## Generate job ids from seeds and parts:
  jobs <- as.list(expand.grid(getsim(r, "simseeds"), seq(1:split)))

  ## Setup progress bar:
  total_steps <- siminput_nrow * length(getsim(r, "simseeds"))
  p <- progressr::progressor(steps = total_steps)

  ## Execute on remote location
  nl_results <- furrr::future_map_dfr(
    seq_along(jobs[[1]]),
    function(job) {
      ## Extract current seed and part from job id:
      job_seed <- jobs[[1]][[job]]
      job_part <- jobs[[2]][[job]]
      ## Calculate rowids of the current part:
      rowids <-
        seq(1:n_per_part) +
        (job_part - 1) * n_per_part

      ## Start inner loop to run model simulations:
      res_job <- furrr::future_map_dfr(
        rowids,
        function(siminputrow) {
          # print(paste(siminputrow))

          # Update progress bar:
          p(sprintf("row %d/%d seed %d",
                    siminputrow, nrow(getsim(r, "siminput")),
                    job_seed))

          # Run simulation
          res_one <- run_r_one(
            r = r,
            seed = job_seed,
            siminputrow = siminputrow
          )
          # print(paste("R object choosen3"))
          # print(paste("res_one:"))
          # print(res_one)
          # print(paste("nach res_one"))
          return(res_one)
        })
      # print(paste("vor res_job return"))
      return(res_job)
    })
  # print(paste("vor nl_result return"))
  return(nl_results)
}



#' Execute one r simulation from a r object
#'
#' @description Execute one r simulation from a r object with a defined experiment and simdesign
#'
#' @param r nl object
#' @param seed a random seed for the NetLogo simulation
#' @param siminputrow rownumber of the input tibble within the attached simdesign object that should be executed
#' @return tibble with simulation output results
#' @details
#'
#' run_r_one executes one simulation...
#'
#' @examples
#' \dontrun{
#'
#' # Load r object from test data:
#' r <- r_lhs
#'
#' # Run one simulation:
#' results <- run_r_one(r = r,
#'                       seed = getsim(r, "simseeds")[1],
#'                       siminputrow = 1)
#'
#' }
#' @aliases run_r_one
#' @rdname run_r_one
#'
#' @export

run_r_one <- function(r,
                      seed,
                      siminputrow) {

  util_eval_simdesign(r)

  # print(paste("vor nlmr Aufruf"))
  # print(r_new@simdesign@siminput$minl[siminputrow])
  # print(r_new@simdesign@siminput$maxl[siminputrow])


  if (r_new@simdesign@siminput$minl[siminputrow] > r_new@simdesign@siminput$maxl[siminputrow]) {

    minl_tmp <- r_new@simdesign@siminput$minl[siminputrow]
    r_new@simdesign@siminput$minl[siminputrow] <- r_new@simdesign@siminput$maxl[siminputrow]
    r_new@simdesign@siminput$maxl[siminputrow] <- minl_tmp

  }


  results <- NLMR::nlm_randomrectangularcluster(ncol = 50, nrow = 30, minl = round(r_new@simdesign@siminput$minl[siminputrow]), maxl = round(r_new@simdesign@siminput$maxl[siminputrow]))

  print(landscapetools::show_landscape(results))


  # print(paste("nach nlmr Aufruf"))
  # print(results)

  data <- data.frame(minl = round(r_new@simdesign@siminput$minl[siminputrow]),
                     maxl = round(r_new@simdesign@siminput$maxl[siminputrow]),
                     expname = environment(results@rotation@transfun)[["r_new"]]@experiment@expname,
                     ncol = r_new@experiment@constants$ncol,
                     nrow = r_new@experiment@constants$nrow)

  # print(paste("nach data.frame Aufruf"))
  # print(data)

  tibble_data <- as_tibble(data)

  # print(paste("nach as_tibble Aufruf"))
  # print(tibble_data)


  return(tibble_data)
}
