

## aktuell sind die Inhalte die gleichen wie in der "run_nl.R Datei"


#' Execute all R simulations from an R object
#'
#' @description Execute all R simulations from an R object with a defined experiment and simdesign
#'
#' @param r r object
#' @param split number of parts the job should be split into
#' @param cleanup.csv TRUE/FALSE, if TRUE temporary created csv output files will be deleted after gathering results.
#' @param cleanup.xml TRUE/FALSE, if TRUE temporary created xml output files will be deleted after gathering results.
#' @param cleanup.bat TRUE/FALSE, if TRUE temporary created bat/sh output files will be deleted after gathering results.
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
#' nl <- nl_lhs
#'
#' # Execute all simulations from an nl object with properly attached simdesign.
#' results <- run_r_all(nl)
#'
#' # Run in parallel on local machine:
#' library(future)
#' plan(multisession)
#' results <- run_r_all(nl)
#'
#' }
#' @aliases run_r_all
#' @rdname run_r_all
#'
#' @export

run_r_all <- function(r,
                       split = 1,
                       cleanup.csv = TRUE,
                       cleanup.xml = TRUE,
                       cleanup.bat = TRUE) {
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

          # Update progress bar:
          p(sprintf("row %d/%d seed %d",
                    siminputrow, nrow(getsim(r, "siminput")),
                    job_seed))
          # Run simulation
          res_one <- run_r_one(
            r = r,
            seed = job_seed,
            siminputrow = siminputrow,
            cleanup.csv = cleanup.csv,
            cleanup.xml = cleanup.xml,
            cleanup.bat = cleanup.bat
          )
          return(res_one)
        })
      return(res_job)
    })
  return(nl_results)
}



#' Execute one r simulation from a r object
#'
#' @description Execute one r simulation from a r object with a defined experiment and simdesign
#'
#' @param r nl object
#' @param seed a random seed for the NetLogo simulation
#' @param siminputrow rownumber of the input tibble within the attached simdesign object that should be executed
#' @param cleanup.csv TRUE/FALSE, if TRUE temporary created csv output files will be deleted after gathering results.
#' @param cleanup.xml TRUE/FALSE, if TRUE temporary created xml output files will be deleted after gathering results.
#' @param cleanup.bat TRUE/FALSE, if TRUE temporary created bat/sh output files will be deleted after gathering results.
#' @param writeRDS TRUE/FALSE, if TRUE an rds file with the simulation results will be written to the defined outpath folder of the experiment within the nl object.
#' @return tibble with simulation output results
#' @details
#'
#' run_r_one executes one simulation...
#'
#' @examples
#' \dontrun{
#'
#' # Load nl object from test data:
#' r <- nl_lhs
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
                       siminputrow,
                       cleanup.csv = TRUE,
                       cleanup.xml = TRUE,
                       cleanup.bat = TRUE,
                       writeRDS = FALSE) {

  util_eval_simdesign(r)

  ## Write XML File:
  xmlfile <-
    tempfile(
      pattern = paste0("nlrx_seed_", seed, "_row_", siminputrow, "_"),
      fileext = ".xml"
    )

  util_create_sim_XML(r, seed, siminputrow, xmlfile)

  ## Execute:
  outfile <-
    tempfile(
      pattern = paste0("nlrx_seed_", seed, "_row_", siminputrow, "_"),
      fileext = ".csv"
    )

  batchpath <- util_read_write_batch(r)
  util_call_nl(r, xmlfile, outfile, batchpath)

  ## Read results
  nl_results <- util_gather_results(r, outfile, seed, siminputrow)

  ## Delete temporary files:
  cleanup.files <- list("csv" = outfile,
                        "xml" = xmlfile,
                        "bat" = batchpath)

  util_cleanup(r, cleanup.csv, cleanup.xml, cleanup.bat, cleanup.files)


  if (isTRUE(writeRDS))
  {
    if(dir.exists(r@experiment@outpath))
    {
      filename <- paste0("nlrx_seed_", seed, "_row_", siminputrow, ".rds")
      saveRDS(nl_results, file=file.path(r@experiment@outpath, filename))
    } else
    {
      warning(paste0("Outpath of nl object does not exist on remote file system: ", r@experiment@outpath, ". Cannot write rds file!"))
    }
  }

  return(nl_results)
}
