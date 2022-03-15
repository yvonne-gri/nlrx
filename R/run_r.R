
#' Execute all R simulations from an R object
#'
#' @description Execute all R simulations from an R object with a defined experiment and simdesign
#'
#' @param r R object
#' @param split number of parts the job should be split into
#' @return tibble with simulation output results
#' @details
#'
#' run_r_all executes all simulations of the specified R model within the provided R object.
#' The function loops over all random seeds and all rows of the siminput table of the simdesign of R.
#' The loops are created by calling \link[furrr]{future_map_dfr}, which allows running the function either locally or on remote HPC machines.
#' The logical cleanup variables can be set to FALSE to preserve temporary generated output files (e.g. for debugging).
#'
#' When using run_r_all in a parallelized environment (e.g. by setting up a future plan using the future package),
#' the outer loop of this function (random seeds) creates jobs that are distributed to available cores of the current machine.
#' The inner loop (siminputrows) distributes simulation tasks to these cores.
#' However, it might be advantageous to split up large jobs into smaller jobs for example to reduce the total runtime of each job.
#' This can be done using the split parameter. If split is > 1 the siminput matrix is split into smaller parts.
#' Jobs are created for each combination of part and random seed.
#' If the split parameter is set such that the siminput matrix can not be splitted into equal parts, the procedure will stop and throw an error message.
#'
#'
#' @examples
#' \dontrun{
#'
#' # Load R object from test data:
#' r <- r_lhs
#'
#' # Execute all simulations from an R object with properly attached simdesign.
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
  r_results <- furrr::future_map_dfr(
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
            siminputrow = siminputrow
          )
          return(res_one)
        })
      return(res_job)
    })
  return(r_results)
}



#' Execute one R simulation from a R object
#'
#' @description Execute one R simulation from a R object with a defined experiment and simdesign
#'
#' @param r R object
#' @param seed a random seed for the R simulation
#' @param siminputrow rownumber of the input tibble within the attached simdesign object that should be executed
#' @return tibble with simulation output results
#' @details
#'
#' run_r_one executes one simulation of the specified R model within the provided R object.
#' Therefore, the model call is generated as an R expression from the library name, the algorithm/function used
#' and the constants and variables passed to the R object and it is evaluated.
#' All variables and parameters are taken from the r object.
#' This makes the run_r_one function adaptable to all algorithms.

#'
#' The random seed is set within the R model to control stochasticity.
#' The siminputrow number defines which row of the input data tibble within
#' the simdesign object of the provided R object is executed.
#' If the output of the model is not a tibble, it must be converted before returning it.
#'
#' This function can be used to run single simulations of the given R model.
#'
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

  # Evaluate all slots of a simdesign object
  util_eval_simdesign(r)


  # Generate the generic model call expression
  call <- paste0( r@modeltype, '::', r@experiment@expname, '( ' )

  if ( length(r@experiment@constants != 0 ) ) {
    for ( i in 1:length(r@experiment@constants) ) {
      if ( i == (length(r@experiment@constants) + length(r@experiment@variables) ))
        call <- paste0( call, names(r@experiment@constants[i]), ' = ', r@experiment@constants[[i]], ' )'  )
      else
        call <- paste0( call, names(r@experiment@constants[i]), ' = ', r@experiment@constants[[i]], ', '  )
    }
  }

  if ( length(r@experiment@variables != 0 ) ) {
    for ( j in 1:length(r@experiment@variables) ) {
      if ( j == (length(r@experiment@constants) + length(r@experiment@variables) ))
        call <- paste0( call, names(r@experiment@variables[j]), ' = ', r@experiment@variables[[j]], ' )'  )
      else
        call <- paste0( call, names(r@experiment@variables[j]), ' = ', r@experiment@variables[[j]], ', '  )
    }
  }

  # Print call for visualization
  print(call)

  # Evaluate the call expression
  results <- eval(parse(text = call))

  # Graphical output of the model call
  print(landscapetools::show_landscape(results))


  # Convert model output to data-frame, the colnames are handled seperately
  data_frame <- data.frame( r@modeltype, r@experiment@expname)
  vector_colnames <- c( "modeltype", "expname" )

  counter <- 3   # Counter for assigning the colnames

  if ( length(r@experiment@variables != 0 ) ) {
    for ( i in 1:length(r@experiment@variables) ) {
      data_frame <- data.frame( data_frame, r@experiment@variables[[i]] )
      vector_colnames[counter] <- c( names(r@experiment@variables[i]) )
      counter <- counter+1
    }
  }

  if ( length(r@experiment@constants != 0 ) ) {
    for ( i in 1:length(r@experiment@constants) ) {
      data_frame <- data.frame( data_frame, r@experiment@constants[[i]] )
      vector_colnames[counter] <- c( names(r@experiment@constants[i]) )
      counter <- counter+1
    }
  }

  # Assign the colnames
  colnames(data_frame) <- vector_colnames

  # Extension of the data_frame with the results
  data_frame <- data.frame(data_frame, raster::as.data.frame(results))


  # Convert the model output into tibble
  tibble_results <- as_tibble(data_frame)

  return(tibble_results)
}
