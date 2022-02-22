#
#
# setClass("sim_obj", slots = c("experiment", "simdesign"))
#
# setClass("nl",
#          contains = "sim_obj")
#
#
# nl <- new("sim_obj", experiment, simdesign)
#
# nl@experiment(...)
# nl@simdesign(...)
#
#
# r_obj <- new("sim_obj", experiment, simdesign)
#
# r_obj@experiment(...)
# r_obj@simdesign(...)
#
#
# experiment <- function(expname = "defaultexp",
#                        outpath = NA_character_,
#                        repetition = 1,
#                        tickmetrics = "true",
#                        idsetup = "setup",
#                        idgo = "go",
#                        idfinal = NA_character_,
#                        idrunnum = NA_character_,
#                        runtime = 1,
#                        evalticks = NA_integer_,
#                        stopcond= NA_character_,
#                        metrics = c("count turtles"),
#                        metrics.turtles = list(),
#                        metrics.patches = NA_character_,
#                        metrics.links = list(),
#                        variables = list(),
#                        constants = list(),
#                        ...) {
#
#   methods::new("experiment",
#                expname=expname,
#                outpath=outpath,
#                repetition=repetition,
#                tickmetrics=tickmetrics,
#                idsetup=idsetup,
#                idgo=idgo,
#                idfinal=idfinal,
#                idrunnum=idrunnum,
#                runtime=runtime,
#                evalticks=evalticks,
#                stopcond=stopcond,
#                metrics=metrics,
#                metrics.turtles=metrics.turtles,
#                metrics.patches=metrics.patches,
#                metrics.links=metrics.links,
#                variables=variables,
#                constants=constants,
#                ...)
#
# }
#
#
# simdesign <- function(simmethod = character(),
#                       siminput = tibble::tibble(),
#                       simobject = list(),
#                       simseeds = NA_integer_,
#                       simoutput = tibble::tibble(),
#                       ...) {
#
#   methods::new("simdesign",
#                simmethod=simmethod,
#                siminput=siminput,
#                simobject=simobject,
#                simseeds=simseeds,
#                simoutput=simoutput)
#
# }
