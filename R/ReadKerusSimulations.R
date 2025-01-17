# Function to load a multiple KerusCloud simulation scenarios
#' @title Read KerusCloud Virtual Population Simulations
#' @rdname ReadKerusCloudSimulations
#' @author Gareth Burns
#' @description A function optimised to read in KerusCloud Virtual Population
#'   simulations.
#' @param files \strong{A character vector} Virtual Population file name(s).
#' @param directory \strong{A character value} Directory of the Virtual Population
#'  files. Defaults to current working directory.
#' @param manifest_file \strong{A data.frame} manifest file output by KerusCloud
#'   Virtual Population download. Optional.
#' @param as_dataframe \strong{A logical value.} Defaults to \code{TRUE}. If \code{TRUE} a
#'   \code{data.frame} is returned, if \code{FALSE} a list is returned.
#' @param ... Named arguments that can be passed to read.csv
#' @details
#'   If the manifest file is supplied it will retrieve the respective simulation
#'   and scenario details and append it as a column.
#'   \emph{Note}: If you supply the \code{nrows} and \code{colClasses} arguments to \code{...}
#'   this will make the import of files quicker.
#' @return A list of simulations scenarios or the total number of simulation
#'  scenarios in the manifest file.
#' @importFrom tools file_path_sans_ext
#' @export

ReadKerusSimulations <-
  function(files,
           directory = getwd(),
           manifest_file = NULL,
           as_dataframe = TRUE,
           ...) {
    stopifnot(is.character(files), is.logical((as_dataframe)))

    if (length(manifest_file)) {
      manifestDetails <-
        manifest_file[which(manifest_file$Filename %in% files),]
      # Append column where multiple scenarios exist to provide user
      # ability to differentiate
      catColumns <-
        apply(
          manifestDetails,
          2L,
          FUN = function(x)
            length(unique(x)) > 1
        )
      manifestDetails <- manifestDetails[, catColumns]
    } else {
      manifestDetails <- NULL
    }

    if (isTRUE(as_dataframe)) {
      virtualPopulationSimulations <-
        lapply(files, function(file) {
          manifestDetails <- manifestDetails[which(manifestDetails$Filename %in% file),][-1]
          simDataFrame <- read.csv(file.path(directory, file), ...)
          if (length(manifestDetails)) {
            simDataFrame <-
              cbind(simDataFrame, manifestDetails, row.names = NULL)
          }
          simDataFrame
        })

      return(do.call("rbind", virtualPopulationSimulations))

      return(virtualPopulationSimulations)
    } else {
      virtualPopulationSimulations <-
        setNames(lapply(files,  function(file) {
          read.csv(file.path(directory, file), ...)
        }),
        file_path_sans_ext(files))
    }
  }
