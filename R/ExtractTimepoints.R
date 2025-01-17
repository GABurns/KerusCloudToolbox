# Function to Extract Timepoints from Metadata JSON
#' @title Select KerusCloud Virtual Population Simulations File
#' @author Gareth Burns
#' @description A function that uses the Metadata JSON to extract the time points
#'   of each variable and create a long \code{data.frame}.
#' @param metadata An list. The KerusCloud Virtual Population metadata.
#' @param variables A character vector. Manifest file downloaded from KerusCloud.
#' @param infer_baseline A logical value. Infer missing values as timepoint = 0.
#' @param as_list A logical value. See details. Defaults to FALSE.
#' @param ... Not used at present.
#' @return A data.frame containing the following columns:
#'   \itemize{
#'     \item{Variable}
#'     \item(Timepoint)
#'   }
#'     simulation file from the simulation and scenarios specified.
#' @importFrom purrr pluck
#' @export

ExtractTimepoints <-
  function(metadata,
           variables = NULL,
           infer_baseline = FALSE,
           as_list = FALSE,
           ...) {
    stopifnot(is.list(metadata),
              is.logical(infer_baseline),
              is.logical(as_list))

    variableList <-  metadata$Variables

    # Filter the metadata to only include
    if (!is.null(variables)) {
      if (any(!(variables %in% names(variableList)))) {
        warning("Not all the supplied variables are in the metadata")
      }

      if (all(!(variables %in% names(variableList)))) {
        stop("None of the variables supplied exist in the supplied metadata")
      }

      variableList <-
        variableList[names(variableList) %in% variables]
    }

    # Extract the timepoints
    missingDefault <- if (infer_baseline) 0 else NA # NULLs get dropped for unlist

    variableTimepoints <-
      setNames(lapply(seq_along(variableList), function(index, variable_list) {
        variableLabel <- names(variable_list)[index]
        variable <- variable_list[[index]]

        time <-
          as.numeric(pluck(variable, "Timepoint", .default = missingDefault))

        return(time)

      }, variable_list = variableList),
      names(variableList))

    # Wrangle to a long data.frame
    if (isTRUE(as_list)) {
      return(variableTimepoints)
    } else {
      return(data.frame(
        variable = rep(names(variableTimepoints), lengths(variableTimepoints)),
        time = unlist(variableTimepoints),
        stringsAsFactors = FALSE
      ))
    }
  }
