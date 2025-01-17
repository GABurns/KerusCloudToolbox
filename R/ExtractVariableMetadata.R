# Function to load a multiple KerusCloud simulation scenarios
#' @title Read KerusCloud Virtual Population Simulations
#' @rdname ExtractVariableMetadata
#' @author Gareth Burns
#' @description A function optimised to read in KerusCloud Virtual Population
#'   simulations.
#' @param metadata \strong{A list} Virtual Population file name(s).
#' @param variable_names \strong{A character vector}. Name(s) of the
#' @param variarable_types \strong{A character value}. The names of the variable
#'   types. Can be one of:
#'   \itemize{
#'     \item{"Exponential"},
#'     \item{"Log-Normal"},
#'     \item{"Normal"},
#'     \item{"Uniform"},
#'     \item{"Weibull"},
#'     \item{"Binomial"},
#'     \item{"Negative-Binomial"},
#'     \item{"Multinomial"},
#'     \item{"Group"},
#'     \item{"Recruitment"},
#'     \item{"Time-to-Event"},
#'     \item{"Derived Multinomial"},
#'     \item{"Operational Derivation"},
#'     \item{"Continious RM"},
#'     \item{"Irreversible Event"},
#'     \item{"Reversible Event"}
#'     }
#' @param ... Not used at present.
#' @details
#'   The meta_data can also be the directory of the
#' @return A list containing the metadata of the selected variables from
#'   variable_names and variable_list arguments.
#' @importFrom tools file_path_sans_ext
#' @importFrom purrr keep
#' @export

ExtractVariableMetadata <-
  function(metadata,
           variable_names = NULL,
           variable_types = NULL,
           ...) {

    if (!is.list(metadata)) {
      metadata <- read_json(path = file.path(metadata))
    }

    variableList <- metadata$Variables

    # Optional variable_names argument
    if (!is.null(variable_names)) {
      variableList <-
        variableList[names(variableList) %in% variable_names]
    }

    if (!is.null(variable_types)) {
      variableList <- keep(
        variableList,
        .p = function(x) {
          ("Type" %in% names(x) &&
          x$Type %in% variable_types)
        }
      )
    }
    return(variableList)
  }
