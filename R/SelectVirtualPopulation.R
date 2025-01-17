# Function to retrieve Virtual Population filename
#' @title Select KerusCloud Virtual Population Simulations File
#' @author Gareth Burns
#' @description A function that uses the manifest file and the downloaded Virtual
#'     Population data from KerusCloud to identify simulation files.
#' @param simulation An integer. The number of the simulation.
#' @param manifest_file A data.file. Manifest file downloaded from KerusCloud.
#' @param selected_scenarios A named list. Name corresponds to the scenario type
#'   and the element corresponds to the desired label of the scenario.
#' @param ... Not used at present.
#' @return A character value. Corresponds to the individual
#'     simulation file from the simulation and scenarios specified.
#' @importFrom snakecase to_any_case
#' @importFrom purrr list_flatten
#' @export

SelectVirtualPopulation <-
  function(simulation,
           manifest_file,
           selected_scenarios = NULL,
           ...) {
    browser()
    # Validation on input arguments
    if (missing(simulation) ||
        !isTRUE(all.equal(simulation, as.integer(simulation))) ||
        simulation <= 0) {
      stop("Invalid or missing simulation argument")
    }

    # Identify which scenarios have been applied to the project
    # Only active scenarios have a respective column in manifest file
    validScenarios <-
      to_any_case(colnames(manifest_file)[grepl(".Scenarios", colnames(manifest_file))], case = "snake")
    if (length(validScenarios)) {
      if (!(all(validScenarios %in% names(selected_scenarios)))) {
        warning("There's scenario types in this project you haven't put as an argument in selected_scenarios. This may cause unexpected results")
      }
    }

    if (length(selected_scenarios)) {
    scenarioGrid <- expand.grid(selected_scenarios)

    files <-  apply(
      scenarioGrid,
      1L,
      FUN = function(expected_scenarios, manifest_file) {
        logicals <-
          lapply(seq_len(length(expected_scenarios)), function(index, scenario_grid) {
            manifest_file[[make.names(to_any_case(names(scenario_grid[index]), case = "title"), allow_ = FALSE)]] == scenario_grid[[index]]
          },
          scenario_grid = expected_scenarios)

        simulationLogical <-
          list(manifest_file$Simulation == simulation)
        logicals <- c(simulationLogical, logicals)

        index <-
          which(Reduce(function(a, b) {
            mapply(all, a, b)
          }, logicals))

        if (!length(index)) {
          stop("No files were found from the supplied simulation and scenarios")
        }
        return(manifest_file$Filename[[index]])
      },
      manifest_file = manifest_file
    )

    return(files)
    } else {
      index <- manifest_file$Simulation == simulation

      return(manifest_file$Filename[index])

    }

  }
