#' @title ExtractRecruitmentProfile
#' @author Gareth Burns
#' @description A function that uses the metadata file and the downloaded Virtual
#'     Population data from KerusCloud to extract the recruitment profile and return
#'     in a format for functional programming.
#' @param recruitment_json An list. The number of the simulation.
#' @param ... Not used at present.
#' @return A data.frame. Contains the following columns:
#'  \itemize{
#'    \item{site}
#'    \item{timepoint}
#'    \item{participants}
#'  }
#' @export

ExtractRecruitmentProfile <-
  function(recruitment_json, ...) {

    # Placeholder for data
    results <- data.frame(
      Scenario = character(),
      Site = character(),
      RecruitmentPeriod = character(),
      RecruitmentRate = numeric(),
      stringsAsFactors = FALSE
    )

    timeWindows <- recruitment_json$RecruitmentPeriods

    # Loop through each layer
    for (layer_name in names(recruitment_json$Layers)) {
      # Extract the layer
      layer <- recruitment_json$Layers[[layer_name]]

      # Get the Site
      site <- layer$LayerDefinition$Site

      # Loop through each parameter (Rate.0, Rate.1, etc.)
      for (rate_name in names(layer$Parameters)) {
        # Extract the rate parameter
        rate_param <- layer$Parameters[[rate_name]]

        # Get the RecruitmentPeriodID
        recruitment_timewindow <-
          Filter(function(period) {
          period$RecruitmentPeriodID == rate_param$RecruitmentPeriodID
          }, timeWindows)

        recruitment_timewindow <- paste0("(", recruitment_timewindow[[1L]]$LowerValue,
                                         ",", recruitment_timewindow[[1L]]$UpperValue, "]")

        # Check if the "Value" is a list (for "Scenario-based") or a single value
        if (is.list(rate_param$Value)) {
          # Scenario-based: loop over each scenario in the Value list
          for (scenario_id in names(rate_param$Value)) {
            rate <- rate_param$Value[[scenario_id]]

            # Append a row to the results data frame
            results <- rbind(
              results,
              data.frame(
                Scenario = scenario_id,
                Site = site,
                RecruitmentPeriod = recruitment_timewindow,
                RecruitmentRate = rate,
                stringsAsFactors = FALSE
              )
            )
          }

        } else {
          # Single value: use the single rate value
          rate <- rate_param$Value

          # Append a row to the results data frame
          results <- rbind(
            results,
            data.frame(
              Scenario = NA,
              Site = site,
              RecruitmentPeriod = recruitment_timewindow,
              RecruitmentRate = rate,
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }

    return(results)
  }
