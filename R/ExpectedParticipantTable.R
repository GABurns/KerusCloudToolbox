#' @title ExpectedParticipantTable
#' @author Gareth Burns
#' @description A function that uses the manifest file and the downloaded Virtual
#'     Population data from KerusCloud to create a table with the expected number
#'     of participants per site and at the end of each supplied time period.
#' @param recruitment_profile An data.frame The number of the simulation.
#' @param ... Not used at present.
#' @return A data.frame. Contains the following columns:
#'  \itemize{
#'    \item{Site}
#'    \item{Timepoint}
#'    \item{participants}
#'  }
#' @export

ExpectedParticipantTable <-
  function(recruitment_profile,
           end_timepoint = NULL,
           ...) {
    if (length(unique(recruitment_profile$RecruitmentPeriod)) == 1 &&
        is.null(end_timepoint)) {
      stop("If only one RecruitmentPeriod then a end_timepoint must be suplied")
    }

    recruitment_profile$lower_timepoint <-
      as.numeric(sub(
        "\\((.+),.*",
        "\\1",
        recruitment_profile$RecruitmentPeriod
      ))

    recruitment_profile$upper_timepoint <-
      as.numeric(sub(
        "[^,]*,([^]]*)\\]",
        "\\1",
        recruitment_profile$RecruitmentPeriod
      ))

    if (length(unique(recruitment_profile$recruitment_period)) == 1 &&
        is.null(end_timepoint)) {
      stop("If only one recruitment_period then a end_timepoint must be suplied")

      if (end_timepoint <= max(recruitment_profile$lower_endpoint)) {
        stop("The end_timepoint must be greater than the maximum ")
      }
    }




    if (is.null(end_timepoint)) {
      end_timepoint <-
        recruitment_profile$lower_timepoint[nrow(recruitment_profile)] + (recruitment_profile$upper_timepoint[nrow(recruitment_profile) - 1L] - recruitment_profile$lower_timepoint[nrow(recruitment_profile) - 1L])
    }


    recruitment_profile$upper_timepoint[is.infinite(recruitment_profile$upper_timepoint)] <-
      end_timepoint

    recruitment_profile$duration <-
      recruitment_profile$upper_timepoint - recruitment_profile$lower_timepoint


    recruitment_profile$Timepoint <-
      recruitment_profile$upper_timepoint

    recruitment_profile$Participants <-
      recruitment_profile$duration * recruitment_profile$RecruitmentRate

    expectedParticipantTable <-
      recruitment_profile[c("Site", "Timepoint", "Participants")]

    # Add in timepoint 0 for each Site
    expectedParticipantTable <- rbind(expectedParticipantTable,
                                      data.frame(
                                        Site = unique(expectedParticipantTable$Site),
                                        Timepoint = 0,
                                        Participants = 0
                                      ))

    # Sort Table to expected order
    expectedParticipantTable <-

      expectedParticipantTable[order(expectedParticipantTable$Site,
                                     expectedParticipantTable$Timepoint,
                                     decreasing = FALSE), ]

    expectedParticipantTable$Participants <-  ave(expectedParticipantTable$Participants,
                                                  expectedParticipantTable$Site, FUN = cumsum)

    return(expectedParticipantTable)
  }
