test_that("ExtractRecruitmentProfile works", {

  # Example of metadata JSON
  json_data <-  list(
    "Type" = "Recruitment",
    "Description" = "",
    "Timepoint" = "NA",
    "Levels" = list("SiteA",
                    "B"),
    "Advanced Options" = "",
    "Estimands" = "",
    "RecruitmentPeriods" = list(
      "RecruitmentPeriod.0" = list(
        "RecruitmentPeriodID" = "efa4a279-73c5-4020-8c6b-c138985aeb4d",
        "LowerValue" = 0,
        "UpperValue" = 50
      ),
      "RecruitmentPeriod.1" = list(
        "RecruitmentPeriodID" = "3680fb52-6823-43e3-abc7-b721c2499a15",
        "LowerValue" = 50,
        "UpperValue" = "Inf"
      )
    ),
    "Layers" = list(
      "Layer.0" = list(
        "LayerDefinition" = list("Site" = "B"),
        "Parameters" = list(
          "Rate.0" = list(
            "Type" = "Scenario-based",
            "Value" = list(
              "6356df3b-8f08-4258-b399-ad51860be945" = 10,
              "23453366-6090-4293-8f16-c089c577dda9" = 20
            ),
            "RecruitmentPeriodID" = "efa4a279-73c5-4020-8c6b-c138985aeb4d"
          ),
          "Rate.1" = list(
            "Type" = "Scenario-based",
            "Value" = list(
              "6356df3b-8f08-4258-b399-ad51860be945" = 30,
              "23453366-6090-4293-8f16-c089c577dda9" = 50
            ),
            "RecruitmentPeriodID" = "3680fb52-6823-43e3-abc7-b721c2499a15"
          )
        )
      ),
      "Layer.1" = list(
        "LayerDefinition" = list("Site" = "SiteA"),
        "Parameters" = list(
          "Rate.0" = list(
            "Type" = "Single",
            "Value" = 5,
            "RecruitmentPeriodID" = "efa4a279-73c5-4020-8c6b-c138985aeb4d"
          ),
          "Rate.1" = list(
            "Type" = "Single",
            "Value" = 10,
            "RecruitmentPeriodID" = "3680fb52-6823-43e3-abc7-b721c2499a15"
          )
        )
      )
    )
  )

  testObject <-  expect_silent(ExtractRecruitmentProfile(recruitment_json = json_data))

  expect_s3_class(testObject, "data.frame")
})


