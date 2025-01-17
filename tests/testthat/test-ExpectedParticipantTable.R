test_that("ExpectedParticipantTable works", {
  recruitment_profile <- data.frame(Site = c("Site A", "Site A", "Site B", "Site B"),
                                    RecruitmentPeriod = c("(0,50]", "(50,Inf]"),
                                    RecruitmentRate = c(1, 2, 3, 4))

  testObject <- expect_silent(ExpectedParticipantTable(recruitment_profile))

  expect_s3_class(testObject, "data.frame")
})
