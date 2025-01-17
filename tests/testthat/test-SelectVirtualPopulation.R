test_that("Correct file name output", {
  # Mock
  manifest <- data.frame(Filename = paste0("Sim", 1:10, ".csv"),
                         Simulation = 1:10)

  expect_identical(SelectVirtualPopulation(simulation = 3, manifest_file = manifest),
                   "Sim3.csv")


  # Mock with single variable scenario
  manifest <- data.frame(Simulation = rep(1:10, 3),
                         Variable.Scenarios = c(rep("Low", 10), rep("Medium", 10), rep("High", 10)))
  manifest$Filename <-
    paste0("Sim",
           manifest$Simulation,
           "VarScen",
           manifest$Variable.Scenarios,
           ".csv")

  expect_identical(
    SelectVirtualPopulation(
      simulation = 9,
      manifest_file = manifest,
      selected_scenario = list(variable_scenario = "Low")
    ),
    "Sim9VarScenLow.csv"
  )

  # Mock with multiple scenarios
  manifest <- data.frame(
    Simulation = 1:10,
    Variable.Scenarios = c(rep("Low", 10), rep("Medium", 10), rep("High", 10)),
    Correlation.Scenarios = c(rep("High", 30), rep("Low", 30))
  )

  manifest$Filename <-
    paste0(
      "Sim",
      manifest$Simulation,
      "VarScen",
      manifest$Variable.Scenarios,
      "CorScen" ,
      manifest$Correlation.Scenarios,
      ".csv"
    )

  expect_identical(
    SelectVirtualPopulation(
      simulation = 9,
      manifest_file = manifest,
      selected_scenario = c(variable_scenario = "Low", correlation_scenario = "Low")
    ),
    "Sim9VarScenLowCorScenLow.csv"
  )

})


test_that("Errors returned on incorrect inputs", {
  expect_error(SelectVirtualPopulation())
  expect_error(SelectVirtualPopulation(simulation = 1))

  # Mock manifest file
  manifest <-
    data.frame(Filename = paste0(rep(paste(
      sample(LETTERS, 10), collapse = ""
    ), 10), ".csv"),
    Simulation = 1:10)

  expect_error(SelectVirtualPopulation(simulation = -1, manifest_file = manifest))
  expect_error(SelectVirtualPopulation(simulation = 1.5, manifest_file = manifest))
  expect_error(SelectVirtualPopulation(simulation = c(1, 2), manifest_file = manifest))

  # Mock manifest file with Variable Scenario
  manifest <-
    data.frame(
      Filename = paste0(rep(paste(
        sample(LETTERS, 10), collapse = ""
      ), 10), ".csv"),
      Simulation = rep(1:10, 3),
      Variable.Scenario = c(rep("Low", 10), rep("Medium", 10), rep("High", 10))
    )

  # Missing scenario argument
  expect_error(SelectVirtualPopulation(simulation = c(1, 2), manifest_file = manifest))
})
