context("[Bayesian Quality Control] Bayesian Process Capability Study")

# jaspTools cannot expand the Common.PlotLayout / Common.Priors components used by
# inst/qml/bayesianProcessCapabilityStudies.qml, so analysisOptions() returns an
# incomplete set. These helpers supply the missing options with their qml defaults.
.plotLayoutOptions <- function(base, checked = FALSE) {
  o <- list()
  o[[base]]                                       <- checked
  o[[paste0(base, "IndividualPointEstimate")]]     <- FALSE
  o[[paste0(base, "IndividualPointEstimateType")]] <- "mean"
  o[[paste0(base, "IndividualCi")]]                <- FALSE
  o[[paste0(base, "IndividualCiType")]]            <- "central"
  o[[paste0(base, "IndividualCiMass")]]            <- 95
  o[[paste0(base, "IndividualCiLower")]]           <- 0
  o[[paste0(base, "IndividualCiUpper")]]           <- 1
  o[[paste0(base, "IndividualCiBf")]]              <- "1"
  o[[paste0(base, "TypeLower")]]                   <- 0
  o[[paste0(base, "TypeUpper")]]                   <- 1
  o[[paste0(base, "PanelLayout")]]                 <- "multiplePanels"
  o[[paste0(base, "Axes")]]                        <- "free"
  o[[paste0(base, "custom_x_min")]]                <- 0
  o[[paste0(base, "custom_x_max")]]                <- 1
  o[[paste0(base, "custom_y_min")]]                <- 0
  o[[paste0(base, "custom_y_max")]]                <- 1
  o[[paste0(base, "PriorDistribution")]]           <- FALSE
  o
}

.plotBases <- c("posteriorDistributionPlot", "priorDistributionPlot",
                "sequentialAnalysisPointEstimatePlot", "sequentialAnalysisPointIntervalPlot",
                "posteriorPredictiveDistributionPlot", "priorPredictiveDistributionPlot")

# Default options for the analysis, with spec limits matched to datasets/processCapability.csv
# (40 observations, roughly normal around 10 with sd 0.5).
.bpcsOptions <- function() {
  options <- analysisOptions("bayesianProcessCapabilityStudies")
  extra   <- c(do.call(c, lapply(.plotBases, .plotLayoutOptions)),
               list(axisLabels = FALSE, normalModelComponentsList = list(), tModelComponentsList = list()))
  options[names(extra)] <- extra

  options$capabilityStudyType          <- "normalCapabilityAnalysis"
  options$measurementLongFormat        <- "measurement"
  options$priorSettings                <- "default"
  options$lowerSpecificationLimit      <- TRUE
  options$lowerSpecificationLimitValue <- 8.5
  options$target                       <- TRUE
  options$targetValue                  <- 10
  options$upperSpecificationLimit      <- TRUE
  options$upperSpecificationLimitValue <- 11.5
  # keep the sampler cheap, this analysis refits per observation in the sequential plots
  options$noChains                     <- 1
  options$noWarmup                     <- 200
  options$noIterations                 <- 1000
  options
}

.capabilityRows <- function(results) {
  rows <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
}

## Capability table ####

options <- .bpcsOptions()
set.seed(1)
results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

test_that("Analysis runs to completion", {
  expect_equal(results[["status"]], "complete")
  expect_null(results[["results"]][["errorMessage"]])
})

test_that("Capability table reports every metric the user selected", {
  # regression: qc names these Cpu/Cpl and errors on CpU/CpL, mismatched casing
  # silently dropped both metrics from the table
  expect_equal(.capabilityRows(results)$metric, c("Cp", "Cpu", "Cpl", "Cpk", "Cpc", "Cpm"))
})

test_that("Capability table estimates are plausible for a well centred process", {
  df <- .capabilityRows(results)
  # LSL 8.5, USL 11.5, sd about 0.5 => Cp near 1
  expect_equal(df$mean[df$metric == "Cp"], 1.0, tolerance = 0.25)
  # the sampler makes these stochastic, so only assert the ordering that must hold
  expect_true(all(df$lower < df$mean))
  expect_true(all(df$mean  < df$upper))
  expect_true(df$mean[df$metric == "Cpk"] <= df$mean[df$metric == "Cp"])
})

test_that("Deselecting metrics removes them from the table", {
  options <- .bpcsOptions()
  options$Cpu <- FALSE
  options$Cpl <- FALSE
  options$Cpc <- FALSE
  options$Cpm <- FALSE
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)
  expect_equal(.capabilityRows(results)$metric, c("Cp", "Cpk"))
})

## Estimation ####

test_that("Estimates are deterministic across runs", {
  # qc::bpc defaults to method = "integration", so the fit is numerical rather
  # than sampled and repeated runs must agree exactly.
  #
  # NOTE: this is also why the MCMC Settings group in the qml currently has no
  # effect on the capability table. noChains/noWarmup/noIterations are passed to
  # qc::bpc (they used to be ignored entirely) but only take effect on the mcmc
  # path, which the analysis never selects because there is no qml control for
  # the estimation method. Either add that control or drop the settings group.
  options <- .bpcsOptions()
  set.seed(1)
  first  <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)
  set.seed(2)
  second <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(.capabilityRows(first)$mean, .capabilityRows(second)$mean)
  expect_equal(.capabilityRows(first)$sd,   .capabilityRows(second)$sd)
})

## Plots ####

test_that("Distribution and predictive plots are produced without error", {
  options <- .bpcsOptions()
  options$posteriorDistributionPlot           <- TRUE
  options$priorDistributionPlot               <- TRUE
  options$posteriorPredictiveDistributionPlot <- TRUE
  options$priorPredictiveDistributionPlot     <- TRUE
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  for (base in c("posteriorDistributionPlot", "priorDistributionPlot",
                 "posteriorPredictiveDistributionPlot", "priorPredictiveDistributionPlot")) {
    plotName <- results[["results"]][[base]][["data"]]
    expect_true(!is.null(plotName), info = base)
  }
  expect_length(results[["state"]][["figures"]], 4)
})

## Interval table ####

test_that("Interval table is produced without error", {
  options <- .bpcsOptions()
  options$intervalTable <- TRUE
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  expect_true(length(results[["results"]][["bpcsIntervalTable"]][["data"]]) > 0)
})

## Readiness ####

test_that("Analysis stays empty until the specification limits are set", {
  options <- .bpcsOptions()
  options$lowerSpecificationLimit <- FALSE
  options$upperSpecificationLimit <- FALSE
  options$target                  <- FALSE
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  expect_length(.capabilityRows(results), 0)
})
