if(!require('renv')) install.packages('renv')
library(renv)

renv::activate()
renv::restore()
.rs.restartR()

library(Strategus)
library(dplyr)
library(ShinyAppBuilder)
library(OhdsiShinyModules)
library(DatabaseConnector)

workingDir <- file.path(getwd(), 'output')
if(!file.exist(workingDir)) dir.create(workingDir, recursive = T)

## Please fill below with your own database information
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = '',
  server = '',
  user = '',
  password = '',
  port = ''
)

# Read cohort definitions

cohortDefinitionSet <- read.csv(system.file("cohorts", "cohortDefinition.csv", package = "LegendHtnStepCare"))

# Shared resources - Cohort Generator

cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cohortGeneratorModuleSepcifications <- cgModuleSettingsCreator$createModuleSpecifications(generateStats = T)

# Cohort Diagnostics

cdModuleSettingsCreator <- Strategus::CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = T,
  runIncludedSourceConcepts = T,
  runOrphanConcepts = T,
  runTimeSeries = F,
  runVisitContext = T,
  runBreakdownIndexEvents = T,
  runIncidenceRate = T,
  runCohortRelationship = T,
  runTemporalCohortCharacterization = T,
  minCharacterizationMean = .01,
  irWashoutPeriod = 365
)

# Estimation module should be added here

analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSepcifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) 

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(workingDir, 'studySpecification.json')
)

analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
  file.path(workingDir, 'studySpecification.json')
)


# Execution the study with Strategus

executionSettings <- Strategus::createCdmExecutionSettings(
  workDatabaseSchema = 'main',
  cdmDatabaseSchema = 'main',
  cohortTableNames = CohortGenerator::getCohortTableNames(
    cohortTable = 'my_cohorts'),
  workFolder = file.path(workingDir, 'strategusWork'),
  resultsFolder = file.path(workingDir, 'strategusOutput'),
  minCellCount = 5
)

Strategus::execute(
  analysisSpecifications = analysisSpecifications,
  connectionDetails = connectionDetails,
  executionSettings = executionSettings
)

# Saving results to a sqlite database

resultsDatabaseSchema <- 'main'
resultsConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = file.path(workingDir, 'strategusOutput', 'results.sqlite')
)

analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
  file.path(workingDir, 'studySpecification.json')
)

resultsDataModelSettings <- Strategus::createResultsDataModelSettings(
  resultsDatabaseSchema = resultsDatabaseSchema,
  resultsFolder = file.path(workingDir, 'strategusOutput')
)

Strategus::createResultDataModel(
  analysisSpecifications = analysisSpecifications,
  resultsConnectionDetails = resultsConnectionDetails,
  resultsDataModelSettings = resultsDataModelSettings
)

Strategus::uploadResults(
  analysisSpecifications = analysisSpecifications,
  resultsConnectionDetails = resultsConnectionDetails,
  resultsDataModelSettings = resultsDataModelSettings
)

## ShinyViewer

shinyConfig <- initializeModuleConfig() |>
  addModuleConfig(
    createDefaultAboutConfig()
  ) |>
  addModuleConfig(
    createDefaultDatasourcesConfig()
  ) |>
  addModuleConfig(
    createDefaultCohortGeneratorConfig()
  ) |>
  addModuleConfig(
    createDefaultCohortDiagnosticsConfig()
  ) |>
  addModuleConfig(
    createDefaultCharacterizationConfig()
  ) |>
  addModuleConfig(
    createDefaultPredictionConfig()
  ) |>
  addModuleConfig(
    createDefaultEstimationConfig()
  )

ShinyAppBuilder::createShinyApp(
  config = shinyConfig,
  connectionDetails = resultsConnectionDetails,
  resultDatabaseSettings = createDefaultResultDatabaseSettings(
    schema = resultsDatabaseSchema
  )
)


