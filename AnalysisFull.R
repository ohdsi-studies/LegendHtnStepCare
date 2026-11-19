# CohortGenerator, CohortDiagnostics, and CohortMethod

# Install packages (run only once)

install.packages(
  c("remotes", "dplyr", "RSQLite", "CirceR"),
  repos = "https://cloud.r-project.org"
)

remotes::install_version(
  "ParallelLogger",
  version = "3.5.1",
  repos = "https://cloud.r-project.org",
  upgrade = "never"
)

remotes::install_github(
  "OHDSI/DatabaseConnector",
  ref = "v7.0.0",
  upgrade = "never"
)

remotes::install_github(
  "OHDSI/ResultModelManager",
  ref = "v0.6.2",
  upgrade = "never"
)

remotes::install_github(
  "OHDSI/CohortGenerator",
  ref = "v1.0.1",
  upgrade = "never"
)

remotes::install_github(
  "OHDSI/CohortMethod",
  ref = "v5.5.2",
  upgrade = "never"
)

remotes::install_github(
  "OHDSI/CohortDiagnostics",
  ref = "c4b9c5706bb4418b9c3f4b397fb3ba1fae822e9d",
  upgrade = "never"
)

remotes::install_github(
  "OHDSI/Strategus",
  ref = "v1.3.1",
  upgrade = "never"
)

# Load packages

library(LegendHtnStepCare)
library(Strategus)
library(CohortGenerator)
library(CohortDiagnostics)
library(CohortMethod)
library(dplyr)
library(DatabaseConnector)
library(ShinyAppBuilder)
library(OhdsiShinyModules)
workingDir <- file.path(getwd(), '###') 
if(!file.exists(workingDir)) dir.create(workingDir, recursive = T)

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
cohortDefinitionOT2 <- read.csv(system.file("cohorts", "cohortDefinitionOT2.csv", package = "LegendHtnStepCare"))

# Read outcome definitions

outcomeCohortSet <- read.csv(
  system.file(
    "settings", "OutcomesOfInterest.csv",
    package = "LegendHtnStepCare"
  )
)

# Create outcome cohort definition set

outcomeDefinitionSet <-
  CohortGenerator::createEmptyCohortDefinitionSet()

for (i in seq_len(nrow(outcomeCohortSet))) {
  
  cohortJsonFileName <- system.file(
    "cohorts",
    paste0(outcomeCohortSet$name[i], ".json"),
    package = "LegendHtnStepCare"
  )
  
  if (cohortJsonFileName == "") next
  
  cohortJson <- readChar(
    cohortJsonFileName,
    file.info(cohortJsonFileName)$size
  )
  
  cohortExpression <-
    CirceR::cohortExpressionFromJson(cohortJson)
  
  cohortSql <- CirceR::buildCohortQuery(
    cohortExpression,
    options = CirceR::createGenerateOptions(
      generateStats = TRUE
    )
  )
  
  outcomeDefinitionSet <- rbind(
    outcomeDefinitionSet,
    data.frame(
      cohortId = outcomeCohortSet$cohortId[i],
      cohortName = outcomeCohortSet$atlasName[i],
      json = cohortJson,
      sql = cohortSql,
      stringsAsFactors = FALSE
    )
  )
}

# Base + OT2 Exposure
exposureDefinitionSet <- rbind(
  cohortDefinitionSet,
  cohortDefinitionOT2
)

# Base + OT2 + Outcome
cohortDefinitionSetAll <- rbind(
  exposureDefinitionSet,
  outcomeDefinitionSet
)

# Shared resources - Cohort Generator

cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSetAll) 
cohortGeneratorModuleSepcifications <- cgModuleSettingsCreator$createModuleSpecifications(generateStats = T)

# Cohort Diagnostics

cdModuleSettingsCreator <- Strategus::CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = exposureDefinitionSet$cohortId, 
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

# Negative Control Outcome cohort set
ncoCohortSet <- read.csv(system.file("settings","NegativeControls.csv",package = "LegendHtnStepCare"))

# Align NCO columns with CohortGenerator shared-resource format 
ncoCohortSet <- data.frame(
  cohortId = ncoCohortSet$cohortId,
  cohortName = ncoCohortSet$name,
  outcomeConceptId = ncoCohortSet$conceptId
)

# Negative control outcome shared resource
ncoSharedResource <-
  cgModuleSettingsCreator$
  createNegativeControlOutcomeCohortSharedResourceSpecifications(
    negativeControlOutcomeCohortSet = ncoCohortSet,
    occurrenceType = "all",
    detectOnDescendants = TRUE
  )

# Create CohortMethod module
cmModule <- CohortMethodModule$new()

# Negative control outcomes
negativeControlOutcomes <- lapply(
  X = ncoCohortSet$cohortId,
  FUN = CohortMethod::createOutcome,
  outcomeOfInterest = FALSE,
  trueEffectSize = 1
  )

# Outcomes of interest
outcomesOfInterest <- lapply(
  X = outcomeDefinitionSet$cohortId,   
  FUN = CohortMethod::createOutcome,
  outcomeOfInterest = TRUE
)

# Combine negative controls and outcomes of interest
outcomes <- append(
  negativeControlOutcomes,
  outcomesOfInterest
)

# Concept definitions for excluded covariates
conceptDefinition <- read.csv(system.file("settings","ConceptDefinition.csv",package = "LegendHtnStepCare"))

# Generate all TCO combinations
cohortInfo <- dplyr::bind_rows(
  cohortDefinitionSet[, c("cohortId", "cohortName")],
  cohortDefinitionOT2[, c("cohortId", "cohortName")]
)

cohortInfo$firstClass <- sub(
  "^First ([^ ]+) adding .*", "\\1", cohortInfo$cohortName
)
cohortInfo$secondClass <- sub(
  "^First [^ ]+ adding ([^ ]+).*", "\\1", cohortInfo$cohortName
)
cohortInfo$subgroupCode <- cohortInfo$cohortId %/% 100000
subgroupNames <- c(
  "10" = "Overall", "11" = "Young", "12" = "Middle",
  "13" = "Old", "14" = "Male", "15" = "Female",
  "16" = "White", "17" = "Black", "18" = "Hispanic"
)
cohortInfo$subgroup <- subgroupNames[
  as.character(ifelse(
    cohortInfo$subgroupCode %in% 20:28,
    cohortInfo$subgroupCode - 10,
    cohortInfo$subgroupCode
  ))
]

pairTable <- cohortInfo %>%
  split(interaction(cohortInfo$subgroupCode,
                    cohortInfo$firstClass, drop = TRUE)) %>%
  lapply(function(x) {
    pairs <- combn(seq_len(nrow(x)), 2)
    data.frame(
      targetId = x$cohortId[pairs[1, ]],
      comparatorId = x$cohortId[pairs[2, ]],
      firstClass = x$firstClass[pairs[1, ]],
      targetSecondClass = x$secondClass[pairs[1, ]],
      comparatorSecondClass = x$secondClass[pairs[2, ]],
      subgroupCode = x$subgroupCode[pairs[1, ]],
      cohortType = ifelse(x$subgroupCode[pairs[1, ]] >= 20, "OT2", "Base")
    )
  }) %>%
  dplyr::bind_rows()

pairTable$excludedIds <- lapply(seq_len(nrow(pairTable)), function(i) {
  excludeGroups <- c(
    pairTable$firstClass[i],
    pairTable$targetSecondClass[i],
    pairTable$comparatorSecondClass[i]
  )
  unique(conceptDefinition$CONCEPT_ID[
    conceptDefinition$group %in% excludeGroups
  ])
})

targetComparatorOutcomesList <- lapply(seq_len(nrow(pairTable)), function(i) {
  CohortMethod::createTargetComparatorOutcomes(
    targetId = pairTable$targetId[i],
    comparatorId = pairTable$comparatorId[i],
    outcomes = outcomes,
    excludedCovariateConceptIds = pairTable$excludedIds[[i]]
  )
})

# CohortMethod analysis settings
covarSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
  washoutPeriod = 365,
  firstExposureOnly = TRUE,
  removeDuplicateSubjects = "keep first", 
  maxCohortSize = 0,
  covariateSettings = covarSettings
)

#Intent-to-treat
createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
  minDaysAtRisk = 1,
  riskWindowStart = 1,
  startAnchor = "cohort start",
  riskWindowEnd = 99999,
  endAnchor = "cohort start"
)

#On-treatment-1,2
createStudyPopArgsOT1 <- CohortMethod::createCreateStudyPopulationArgs(
  minDaysAtRisk = 1,
  riskWindowStart = 1,
  startAnchor = "cohort start",
  riskWindowEnd = 0,
  endAnchor = "cohort end"
)

#Landmark
createStudyPopArgsLandmark <- CohortMethod::createCreateStudyPopulationArgs(
  minDaysAtRisk = 1,
  riskWindowStart = 365,
  startAnchor = "cohort start",
  riskWindowEnd = 99999,
  endAnchor = "cohort start"
)

#Propensity score model
createPsArgs <- CohortMethod::createCreatePsArgs(
  stopOnError = FALSE,
  control = Cyclops::createControl(
    cvRepetitions = 1
  )
)

#PS matching
matchOnPsArgs <- CohortMethod::createMatchOnPsArgs() 

#PS stratification
stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs()

#Outcome model
fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = TRUE
)

#Covariate balance
computeSharedCovBalArgs <- CohortMethod::createComputeCovariateBalanceArgs()

computeCovBalArgs <- CohortMethod::createComputeCovariateBalanceArgs(
  covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
)

# Generate six CohortMethod analyses
studyPops <- list(
  createStudyPopArgs,          # ITT
  createStudyPopArgsOT1,       # OT1 or OT2 depending on cohort definition
  createStudyPopArgsLandmark   # Landmark
)

descriptions <- c(
  "ITT matching", "OT matching", "Landmark matching",
  "ITT stratification", "OT stratification", "Landmark stratification"
)

cmAnalysisList <- lapply(1:6, function(i) {
  
  CohortMethod::createCmAnalysis(
    analysisId = i,
    description = descriptions[i],
    getDbCohortMethodDataArgs = getDbCmDataArgs,
    createStudyPopArgs = studyPops[[(i - 1) %% 3 + 1]],                  
    createPsArgs = createPsArgs,
    matchOnPsArgs = if (i <= 3) matchOnPsArgs else NULL,                        
    stratifyByPsArgs = if (i > 3) stratifyByPsArgs else NULL,                   
    computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
    computeCovariateBalanceArgs = computeCovBalArgs,
    fitOutcomeModelArgs = fitOutcomeModelArgs
  )
})

# Analyses to Exclude
ot2Pairs <- subset(
  pairTable,
  cohortType == "OT2",
  select = c(targetId, comparatorId)
)

analysesToExclude <- ot2Pairs[
  rep(seq_len(nrow(ot2Pairs)), each = 4),
]
analysesToExclude$analysisId <- rep(
  c(1, 3, 4, 6),
  times = nrow(ot2Pairs)
)
rownames(analysesToExclude) <- NULL

# Create CohortMethod module specification
cohortMethodModuleSpecifications <- cmModule$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = analysesToExclude
)

# analysis specifications
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(ncoSharedResource) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSepcifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(workingDir, 'studySpecification.json')
)

analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
  file.path(workingDir, 'studySpecification.json')
)

# Execute the study with Strategus

executionSettings <- Strategus::createCdmExecutionSettings(
  workDatabaseSchema = '', #please change this based on your schema name
  cdmDatabaseSchema = '', #please change this based on your schema name
  cohortTableNames = CohortGenerator::getCohortTableNames(
    cohortTable = '###'),
  workFolder = file.path(workingDir, 'strategusWork'),
  resultsFolder = file.path(workingDir, 'strategusOutput'),
  minCellCount = 5,
  maxCores = 1L
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

# PNG rendering (if needed)
options(
  bitmapType = "cairo",
  shiny.useragg = TRUE,
  shiny.usecairo = TRUE
)

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
