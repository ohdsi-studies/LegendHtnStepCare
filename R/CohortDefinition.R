## Create exposure cohorts -- Dual combination (stepped care only) of antihypertensive drugs


# install.packages("remotes")
# remotes::install_github("OHDSI/Hades", upgrade = "always")

library(remotes)
library(dplyr)
library(Capr)
library(CirceR)
library(testthat)
library(DatabaseConnector)

workDir = file.path(getwd())

## Load the concept sets
conceptDefinition = read.csv(file.path(workDir, 'inst/settings/ConceptDefinition.csv'), stringsAsFactors = F)

## Create Capr concept sets
createConceptSets <- function(cs1, cs2) {
  cs_antihypertensive <- cs(descendants(conceptDefinition %>%
                                          filter(group == 'Antihypertensive') %>%
                                          select(CONCEPT_ID) %>% pull()),
                            name = 'Antihypertensives')
  
  cs_first <- cs(descendants(cs1),
                 ### Exclude single pill combination -- based on RxNorm Classification
                 descendants(exclude(
                   conceptDefinition %>%
                     filter(group == 'Single Pill Combination') %>%
                     select(CONCEPT_ID) %>% pull()
                 )),
                 name = 'First agent')
  
  cs_second <- cs(descendants(cs2),
                  name = 'Second agent')
  
  cs_others <- cs(
    descendants(setdiff(conceptDefinition %>%
                          filter(group == 'Antihypertensive') %>%
                          select(CONCEPT_ID) %>% pull(),
                        c(cs1,cs2))),
    descendants(exclude(cs1, cs2)),
    name = 'Other agents')
  
  cs_hypertension <- cs(descendants(conceptDefinition %>%
                                      filter(group == 'Hypertension') %>%
                                      select(CONCEPT_ID) %>% pull()),
                        descendants(exclude(conceptDefinition %>%
                                              filter(group == 'Hypertension Exclude') %>%
                                              select(CONCEPT_ID) %>% pull())),
                        name = 'Hypertension')
  
  cs_previousCVevent <- cs(c(conceptDefinition %>%
                               filter(group %in% c('Other Stroke')) %>%
                               select(CONCEPT_ID) %>% pull()),
                           descendants(conceptDefinition %>%
                                         filter(group %in% c('Cerebral Infarction', 'Myocardial Infarction', 'Heart Failure')) %>%
                                         select(CONCEPT_ID) %>% pull()),
                           descendants(exclude(conceptDefinition %>%
                                                 filter(group %in% c('Old MI', 'Rheumatic HF')) %>%
                                                 select(CONCEPT_ID) %>% pull())),
                           name = 'Previous CV events including acute MI, stroke, HF')
  
  result = list(cs_antihypertensive, cs_first, cs_second, cs_others, cs_hypertension, cs_previousCVevent)
  names(result) = c('cs_antihypertensive', 'cs_first', 'cs_second', 'cs_others', 'cs_hypertension', 'cs_previousCVevent')
  return(result)
}

## Create multiple cohorts using the function and concept sets

createCohortDefinition = function(connection, cdm_database_schema, idConvention = 1000000,
                                  cohortNameTag = '', createFunction = createCohortDefinitionMain, OT2 = F){
  primary = c('ACEI', 'ARB', 'ndCCB', 'dCCB', 'Thiazide')
  secondary = c('ACEI', 'ARB', 'BB', 'ndCCB', 'dCCB', 'Thiazide')
  cohortDefinition = primary %>% merge(secondary, all = T) %>% filter(x != y) %>% mutate(combi = paste(y, 'after', x))
  cohortDefinition = cohortDefinition %>% filter(combi != 'ACEI after ARB' & combi != 'ARB after ACEI') %>% arrange(combi) %>% mutate(combi = paste(combi, cohortNameTag))
  cohortDefinition$cohort_id = seq(1:nrow(cohortDefinition)) + idConvention
  cohortDefinition$json = NA
  cohortDefinition$sql = NA
  cohortDefinition = cohortDefinition %>% select(cohort_id, x, y, combi, json, sql)
  colnames(cohortDefinition) = c('cohortId', 'First', 'Second', 'Combi', 'json', 'sql')
  
  for(i in 1:nrow(cohortDefinition)){
    result = createFunction(cs1 = conceptDefinition %>% filter(group == cohortDefinition$First[i]) %>% select(CONCEPT_ID) %>% pull(),
                            cs2 = conceptDefinition %>% filter(group == cohortDefinition$Second[i]) %>% select(CONCEPT_ID) %>% pull(),
                            OT2 = OT2)
    cohortDefinition$json[i] = result$json
    cohortDefinition$sql[i] = result$sql
    rm(result)
  }
  
  cohortDefinition = cohortDefinition %>% select(-First, -Second)
  colnames(cohortDefinition) = c('cohortId', 'cohortName', 'json', 'sql')
  
  for (i in 1:nrow(cohortDefinition)){
    cohortDefinitionJson = cohortDefinition$json[i] %>% jsonlite::fromJSON()
    for(j in 1:length(cohortDefinitionJson$ConceptSets$expression$items)){
      cohortDefinitionJson$ConceptSets$expression$items[[j]]$concept = merge(cohortDefinitionJson$ConceptSets$expression$items[[j]]$concept %>% select(CONCEPT_ID) %>% mutate(rowId = row_number()),
                                                                             conceptDefinition %>% select(-group), by = 'CONCEPT_ID', all.x = T) %>% unique() %>% arrange(rowId) %>% select(-rowId)
    }
    cohortDefinition$json[i] = jsonlite::toJSON(cohortDefinitionJson, pretty = T, auto_unbox = T)
  }
  return(cohortDefinition)
}

### additional functions
limitDistinctConcept <- function (cohortJson){
  cohortJsonList = jsonlite::fromJSON(cohortJson)
  
  ## Adding additional features (not supported by CapR) to Json
  ### Below depends the order of criteria. 
  cohortJsonList$InclusionRules$expression$CriteriaList[[3]]$Occurrence = cohortJsonList$InclusionRules$expression$CriteriaList[[3]]$Occurrence %>% 
    mutate(IsDistinct = TRUE, CountColumm = 'DOMAIN_CONCEPT')
  
  cohortJson = jsonlite::toJSON(cohortJsonList, pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  
  return(cohortJson)
}
addSecondDrugExitSql <- function(cohortJson){
  sql = CirceR::buildCohortQuery(expression = cohortJson,
                                 options = CirceR::createGenerateOptions(generateStats = F))
  
  ## Adding an additional queries to restrict the cohort end based on the end date of the first agent which used for monotherapy.
  sqlSplitted <- SqlRender::splitSql(sql) %>% as.list()
  addingClauses <- sqlSplitted[grep(sqlSplitted, pattern = c('drugTarget'))]
  addingClauses <- gsub('#drugTarget', replacement = '#drugTarget2', addingClauses)
  addingClauses <- gsub('#strategy_ends', replacement = '#strategy_ends2', addingClauses)
  addingClauses <- gsub('cs.codeset_id = 0', replacement = 'cs.codeset_id = 1', addingClauses)
  addingClauses <- addingClauses %>% as.list()
  
  updateClauses <- sqlSplitted[grep(sqlSplitted, pattern = c('INTO #cohort_rows'))]
  updateClauses <- gsub('UNION ALL', replacement = 'UNION ALL \n select event_id, person_id, end_date from #strategy_ends2 \n UNION ALL', updateClauses)
  updateClauses <- updateClauses %>% as.list()
  
  removingClauses <- sqlSplitted[grep(sqlSplitted, pattern = c('TABLE #strategy_ends'))]
  removingClauses <- gsub('#strategy_ends', replacement = '#strategy_ends2', removingClauses)
  removingClauses <- gsub('}\r\n\r\nTRUNCATE', replacement = 'TRUNCATE', removingClauses)
  removingClauses <- removingClauses %>% as.list()
  
  sqlUpdated <- c(
    sqlSplitted[1:max(grep(sqlSplitted, pattern = c('drugTarget')))],
    addingClauses,
    updateClauses,
    Filter(function(x) !is.null(x), sqlSplitted[grep(sqlSplitted, pattern = c('INTO #cohort_rows'))+1:length(sqlSplitted)]),
    removingClauses)
  
  sqlMerged <- paste0(unlist(sqlUpdated), collapse = ';\n')
  return(sqlMerged)
}

## Skeleton for the main definition
createCohortDefinitionMain <- function(cs1, cs2, OT2 = F){
  
  conceptSet <- createConceptSets(cs1, cs2)
  
  cd <- cohort(
    entry = entry(drugExposure(conceptSet[['cs_second']], age(gte(18)), firstOccurrence()),
                  observationWindow = continuousObservation(priorDays = 365),
                  qualifiedLimit = 'First'),
    attrition = attrition(
      withAll(atLeast(1, query = drugExposure(conceptSet[['cs_first']]), duringInterval(eventStarts(-Inf, -30, index = 'startDate')))),
      withAll(atLeast(1, query = conditionOccurrence(conceptSet[['cs_hypertension']]), duringInterval(eventStarts(-Inf, 0, index = 'startDate')))),
      withAll(exactly(1, query = drugExposure(conceptSet[['cs_antihypertensive']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      withAll(exactly(0, query = drugExposure(conceptSet[['cs_others']]), duringInterval(eventStarts(0, 180, index = 'startDate')))),
      withAll(exactly(0, query = conditionOccurrence(conceptSet[['cs_previousCVevent']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      expressionLimit = 'All'),
    exit = exit(endStrategy = drugExit(conceptSet[['cs_second']], persistenceWindow = 30, surveillanceWindow = 30),
                if(OT2){censor = censoringEvents(drugExposure(conceptSet[['cs_others']]))}
                ),
    era = era(eraDays = 0L)
  )
  
  cohortJson = cd %>% toCirce() %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  cohortJson = limitDistinctConcept(cohortJson)
  sql = addSecondDrugExitSql(cohortJson)
  result = data.frame(json = cohortJson, sql = sql)
  return(result)
}


### Age groups (18-44 / 45-64 / 65+)
createCohortDefinitionAgeSubgroup1 <- function(cs1, cs2, OT2){
  conceptSet <- createConceptSets(cs1, cs2)
  cd <- cohort(
    entry = entry(drugExposure(conceptSet[['cs_second']], age(bt(18, 44)), firstOccurrence()),
                  observationWindow = continuousObservation(priorDays = 365),
                  qualifiedLimit = 'First'),
    attrition = attrition(
      withAll(atLeast(1, query = drugExposure(conceptSet[['cs_first']]), duringInterval(eventStarts(-Inf, -30, index = 'startDate')))),
      withAll(atLeast(1, query = conditionOccurrence(conceptSet[['cs_hypertension']]), duringInterval(eventStarts(-Inf, 0, index = 'startDate')))),
      withAll(exactly(1, query = drugExposure(conceptSet[['cs_antihypertensive']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      withAll(exactly(0, query = drugExposure(conceptSet[['cs_others']]), duringInterval(eventStarts(0, 180, index = 'startDate')))),
      withAll(exactly(0, query = conditionOccurrence(conceptSet[['cs_previousCVevent']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      expressionLimit = 'All'),
    exit = exit(endStrategy = drugExit(conceptSet[['cs_second']], persistenceWindow = 30, surveillanceWindow = 30),
                if(OT2) {censor = censoringEvents(drugExposure(conceptSet[['cs_others']]))}
    ),
    era = era(eraDays = 0L)
  )
  
  cohortJson = cd %>% toCirce() %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  cohortJson = limitDistinctConcept(cohortJson)
  sql = addSecondDrugExitSql(cohortJson)
  result = data.frame(json = cohortJson, sql = sql)
  return(result)
}
createCohortDefinitionAgeSubgroup2 <- function(cs1, cs2, OT2){
  conceptSet <- createConceptSets(cs1, cs2)
  cd <- cohort(
    entry = entry(drugExposure(conceptSet[['cs_second']], age(bt(45, 64)), firstOccurrence()),
                  observationWindow = continuousObservation(priorDays = 365),
                  qualifiedLimit = 'First'),
    attrition = attrition(
      withAll(atLeast(1, query = drugExposure(conceptSet[['cs_first']]), duringInterval(eventStarts(-Inf, -30, index = 'startDate')))),
      withAll(atLeast(1, query = conditionOccurrence(conceptSet[['cs_hypertension']]), duringInterval(eventStarts(-Inf, 0, index = 'startDate')))),
      withAll(exactly(1, query = drugExposure(conceptSet[['cs_antihypertensive']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      withAll(exactly(0, query = drugExposure(conceptSet[['cs_others']]), duringInterval(eventStarts(0, 180, index = 'startDate')))),
      withAll(exactly(0, query = conditionOccurrence(conceptSet[['cs_previousCVevent']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      expressionLimit = 'All'),
    exit = exit(endStrategy = drugExit(conceptSet[['cs_second']], persistenceWindow = 30, surveillanceWindow = 30),
                if(OT2){censor = censoringEvents(drugExposure(conceptSet[['cs_others']]))}
    ),
    era = era(eraDays = 0L)
  )
  
  cohortJson = cd %>% toCirce() %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  cohortJson = limitDistinctConcept(cohortJson)
  sql = addSecondDrugExitSql(cohortJson)
  result = data.frame(json = cohortJson, sql = sql)
  return(result)
}
createCohortDefinitionAgeSubgroup3 <- function(cs1, cs2, OT2){
  conceptSet <- createConceptSets(cs1, cs2)
  cd <- cohort(
    entry = entry(drugExposure(conceptSet[['cs_second']], age(gte(65)), firstOccurrence()),
                  observationWindow = continuousObservation(priorDays = 365),
                  qualifiedLimit = 'First'),
    attrition = attrition(
      withAll(atLeast(1, query = drugExposure(conceptSet[['cs_first']]), duringInterval(eventStarts(-Inf, -30, index = 'startDate')))),
      withAll(atLeast(1, query = conditionOccurrence(conceptSet[['cs_hypertension']]), duringInterval(eventStarts(-Inf, 0, index = 'startDate')))),
      withAll(exactly(1, query = drugExposure(conceptSet[['cs_antihypertensive']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      withAll(exactly(0, query = drugExposure(conceptSet[['cs_others']]), duringInterval(eventStarts(0, 180, index = 'startDate')))),
      withAll(exactly(0, query = conditionOccurrence(conceptSet[['cs_previousCVevent']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      expressionLimit = 'All'),
    exit = exit(endStrategy = drugExit(conceptSet[['cs_second']], persistenceWindow = 30, surveillanceWindow = 30),
                if(OT2){censor = censoringEvents(drugExposure(conceptSet[['cs_others']]))}
    ),
    era = era(eraDays = 0L)
  )
  
  cohortJson = cd %>% toCirce() %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  cohortJson = limitDistinctConcept(cohortJson)
  sql = addSecondDrugExitSql(cohortJson)
  result = data.frame(json = cohortJson, sql = sql)
  return(result)
}
### Sex groups (Female/male)
createCohortDefinitionMaleSubgroup <- function(cs1, cs2, OT2){
  conceptSet <- createConceptSets(cs1, cs2)
  cd <- cohort(
    entry = entry(drugExposure(conceptSet[['cs_second']], age(gte(18)), male(), firstOccurrence()),
                  observationWindow = continuousObservation(priorDays = 365),
                  qualifiedLimit = 'First'),
    attrition = attrition(
      withAll(atLeast(1, query = drugExposure(conceptSet[['cs_first']]), duringInterval(eventStarts(-Inf, -30, index = 'startDate')))),
      withAll(atLeast(1, query = conditionOccurrence(conceptSet[['cs_hypertension']]), duringInterval(eventStarts(-Inf, 0, index = 'startDate')))),
      withAll(exactly(1, query = drugExposure(conceptSet[['cs_antihypertensive']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      withAll(exactly(0, query = drugExposure(conceptSet[['cs_others']]), duringInterval(eventStarts(0, 180, index = 'startDate')))),
      withAll(exactly(0, query = conditionOccurrence(conceptSet[['cs_previousCVevent']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      expressionLimit = 'All'),
    exit = exit(endStrategy = drugExit(conceptSet[['cs_second']], persistenceWindow = 30, surveillanceWindow = 30),
                if(OT2){censor = censoringEvents(drugExposure(conceptSet[['cs_others']]))}
    ),
    era = era(eraDays = 0L)
  )
  
  cohortJson = cd %>% toCirce() %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  cohortJson = limitDistinctConcept(cohortJson)
  sql = addSecondDrugExitSql(cohortJson)
  result = data.frame(json = cohortJson, sql = sql)
  return(result)
}
createCohortDefinitionFemaleSubgroup <- function(cs1, cs2, OT2){
  conceptSet <- createConceptSets(cs1, cs2)
  cd <- cohort(
    entry = entry(drugExposure(conceptSet[['cs_second']], age(gte(18)), female(), firstOccurrence()),
                  observationWindow = continuousObservation(priorDays = 365),
                  qualifiedLimit = 'First'),
    attrition = attrition(
      withAll(atLeast(1, query = drugExposure(conceptSet[['cs_first']]), duringInterval(eventStarts(-Inf, -30, index = 'startDate')))),
      withAll(atLeast(1, query = conditionOccurrence(conceptSet[['cs_hypertension']]), duringInterval(eventStarts(-Inf, 0, index = 'startDate')))),
      withAll(exactly(1, query = drugExposure(conceptSet[['cs_antihypertensive']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      withAll(exactly(0, query = drugExposure(conceptSet[['cs_others']]), duringInterval(eventStarts(0, 180, index = 'startDate')))),
      withAll(exactly(0, query = conditionOccurrence(conceptSet[['cs_previousCVevent']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      expressionLimit = 'All'),
    exit = exit(endStrategy = drugExit(conceptSet[['cs_second']], persistenceWindow = 30, surveillanceWindow = 30),
                if(OT2){censor = censoringEvents(drugExposure(conceptSet[['cs_others']]))}
    ),
    era = era(eraDays = 0L)
  )
  
  cohortJson = cd %>% toCirce() %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  cohortJson = limitDistinctConcept(cohortJson)
  sql = addSecondDrugExitSql(cohortJson)
  result = data.frame(json = cohortJson, sql = sql)
  return(result)
}
### Race and ethnicity (White, Black, Hispanic)
createCohortDefinitionWhiteSubgroup <- function(cs1, cs2, OT2){
  conceptSet <- createConceptSets(cs1, cs2)
  cd <- cohort(
    entry = entry(drugExposure(conceptSet[['cs_second']], age(gte(18)), firstOccurrence()),
                  observationWindow = continuousObservation(priorDays = 365),
                  qualifiedLimit = 'First'),
    attrition = attrition(
      withAll(atLeast(1, query = drugExposure(conceptSet[['cs_first']]), duringInterval(eventStarts(-Inf, -30, index = 'startDate')))),
      withAll(atLeast(1, query = conditionOccurrence(conceptSet[['cs_hypertension']]), duringInterval(eventStarts(-Inf, 0, index = 'startDate')))),
      withAll(exactly(1, query = drugExposure(conceptSet[['cs_antihypertensive']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      withAll(exactly(0, query = drugExposure(conceptSet[['cs_others']]), duringInterval(eventStarts(0, 180, index = 'startDate')))),
      withAll(exactly(0, query = conditionOccurrence(conceptSet[['cs_previousCVevent']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      expressionLimit = 'All'),
    exit = exit(endStrategy = drugExit(conceptSet[['cs_second']], persistenceWindow = 30, surveillanceWindow = 30),
                if(OT2){censor = censoringEvents(drugExposure(conceptSet[['cs_others']]))}
    ),
    era = era(eraDays = 0L)
  )
  
  cohortJson = cd %>% toCirce() %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  cohortJsonList = jsonlite::fromJSON(cohortJson)
  
  ## Adding race/ethnicity manually
  oldExp = cohortJsonList$InclusionRules$expression
  newExp = data.frame(Type = c(oldExp$Type, 'ALL'))
  newExp$CriteriaList = as.list(c(oldExp$CriteriaList, list(list())))
  newExp$DemographicCriteriaList = as.list(c(oldExp$DemographicCriteriaList, 
                                             list(list())))
  newExp$DemographicCriteriaList[[6]] = data.frame(Race = NA)
  newExp$DemographicCriteriaList[[6]]$Race =list(data.frame(CONCEPT_CODE = '5', CONCEPT_ID = as.integer(8527), CONCEPT_NAME = 'White', DOMAIN_ID = 'Race', INVALID_REASON_CAPTION = 'Unknown', STANDARD_CONCEPT_CAPTION = 'Unknown', VOCABULARY_ID = 'Race'))
  newExp$Groups = as.list(c(oldExp$Groups, list(list())))
  
  cohortJsonList$InclusionRules = data.frame(name = c(cohortJsonList$InclusionRules$name, 'rule6'))
  cohortJsonList$InclusionRules$expression = newExp
  rm(oldExp, newExp)
  
  cohortJson = cohortJsonList %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  cohortJson = limitDistinctConcept(cohortJson)
  sql = addSecondDrugExitSql(cohortJson)
  result = data.frame(json = cohortJson, sql = sql)
  return(result)
}
createCohortDefinitionBlackSubgroup <- function(cs1, cs2, OT2){
  conceptSet <- createConceptSets(cs1, cs2)
  cd <- cohort(
    entry = entry(drugExposure(conceptSet[['cs_second']], age(gte(18)), firstOccurrence()),
                  observationWindow = continuousObservation(priorDays = 365),
                  qualifiedLimit = 'First'),
    attrition = attrition(
      withAll(atLeast(1, query = drugExposure(conceptSet[['cs_first']]), duringInterval(eventStarts(-Inf, -30, index = 'startDate')))),
      withAll(atLeast(1, query = conditionOccurrence(conceptSet[['cs_hypertension']]), duringInterval(eventStarts(-Inf, 0, index = 'startDate')))),
      withAll(exactly(1, query = drugExposure(conceptSet[['cs_antihypertensive']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      withAll(exactly(0, query = drugExposure(conceptSet[['cs_others']]), duringInterval(eventStarts(0, 180, index = 'startDate')))),
      withAll(exactly(0, query = conditionOccurrence(conceptSet[['cs_previousCVevent']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      expressionLimit = 'All'),
    exit = exit(endStrategy = drugExit(conceptSet[['cs_second']], persistenceWindow = 30, surveillanceWindow = 30),
                if(OT2){censor = censoringEvents(drugExposure(conceptSet[['cs_others']]))}
    ),
    era = era(eraDays = 0L)
  )
  
  cohortJson = cd %>% toCirce() %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  cohortJsonList = jsonlite::fromJSON(cohortJson)
  
  ## Adding race/ethnicity manually
  oldExp = cohortJsonList$InclusionRules$expression
  newExp = data.frame(Type = c(oldExp$Type, 'ALL'))
  newExp$CriteriaList = as.list(c(oldExp$CriteriaList, list(list())))
  newExp$DemographicCriteriaList = as.list(c(oldExp$DemographicCriteriaList, 
                                             list(list())))
  newExp$DemographicCriteriaList[[6]] = data.frame(Race = NA)
  newExp$DemographicCriteriaList[[6]]$Race =list(data.frame(CONCEPT_CODE = c('3', '3.01'), CONCEPT_ID = c(8516, 38003598),
                                                            CONCEPT_NAME = c('Black or African American', 'Black'),
                                                            DOMAIN_ID = c('Race', 'Race'),
                                                            INVALID_REASON_CAPTION = c('Unknown', 'Unknown'),
                                                            STANDARD_CONCEPT_CAPTION = c('Unknown', 'Unknown'),
                                                            VOCABULARY_ID = c('Race', 'Race')))
  newExp$Groups = as.list(c(oldExp$Groups, list(list())))
  
  cohortJsonList$InclusionRules = data.frame(name = c(cohortJsonList$InclusionRules$name, 'rule6'))
  cohortJsonList$InclusionRules$expression = newExp
  rm(oldExp, newExp)
  
  cohortJson = cohortJsonList %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  cohortJson = limitDistinctConcept(cohortJson)
  sql = addSecondDrugExitSql(cohortJson)
  result = data.frame(json = cohortJson, sql = sql)
  return(result)
}
createCohortDefinitionHispanicSubgroup <- function(cs1, cs2, OT2){
  conceptSet <- createConceptSets(cs1, cs2)
  cd <- cohort(
    entry = entry(drugExposure(conceptSet[['cs_second']], age(gte(18)), firstOccurrence()),
                  observationWindow = continuousObservation(priorDays = 365),
                  qualifiedLimit = 'First'),
    attrition = attrition(
      withAll(atLeast(1, query = drugExposure(conceptSet[['cs_first']]), duringInterval(eventStarts(-Inf, -30, index = 'startDate')))),
      withAll(atLeast(1, query = conditionOccurrence(conceptSet[['cs_hypertension']]), duringInterval(eventStarts(-Inf, 0, index = 'startDate')))),
      withAll(exactly(1, query = drugExposure(conceptSet[['cs_antihypertensive']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      withAll(exactly(0, query = drugExposure(conceptSet[['cs_others']]), duringInterval(eventStarts(0, 180, index = 'startDate')))),
      withAll(exactly(0, query = conditionOccurrence(conceptSet[['cs_previousCVevent']]), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      expressionLimit = 'All'),
    exit = exit(endStrategy = drugExit(conceptSet[['cs_second']], persistenceWindow = 30, surveillanceWindow = 30),
                if(OT2){censor = censoringEvents(drugExposure(conceptSet[['cs_others']]))}
    ),
    era = era(eraDays = 0L)
  )
  
  cohortJson = cd %>% toCirce() %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  cohortJsonList = jsonlite::fromJSON(cohortJson)
  ## Adding race/ethnicity manually
  oldExp = cohortJsonList$InclusionRules$expression
  newExp = data.frame(Type = c(oldExp$Type, 'ALL'))
  newExp$CriteriaList = as.list(c(oldExp$CriteriaList, list(list())))
  newExp$DemographicCriteriaList = as.list(c(oldExp$DemographicCriteriaList, 
                                             list(list())))
  newExp$DemographicCriteriaList[[6]] = data.frame(Ethnicity = NA)
  newExp$DemographicCriteriaList[[6]]$Ethnicity =list(data.frame(CONCEPT_CODE = c('Hispanic'), CONCEPT_ID = c(38003563),
                                                                 CONCEPT_NAME = c('Hispanic or Latino'),
                                                                 DOMAIN_ID = c('Ethnicity'),
                                                                 INVALID_REASON_CAPTION = c('Unknown'),
                                                                 STANDARD_CONCEPT_CAPTION = c('Unknown'),
                                                                 VOCABULARY_ID = c('Ethnicity')))
  newExp$Groups = as.list(c(oldExp$Groups, list(list())))
  
  cohortJsonList$InclusionRules = data.frame(name = c(cohortJsonList$InclusionRules$name, 'rule6'))
  cohortJsonList$InclusionRules$expression = newExp
  rm(oldExp, newExp)
  
  cohortJson = cohortJsonList %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  cohortJson = limitDistinctConcept(cohortJson)
  sql = addSecondDrugExitSql(cohortJson)
  result = data.frame(json = cohortJson, sql = sql)
  return(result)
}


## Main setting (On-treatment 1): no additional censoring
cohortDefinitionMain = createCohortDefinition(connection, cdm_database_schema, idConvention = 1000000, cohortNameTag = '', createFunction = createCohortDefinitionMain, OT = F)
cohortDefinitionAgeSubgroup1 = createCohortDefinition(connection, cdm_database_schema, idConvention = 1100000, cohortNameTag = 'young age group', createFunction = createCohortDefinitionAgeSubgroup1, OT = F)
cohortDefinitionAgeSubgroup2 = createCohortDefinition(connection, cdm_database_schema, idConvention = 1200000, cohortNameTag = 'middle age group', createFunction = createCohortDefinitionAgeSubgroup2, OT = F)
cohortDefinitionAgeSubgroup3 = createCohortDefinition(connection, cdm_database_schema, idConvention = 1300000, cohortNameTag = 'old age group', createFunction = createCohortDefinitionAgeSubgroup3, OT = F)
cohortDefinitionMaleSubgroup = createCohortDefinition(connection, cdm_database_schema, idConvention = 1400000, cohortNameTag = 'male group', createFunction = createCohortDefinitionMaleSubgroup, OT = F)
cohortDefinitionFemaleSubgroup = createCohortDefinition(connection, cdm_database_schema, idConvention = 1500000, cohortNameTag = 'female group', createFunction = createCohortDefinitionFemaleSubgroup, OT = F)
cohortDefinitionWhiteSubgroup = createCohortDefinition(connection, cdm_database_schema, idConvention = 1600000, cohortNameTag = 'white group', createFunction = createCohortDefinitionWhiteSubgroup, OT = F)
cohortDefinitionBlackSubgroup = createCohortDefinition(connection, cdm_database_schema, idConvention = 1700000, cohortNameTag = 'black group', createFunction = createCohortDefinitionBlackSubgroup, OT = F)
cohortDefinitionHispanicSubgroup = createCohortDefinition(connection, cdm_database_schema, idConvention = 1800000, cohortNameTag = 'hispanic group', createFunction = createCohortDefinitionHispanicSubgroup, OT = F)

cohortDefinitionFinal = rbind(cohortDefinitionMain,
                              cohortDefinitionAgeSubgroup1,
                              cohortDefinitionAgeSubgroup2,
                              cohortDefinitionAgeSubgroup3,
                              cohortDefinitionMaleSubgroup,
                              cohortDefinitionFemaleSubgroup,
                              cohortDefinitionWhiteSubgroup,
                              cohortDefinitionBlackSubgroup,
                              cohortDefinitionHispanicSubgroup)

rm(cohortDefinitionMain,
   cohortDefinitionAgeSubgroup1,
   cohortDefinitionAgeSubgroup2,
   cohortDefinitionAgeSubgroup3,
   cohortDefinitionMaleSubgroup,
   cohortDefinitionFemaleSubgroup,
   cohortDefinitionWhiteSubgroup,
   cohortDefinitionBlackSubgroup,
   cohortDefinitionHispanicSubgroup)

## Save the result
write.csv(cohortDefinitionFinal, file.path(workDir, 'inst/cohorts/cohortDefinition.csv'), row.names = F)


## On-treatment 2: additional censoring when other antihypertensive added during follow-up
cohortDefinitionMain = createCohortDefinition(connection, cdm_database_schema, idConvention = 2000000, cohortNameTag = 'OT2', createFunction = createCohortDefinitionMain, OT = T)
cohortDefinitionAgeSubgroup1 = createCohortDefinition(connection, cdm_database_schema, idConvention = 2100000, cohortNameTag = 'OT2 young age group', createFunction = createCohortDefinitionAgeSubgroup1, OT = T)
cohortDefinitionAgeSubgroup2 = createCohortDefinition(connection, cdm_database_schema, idConvention = 2200000, cohortNameTag = 'OT2 middle age group', createFunction = createCohortDefinitionAgeSubgroup2, OT = T)
cohortDefinitionAgeSubgroup3 = createCohortDefinition(connection, cdm_database_schema, idConvention = 2300000, cohortNameTag = 'OT2 old age group', createFunction = createCohortDefinitionAgeSubgroup3, OT = T)
cohortDefinitionMaleSubgroup = createCohortDefinition(connection, cdm_database_schema, idConvention = 2400000, cohortNameTag = 'OT2 male group', createFunction = createCohortDefinitionMaleSubgroup, OT = T)
cohortDefinitionFemaleSubgroup = createCohortDefinition(connection, cdm_database_schema, idConvention = 2500000, cohortNameTag = 'OT2 female group', createFunction = createCohortDefinitionFemaleSubgroup, OT = T)
cohortDefinitionWhiteSubgroup = createCohortDefinition(connection, cdm_database_schema, idConvention = 2600000, cohortNameTag = 'OT2 white group', createFunction = createCohortDefinitionWhiteSubgroup, OT = T)
cohortDefinitionBlackSubgroup = createCohortDefinition(connection, cdm_database_schema, idConvention = 2700000, cohortNameTag = 'OT2 black group', createFunction = createCohortDefinitionBlackSubgroup, OT = T)
cohortDefinitionHispanicSubgroup = createCohortDefinition(connection, cdm_database_schema, idConvention = 2800000, cohortNameTag = 'OT2 hispanic group', createFunction = createCohortDefinitionHispanicSubgroup, OT = T)

cohortDefinitionFinal = rbind(cohortDefinitionMain,
                              cohortDefinitionAgeSubgroup1,
                              cohortDefinitionAgeSubgroup2,
                              cohortDefinitionAgeSubgroup3,
                              cohortDefinitionMaleSubgroup,
                              cohortDefinitionFemaleSubgroup,
                              cohortDefinitionWhiteSubgroup,
                              cohortDefinitionBlackSubgroup,
                              cohortDefinitionHispanicSubgroup)

rm(cohortDefinitionMain,
   cohortDefinitionAgeSubgroup1,
   cohortDefinitionAgeSubgroup2,
   cohortDefinitionAgeSubgroup3,
   cohortDefinitionMaleSubgroup,
   cohortDefinitionFemaleSubgroup,
   cohortDefinitionWhiteSubgroup,
   cohortDefinitionBlackSubgroup,
   cohortDefinitionHispanicSubgroup)

## Save the result
write.csv(cohortDefinitionFinal, file.path(workDir, 'inst/cohorts/cohortDefinitionOT2.csv'), row.names = F)
