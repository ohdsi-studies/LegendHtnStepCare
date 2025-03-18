## Create exposure cohorts -- Dual combination (stepped care only) of antihypertensive drugs


# install.packages("remotes")
# remotes::install_github("OHDSI/Hades", upgrade = "always")

library(remotes)
library(dplyr)
library(Capr)
library(CirceR)
library(testthat)

workDir = file.path(getwd())

## Create a function to generate cohort definition

createCohortDefinition <- function(cs1, cs2){
  
  cs_antihypertensive <- cs(descendants(904542,907013,932745,942350,956874,970250,974166,978555,991382,1305447,1307046,1307863,
                                        1308216,1308842,1309068,1309799,1310756,1313200,1314002,1314577,1317640,1317967,1318137,
                                        1318853,1319880,1319998,1322081,1326012,1327978,1328165,1331235,1332418,1334456,1335471,
                                        1338005,1340128,1341238,1341927,1342439,1344965,1345858,1346686,1346823,1347384,1350489,
                                        1351557,1353766,1353776,1363053,1363749,1367500,1373225,1373928,1386957,1395058,1398937,40226742,40235485),
                            name = 'Antihypertensives')
  
  cs_first <- cs(descendants(cs1),
                 ### Exclude single pill combination -- based on RxNorm Classification
                 descendants(exclude(
                   36236948,36236949,36234201,36234200,36234454,36234455,36235144,36235145,36237459,36237460,36228123,36228124,36237783,
                   36237784,36230422,36230423,36237852,36237853,36237869,36237870,36237230,36237231,36228734,36228735,36228737,36228738,
                   36228739,36228740,36232387,36232388,36218517,36218518,36233813,36233814,36234627,36234628,36231600,36231601,36231237,
                   36231238,36232591,36232592,36239382,36239383,36239801,36239802,36231925,36231926,36231927,36231928,36238820,36238819,
                   36248705,36248706,36228868,36228869,36240204,36240205,36232208,36232209,36232516,36232517,36232520,36232521,36229025,
                   36229026,36229027,36229028,36229223,36229222,36230655,36230656,36230668,36230669,36247927,36247928,36229633,36243282,
                   36243283,36243284,36241277,36241278,36230802,36232453,36232483,36232484,36227929,36227930,36229579,36231207,36231208,
                   36239102,36238510,36242839,36242838,36243159,36243160,36241084,36241085,36241087,36241088,36239296,36239297,36239983,
                   36239984,36239006,36239007,36239639,36239638,36239650,36239651,36216899,36216900,36216901,36216902,36216903,36216904,
                   36219590,36221767,36221768,36221769,36221770,36222006,36222007,36222008,36215127,36215128,36216161,36216162,36216163,
                   36216164,36216165,36216166,36216167,36216168,36216169,36216170,36216171,36216172,36216173,36216174,36216175,36216176,
                   36216246,36216247,36216248,36216249,36216250,36216251,36216252,36217712,36217713,36217714,36217715,36217708,36217709,
                   36217710,36217711,36217716,36217717,36248703,36248704,36217718,36217719,36217720,36217721,36214477,36214478,36214479,
                   36214480,36214481,36214482,36214483,36214484,36238273,36238274,36225635,36225636,36213274,36213275,36213278,36213279,
                   36217664,36217665,36217670,36217671,36223654,36223655,36212160,36215809,36215810,36217844,36217845,36217842,36217843,
                   36219628,36222040,36215393,36215394,36212261,36212262,36212400,36212401,36212293,36212294,36212295,36212296,36212297,
                   36212298,36212299,36212300,36212301,36212302,36212303,36212304,36216551,36216552,36217139,36217140,36217137,36217138,
                   36212440,36212441,36215416,36215417,36215209,36215210,36215223,36215224,36216030,36216031,36216034,36216035,36216725,
                   36216726,36216727,36216728,36224573,36224574,36224575,36224576,36225790,36225791,36226052,36226053,36226137,36226138,
                   36226139,36226140,36224600,36224601,36225434,36225435,36225438,36225439,36225436,36225437,36225440,36225441,36225442,
                   36225443,36224449,36224450,36227481,36227482,36227483,36227484,36227485,36227486,36227487,36227488,36227489,36227490,
                   36225136,36225137,36224289,36224290,36224291,36224292,36224293,36224294,36224295,36224296,36225138,36218311,36224297,
                   36224298,36224299,36224300,36218312,36218313,36218314,36218315,36224301,36224302,36224303,36224304,36224305,36224306,
                   36218316,36218317,36224307,36224308,36224309,36224310,36224311,36224312,36224313,36224314,36218318,36218319,36224315,
                   36224316,36224317,36224318,36224319,36224320,36218320,36218321,36225134,36225135,36218404,36218405,36220835,36220836,
                   36214315,36214316,36224719,36224720,36226979,36226980,36226981,36226982,36218624,36218625,36221077,36221078,36221081,
                   36221082,36250179,36250180,36224690,36224691,36221405,36221406,36226883,36226884,36220155,36220156,36227399,36227400,
                   36219017,36219018,36248677,36248678,36225488,36225489,36223174,36223175,1510704,1510705,
                   2064627,21035025,21052206,21071954,21071955,21130571,21132694,21152528,21152932,35146103,35150194,35406763,35408893,
                   36259265,36264366,36264394,36266935,36269479,36272027,36272030,36274581,36274595,36784817,36786066,36787123,36787465,
                   36881813,36889540,40003426,40007368,40007369,40007373,40007374,40007377,40007378,40007379,40007380,40010558,40010559,
                   40010560,40010561,40010567,40010568,40010569,40011854,40011855,40011856,40011860,40015092,40020436,40026132,40027598,
                   40028109,40028110,40030840,40030842,40030843,40030844,40030845,40033922,40034892,40037280,40037281,40039294,40039680,
                   40039681,40039682,40039683,40044449,40044821,40044824,40044831,40044832,40044834,40044837,40044838,40044839,40044851,
                   40044854,40044856,40044857,40045116,40045118,40045119,40045121,40045124,40045126,40045128,40049453,40054917,40054918,
                   40059139,40060227,40061782,40065740,40085584,40095410,40096153,40096806,40099414,40099416,40137849,40140541,40142669,
                   40147525,40160338,40167838,40224153,40232328,40749849,40830512,40830513,40893011,40924218,40924457,40955187,40955683,
                   40986248,40986369,40986688,41017227,41048702,41080575,41111670,41143011,41143341,41174430,41174481,41205367,41205620,
                   41236870,41237790,41238954,41267737,41298388,41298962,41299800,42479214,42705336,42874880,42875928,42938519,42948657,
                   42950872,42960011,42960024,42972643,43022805,43134832,43156974,43157075,43166576,43167787,43178952,43188587,43189757,
                   43189944,43193525,43199873,43199916,43203499,43205563,43211738,43260913,43585468,43599922,43636119,43672256,43677126,
                   43725912,43748731,43749671,43839612,44031088,44031476,44057083,44069972,44082841,44095788,45892980
                 )),
                 name = 'First agent')
  
  cs_second <- cs(descendants(cs2),
                  name = 'Second agent')
  
  cs_others <- cs(
    descendants(setdiff(c(904542,907013,932745,942350,956874,970250,974166,978555,991382,1305447,1307046,1307863,1308216,1308842,
                          1309068,1309799,1310756,1313200,1314002,1314577,1317640,1317967,1318137,1318853,1319880,1319998,1322081,
                          1326012,1327978,1328165,1331235,1332418,1334456,1335471,1338005,1340128,1341238,1341927,1342439,1344965,
                          1345858,1346686,1346823,1347384,1350489,1351557,1353766,1353776,1363053,1363749,1367500,1373225,1373928,
                          1386957,1395058,1398937,40226742,40235485), c(cs1,cs2))),
    descendants(exclude(cs1, cs2)),
    name = 'Other agents')
  
  cs_hypertension <- cs(descendants(320128),
                        descendants(exclude(4215640, 4034031, 4083723, 4302591)),
                        name = 'Hypertension')
  
  cs_previousCVevent <- cs(c(372924, 375557, 376713, 441874, 439847, 432923),
                           descendants(316139, 443454, 4329847),
                           descendants(exclude(315295, 314666)),
                           name = 'Previous CV events including acute MI, stroke, HF')
  
  cd <- cohort(
    entry = entry(drugExposure(cs_second, age(gte(18)), firstOccurrence()),
                  observationWindow = continuousObservation(priorDays = 365),
                  qualifiedLimit = 'First'),
    attrition = attrition(
      withAll(atLeast(1, query = drugExposure(cs_first), duringInterval(eventStarts(-Inf, -30, index = 'startDate')))),
      withAll(atLeast(1, query = conditionOccurrence(cs_hypertension), duringInterval(eventStarts(-Inf, 0, index = 'startDate')))),
      withAll(exactly(1, query = drugExposure(cs_antihypertensive), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      withAll(exactly(0, query = drugExposure(cs_others), duringInterval(eventStarts(0, 180, index = 'startDate')))),
      withAll(exactly(0, query = conditionOccurrence(cs_previousCVevent), duringInterval(eventStarts(-Inf, -1, index = 'startDate')))),
      expressionLimit = 'All'),
    exit = exit(endStrategy = drugExit(cs_second, persistenceWindow = 30, surveillanceWindow = 30)),
    era = era(eraDays = 0L)
  )
  
  cohortJson = cd %>% toCirce() %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()
  cohortJsonList = jsonlite::fromJSON(cohortJson)
  
  ## Adding additional features (not supported by CapR) to Json
  ### Below depends the order of criteria. 
  cohortJsonList$InclusionRules$expression$CriteriaList[[3]]$Occurrence = cohortJsonList$InclusionRules$expression$CriteriaList[[3]]$Occurrence %>% 
    mutate(IsDistinct = TRUE, CountColumm = 'DOMAIN_CONCEPT')
  
  cohortJson = jsonlite::toJSON(cohortJsonList, pretty = TRUE, auto_unbox = TRUE) %>% as.character()
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

  result = data.frame(json = cohortJson, sql = sql)
  return(result)
}

## Define concepts for each antihypertensive drug class
conceptDefinition = list(c(1335471,1340128,1341927,1363749,1308216,1310756,1373225,1331235,1334456,1342439),
                         c(1351557,1346686,1347384,1367500,40226742,1317640,1308842,40235485),
                         c(1318137,1318853,1319880,1326012,1332418,1353776),
                         c(1307863,1328165),
                         c(1395058,974166,978555,907013),
                         c(1307046,1314002,1314577,1319998,1322081,1327978,1338005,
                           1345858,1346823,1353766,1386957, 1313200))
names(conceptDefinition) = c('ACEI','ARB','dCCB','ndCCB','Thiazide','BB')


## Create multiple cohorts using the function and concept sets

### cohort id convention
primary = c('ACEI', 'ARB', 'ndCCB', 'dCCB', 'Thiazide')
secondary = c('ACEI', 'ARB', 'BB', 'ndCCB', 'dCCB', 'Thiazide')
cohortDefinition = primary %>% merge(secondary, all = T) %>% filter(x != y) %>% mutate(combi = paste(y, 'after', x))
cohortDefinition = cohortDefinition %>% filter(combi != 'ACEI after ARB' & combi != 'ARB after ACEI') %>% arrange(combi)
cohortDefinition$cohort_id = seq(1:nrow(cohortDefinition)) + 1000000
cohortDefinition$json = NA
cohortDefinition$sql = NA
cohortDefinition = cohortDefinition %>% select(cohort_id, x, y, combi, json, sql)
colnames(cohortDefinition) = c('cohortId', 'First', 'Second', 'Combi', 'json', 'sql')

for(i in 1:nrow(cohortDefinition)){
  result = createCohortDefinition(cs1 = conceptDefinition[which(names(conceptDefinition) == cohortDefinition$First[i])][[1]],
                                  cs2 = conceptDefinition[which(names(conceptDefinition) == cohortDefinition$Second[i])][[1]])
  cohortDefinition$json[i] = result$json
  cohortDefinition$sql[i] = result$sql
  rm(result)
}

cohortDefinition = cohortDefinition %>% select(-First, -Second)
colnames(cohortDefinition) = c('cohortId', 'cohortName', 'json', 'sql')

## Save the result

write.csv(cohortDefinition, file.path(workDir, '/inst/cohortDefinition.csv'), row.names = F)
