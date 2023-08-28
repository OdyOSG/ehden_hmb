# A. Meta Info -----------------------

# Task: Treatment Patterns
# Author: Carina
# Date: 2023-07-26
# Description: The purpose of the _treatmentPatterns.R script is to.....

# B. Functions ------------------------

## Treatment Patterns -------------------------

prepSankey <- function(th, minNumPatterns) {

  treatment_pathways <- th %>%
    tidyr::pivot_wider(id_cols = person_id,
                       names_from = event_seq,
                       names_prefix = "event_cohort_name",
                       values_from = event_cohort_name) %>%
    dplyr::count(dplyr::across(tidyselect::starts_with("event_cohort_name"))) %>%
    dplyr::mutate(End = "end", .before = "n") %>%
    dplyr::filter(n >= minNumPatterns)


  links <- treatment_pathways %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = c(-row, -n),
                        names_to = 'column', values_to = 'source') %>%
    dplyr::mutate(column = match(column, names(treatment_pathways))) %>%
    tidyr::drop_na(source) %>%
    dplyr::mutate(source = paste0(source, '__', column)) %>%
    dplyr::group_by(row) %>%
    dplyr::mutate(target = dplyr::lead(source, order_by = column)) %>%
    tidyr::drop_na(target, source) %>%
    dplyr::group_by(source, target) %>%
    dplyr::summarise(value = sum(n), .groups = 'drop') %>%
    dplyr::arrange(desc(value))

  nodes <- data.frame(name = unique(c(links$source, links$target)))
  nodes <- data.table::data.table(nodes)
  links <- data.table::data.table(links)
  links$source <- match(links$source, nodes$name) - 1
  links$target <- match(links$target, nodes$name) - 1
  nodes$name <- sub('__[0-9]+$', '', nodes$name)
  links$type <- sub(' .*', '',
                    as.data.frame(nodes)[links$source + 1, 'name'])
  data.table::setkey(links, type)
  data.table::setorder(links, cols = - "value")

  res <- list(
    'treatmentPatterns' = treatment_pathways,
    'links' = links,
    'nodes' = nodes
  )

  return(res)

}

# getPersonIds <- function(con,
#                          workDatabaseSchema,
#                          cohortTable,
#                          cohortIds) {
#
#   sql <- "
#     SELECT cohort_definition_id, subject_id
#     FROM @workDatabaseSchema.@cohortTable
#     WHERE cohort_definition_id IN (@cohortIds)
#   " %>%
#     SqlRender::render(
#       workDatabaseSchema = workDatabaseSchema,
#       cohortTable = cohortTable,
#       cohortIds = cohortIds
#   ) %>%
#     SqlRender::translate(targetDialect = con@dbms)
#
#   cohortTbl <-  DatabaseConnector::querySql(connection = con, sql = sql)
#   colnames(cohortTbl) <- tolower(colnames(cohortTbl))
#
#   ll <- cohortTbl %>%
#     dplyr::arrange(cohort_definition_id) %>%
#     tidyr::nest(.by = cohort_definition_id) %>%
#     dplyr::mutate(
#       idx = as.integer(substr(cohort_definition_id, 1, 1))
#     ) %>%
#     dplyr::select(cohort_definition_id, idx, data)
#
#   return(ll)
#
# }


executeTreatmentPatterns <- function(con,
                                 executionSettings,
                                 analysisSettings) {

  ## Prep
  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  # make output folder
  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder)
  thHistoryFolder <- outputFolder[[2]]
  txPatFolder <- outputFolder[[3]]

  targetCohortId <- analysisSettings$treatmentPatterns$cohorts$targetCohort$id
  targetCohortName <- analysisSettings$treatmentPatterns$cohorts$targetCohort$name

  # list all treatment history files
  thFiles <- fs::dir_ls(thHistoryFolder, recurse = TRUE, type = "file")

  #read parquet file - get treatment history
  th <- arrow::read_parquet(file = thFiles)

  #get the target cohort name
  file_label <- tools::file_path_sans_ext(basename(thFiles)) %>%
    gsub("th_", "", .)


  #do consoule print
  cli::cat_line()
  cli::cat_bullet(crayon::magenta("Build Sankey data for Id: "),
                  targetCohortId, " (name: ",  targetCohortName, ")",
                  bullet = "pointer", bullet_col = "yellow")


  patterns <- th %>%
    prepSankey(minNumPatterns = 30L)

  # extract folder names for class and era
  save_path <- fs::path(txPatFolder) %>%
    fs::dir_create()

  #save file
  file_name <- paste("sankey", targetCohortId, sep = "_")
  save_path2 <- fs::path(save_path, file_name, ext = "rds")
  readr::write_rds(patterns, file = save_path2)
  cli::cat_line()
  cli::cat_bullet("Saved file ", crayon::green(basename(save_path2)), " to:",
                  bullet = "info", bullet_col = "blue")
  cli::cat_bullet(crayon::cyan(save_path), bullet = "pointer", bullet_col = "yellow")
  cli::cat_line()

  invisible(patterns)
}
#
# ## Time to Event -----------------------


prepTte <- function(con,
                    th,
                    workDatabaseSchema,
                    cohortTable,
                    targetCohorts) {

  cli::cat_line(crayon::blue("Extracting Survival Table..."))

  targetCohortIds <- targetCohorts$id
  targetCohortNames <- targetCohorts$name
  # get target cohort table
  sql <- "SELECT * FROM @write_schema.@cohort_table
          WHERE cohort_definition_id IN (@target_cohort_id);"  %>%
    SqlRender::render(
      write_schema = workDatabaseSchema,
      cohort_table = cohortTable,
      target_cohort_id = targetCohortIds
    ) %>%
    SqlRender::translate(con@dbms)

  targetTbl <- DatabaseConnector::querySql(connection = con, sql = sql)

  colnames(targetTbl) <- tolower(colnames(targetTbl))

  #prep treatment history table to tibble
  dt <- th %>%
    tibble::as_tibble()

  #loop on targetCohort Ids
  survDat <- vector('list', length(targetCohortIds))
  for (i in seq_along(targetCohortIds)) {

    tte <- targetTbl %>%
      # filter to cohort id
      dplyr::filter(cohort_definition_id == targetCohortIds[i]) %>%
      # join th and target table to determine censoring
      dplyr::inner_join(th, by = c("subject_id" = "person_id"), relationship = "many-to-many") %>%
      #identifying the event and convert time to years
      dplyr::mutate(
        event = dplyr::case_when(
          event_end_date < cohort_end_date ~ 1,
          TRUE ~ 0
        ),
        time_years = duration_era / 365.25
      ) %>%
      dplyr::select(event_cohort_id, time_years, event)


    # determine the number of persons in a combo
    keep_ids <- tte %>%
      count(event_cohort_id) %>%
      dplyr::mutate(
        combo = ifelse(grepl("\\+", event_cohort_id), 1, 0)
      ) %>%
      dplyr::filter(
        combo == 0 | (combo == 1 & n >= 5)
      ) %>%
      dplyr::pull(event_cohort_id)


    # create the surv fit object
    survFit <- ggsurvfit::survfit2(
      survival::Surv(time_years, event) ~ event_cohort_id, data = tte %>% filter(event_cohort_id %in% keep_ids)
    )
    # place the surv fit table into the list
    survDat[[i]] <- ggsurvfit::tidy_survfit(survFit) %>%
      dplyr::select(time, `n.risk`, `n.event`, estimate:strata) %>%
      dplyr::mutate(
        targetCohortId = targetCohortIds[i],
        targetCohortName = targetCohortNames[i]
      )
  }
  #bind data at the end
  survDat <- do.call('rbind', survDat)
  return(survDat)

}

executeTimeToEvent <- function(con,
                           executionSettings,
                           analysisSettings) {


  ## Prep
  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  # prep output folder
  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder)
  thHistoryFolder <- outputFolder[2] # hard coded to get th folder
  tteFolder <- outputFolder[4] # hard coded to get tte folder

  targetCohorts <- analysisSettings$treatmentPatterns$cohorts$targetCohort
  targetCohortId <- targetCohorts$id
  targetCohortName <- targetCohorts$name

  # list all treatment history files
  thFiles <- fs::dir_ls(thHistoryFolder, recurse = TRUE, type = "file")

  #read parquet file - get treatment history
  th <- arrow::read_parquet(file = thFiles)

  #get the target cohort name
  file_label <- tools::file_path_sans_ext(basename(thFiles)) %>%
    gsub("th_", "", .)


  #do consoule print
  cli::cat_line()
  cli::cat_bullet(crayon::magenta("Execute time to discontinuation for Id: "),
                  targetCohortId, " (name: ",  targetCohortName, ")",
                  bullet = "pointer", bullet_col = "yellow")


  # get time to event dataframe
  tteDat <- prepTte(con = con,
                    th = th,
                    workDatabaseSchema = workDatabaseSchema,
                    cohortTable = cohortTable,
                    targetCohorts = targetCohorts)


  # create tte folder
  save_path <- fs::path(tteFolder) %>%
    fs::dir_create()

  #save time to event data
  file_name <- paste(file_label, "tte", sep = "_")
  save_path2 <- fs::path(save_path, file_name, ext = "csv")
  readr::write_csv(tteDat, file = save_path2)
  cli::cat_line()
  cli::cat_bullet("Saved file ", crayon::green(basename(save_path2)), " to:",
                  bullet = "info", bullet_col = "blue")
  cli::cat_bullet(crayon::cyan(save_path), bullet = "pointer", bullet_col = "yellow")
  cli::cat_line()


  invisible(tteDat)

}

