
# function to bind csvs from executions
bindCsv <- function(allPaths, task, file) {

  dat <- allPaths %>%
    dplyr::filter(listOfTasks == !!task) %>%
    dplyr::mutate(
      filePath = fs::path(fullPath, !!file)
    ) %>%
    dplyr::select(filePath, listOfDatabase) %>%
    purrr::pmap_dfr(~readr::read_csv(..1, show_col_types = FALSE) %>%
                      dplyr::mutate(
                        databaseId = ..2)) %>%
    dplyr::relocate(databaseId, .before = 1)

  return(dat)
}

# function to bind csv within a subfolder
bindFolder <- function(path, folder) {

  dat <- fs::path(path, folder) %>%
    fs::dir_ls(type = "file") %>%
    purrr::map_dfr(~readr::read_csv(.x, show_col_types = FALSE))

  return(dat)
}

# Function to mask low counts. Default count is 5
maskLowCount <- function(df, countLimit = 5L) {

  dfLow <- df %>%
    dplyr::mutate(
      pct = dplyr::if_else(count <= countLimit, "-", scales::percent(pct, accuracy = 0.01), "-"),
      count = dplyr::if_else(count <= countLimit, "<5", format(count, big.mark = ",", scientific = FALSE), "-")
    )

  return(dfLow)
}

# Function to mask low counts. Default count is 5
maskLowCountInci <- function(df, countLimit = 5L) {

  dfLow <- df %>%
    dplyr::mutate(
      INCIDENCE_PROPORTION_P100P = dplyr::if_else(OUTCOMES <= countLimit, "-", format(round(INCIDENCE_PROPORTION_P100P, 2) , big.mark = ",", scientific = FALSE), "-"),
      INCIDENCE_RATE_P1000PY = dplyr::if_else(OUTCOMES <= countLimit, "-", format(round(INCIDENCE_RATE_P1000PY, 2), big.mark = ",", scientific = FALSE), "-"),
      OUTCOMES = dplyr::if_else(OUTCOMES <= countLimit, "<5", format(OUTCOMES, big.mark = ",", scientific = FALSE), "-")
    )

  return(dfLow)
}



# Treatment Patterns Functions -----------

# bind txPath rds
bindTxPathTab <- function(path, database) {

  rdsFiles <- path %>%
    fs::dir_ls(type = "file")

  # use when corrected
  # cohortId <- tools::file_path_sans_ext(basename(rdsFiles)) %>%
  #   gsub("sankey_", "", .) %>%
  #   as.integer()

  cohortName <- tools::file_path_sans_ext(basename(rdsFiles)) %>%
    gsub("_sankey", "", .)

  dat <- purrr::map2_dfr(rdsFiles,
                         cohortName,
                         ~readr::read_rds(.x) %>%
                           getElement("treatmentPatterns") %>%
                           dplyr::mutate(databaseId = !!database,
                                         cohortName = !!.y))

  return(dat)
}


# function to plot treatment patterns
plot_patterns <- function(sankey) {

  links <- sankey$links
  nodes <- sankey$nodes

  label <- unique(links$type)
  label2 <- paste0("'", paste(label, collapse = "','"), "',", "'end'")

  martin_colors <- unname(colorBlindness::paletteMartin)[-1]

  col <- martin_colors[seq_along(label)]
  col2 <- paste0("'", paste(col, collapse = "','"), "',", "'#1B1919FF'")

  myCol <- glue::glue('d3.scaleOrdinal() .domain([{label2}]) .range([{col2}])')

  # Plot sankeyNetwork
  sankey <- networkD3::sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = 'source',
    Target = 'target',
    Value = 'value',
    NodeID = 'name',
    fontSize = 11,
    sinksRight = FALSE,
    colourScale = myCol
  )

  return(sankey)
}

## Group sankey data together to save
groupSankey <- function(path, database) {

  # get rds files
  rdsFiles <- path %>%
    fs::dir_ls(type = "file")

  # retrieve dat to make sankey
  dat <- purrr::map(rdsFiles, ~readr::read_rds(.x)[c(2,3)])

  #find which links df that have data
  jj <- purrr::map_lgl(dat, ~nrow(.x$links) > 0)

  #subset list to keep only those with data
  sankeyDat <- dat[jj]

  sankey <- purrr::map(sankeyDat, ~plot_patterns(.x))

  # rename to the cohort id
  names(sankey) <- tools::file_path_sans_ext(basename(names(sankey))) %>%
    gsub("sankey_", "", .)

  return(sankey)
}


# Time to event functions -----

bindTteData <- function(path,
                        database,
                        task,
                        file,
                        nYears = 3) {


  # create path to file
  pathToFile <- fs::path(path, database, task, file)

  #create target id
  targetId <- gsub(".*_", "", file) %>%
    tools::file_path_sans_ext()

  #read in data
  tteData <- readr::read_csv(file = pathToFile, show_col_types = FALSE)

  # %>%
  #   dplyr::filter(
  #     time <= 3
  #   )
  #
  # #remove singleLine Strata
  # singleLineStrata <- tteData %>%
  #   dplyr::filter(
  #     !grepl("\\+", strata)
  #   ) %>%
  #   dplyr::pull(strata) %>%
  #   unique()
  #
  # #get top 4 multi lines
  # top4MultiLineStrata <-tteData %>%
  #   dplyr::filter(
  #     grepl("\\+", strata)
  #   ) %>%
  #   count(strata) %>%
  #   dplyr::arrange(desc(n)) %>%
  #   dplyr::slice(1:4) %>%
  #   dplyr::pull(strata) %>%
  #   unique()
  #
  # # combine specified strata lines
  # strataLines <- c(singleLineStrata, top4MultiLineStrata)


  # subset tted Data to the specified strata lines
  subsetTteData <- tteData %>%
    dplyr::mutate(strata = as.character(strata)) %>%
    # dplyr::filter(
    #   strata %in% strataLines
    # ) %>%
    dplyr::mutate(
      database = !!database,
      targetId = !!targetId
    ) %>%
    dplyr::select(
      database, targetId, strata, time, n.risk, n.event, estimate, std.error, line
    )

  return(subsetTteData)
}


bindTteData2 <- function(path,
                         database,
                         task,
                         file,
                         nYears = 3) {

  # create path to file
  pathToFile <- fs::path(path, database, task, file)

  #create target id
  targetId <- gsub(".*_", "", file) %>%
    tools::file_path_sans_ext()

  #read in data
  tteData <- readr::read_csv(file = pathToFile, show_col_types = FALSE)
  # %>%
  #   dplyr::filter(
  #     time <= 3
  #   )

  # subset tted Data to the specified strata lines
  updateTteData <- tteData %>%
    dplyr::mutate(
      database = !!database,
      targetId = !!targetId
    ) %>%
    dplyr::select(
      database, targetId, outcomeCohortId, time, n.risk, n.event, estimate, std.error
    )

  return(updateTteData)
}


# create KM plot
plotKM <- function(kmData, # input data
                   targetCohortId, # select target cohort
                   database, # select database
                   nLines = 15L, # determine the number of lines to plot default is 8
                   saveLocation = here::here("appData/www") # location to save plot
) {

  # create saveLocation if doesnt exist
  saveLocation <- fs::dir_create(saveLocation)

  #console prints
  cli::cat_line(crayon::green("Create KM Plot"))
  cli::cat_bullet(glue::glue("Database: {crayon::yellow(database)}"),
                  bullet = "pointer", bullet_col = "yellow")
  cli::cat_bullet(glue::glue("Cohort Id: {crayon::yellow(targetCohortId)}"),
                  bullet = "pointer", bullet_col = "yellow")

  # subset data based on targetId, database and era
  dat <- kmData %>%
    dplyr::filter(
      databaseId == database,
      targetCohortId == !!targetCohortId
    ) %>%
    dplyr::mutate(
      strata = factor(strata)
    )

  # determine the top 8 treatment lines and subset dat to only show these
  keep <- dat %>%
    count(strata) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice(1:nLines) %>%
    dplyr::pull(strata)

  # filter data to only keep top 8 treatment lines
  dat2 <- dat %>%
    dplyr::filter(
      strata %in% keep
    )

  # prep plot
  cols <- unname(grafify::graf_palettes$kelly) # get plotting colors
  plot_colors <- cols[1:nLines] # retrieve first nLines

  # get km plot
  p <- ggplot(dat2, aes(x = time, y = estimate, color = strata)) +
    geom_step(linewidth = 1) +
    scale_color_manual(values = plot_colors) + #scale colors to kelly
    scale_y_continuous(labels = scales::percent_format()) + # convert y axis to percent
    labs(x = 'Years', y = 'Probability of Survival') + # improve labels
    theme_classic()

  #create saving convention
  plotName <- glue::glue("km_{database}_{targetCohortId}.png") %>%
    as.character()

  plotSave <- fs::path(saveLocation, plotName)

  # save plot
  ggplot2::ggsave(filename = plotSave,
                  plot = p,
                  width = 27, height = 24, units = "cm")

  #console print of save location
  cli::cat_line("Saved plot to: ")
  cli::cat_bullet(crayon::cyan(plotSave),
                  bullet = "pointer", bullet_col = "yellow")

  invisible(p)
}


## Run the plotKm in for loop or purrr walk to get all the plots in the www folder

# Get Probability Tables ------------------------

findSurvProbAtTime <- function(dat, strata, t) {

  label <- paste("yr", t,  sep = "_")

  survProb <- dat %>%
    dplyr::filter(strata == !!strata) %>% #filter on strata
    dplyr::mutate(
      tt = abs(time - !!t) # find absolute value of time column with t
    ) %>% # this will find value closest to zero
    dplyr::arrange(tt) %>% #sort from smallest to largest
    dplyr::slice(1) %>%
    dplyr::select(
      strata, estimate, conf.low, conf.high
    ) %>%
    dplyr::mutate(
      t = label, .before = 1
    )

  return(survProb)
}


findSurvProbAtTime2 <- function(dat, t) {

  label <- paste("yr", t,  sep = "_")

  survProb <- dat %>%
    #dplyr::filter(strata == !!strata) %>% #filter on strata
    dplyr::mutate(
      tt = abs(time - !!t) # find absolute value of time column with t
    ) %>% # this will find value closest to zero
    group_by(outcomeName) %>%
    dplyr::arrange(tt) %>% #sort from smallest to largest
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(outcomeName, estimate, conf.low, conf.high) %>%
    dplyr::mutate(
      t = label, .before = 1
    )

  return(survProb)
}


findTimeAtSurvProb <- function(dat, strata, p) {

  label <- paste("p", p,  sep = "_")
  tmp <- dat %>%
    dplyr::filter(strata == !!strata) #filter on strata

  timeEst <- tmp %>%
    dplyr::mutate(
      pp = abs(estimate - !!p) # find absolute value of time column with p
    ) %>% # this will find two values closest to zero
    dplyr::arrange(pp) %>% #sort from smallest to largest
    dplyr::slice(1) %>%
    dplyr::select(strata, time)

  ciBounds <- tibble::tibble(
    'conf.low' = tmp$time[min(which(tmp$conf.low < p))],
    'conf.high' = tmp$time[max(which(tmp$conf.high > p)) + 1]
  )

  timeEst <- dplyr::bind_cols(timeEst, ciBounds) %>%
    dplyr::mutate(
      p = label, .before = 1
    )

  return(timeEst)
}


getSurvProbTab <- function(kmData,
                           targetCohortId, # select target cohort
                           database, # select database
                           nLines = 10L # determine the number of lines to plot default is 10
) {

  # subset data based on targetId, database and era
  dat <- kmData %>%
    dplyr::filter(
      databaseId == !!database,
      targetCohortId == !!targetCohortId
    )

  # determine the top 8 treatment lines and subset dat to only show these
  keep <- dat %>%
    count(strata) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice(1:nLines) %>%
    dplyr::pull(strata)

  # filter data to only keep top 8 treatment lines
  dat2 <- dat %>%
    dplyr::filter(
      strata %in% keep
    )

  #list times 0.5 yrs (6months), 1 year and 2 years
  t <- c(0.5, 1, 2)
  # expand grid to make tabular structure of permutations. strata per t
  mm <- tidyr::expand_grid(keep, t)
  #function factory of survProb at a specified time
  survProbTab <- purrr::pmap_dfr(
    mm,
    ~findSurvProbAtTime(
      dat = dat2,
      strata = ..1, # column for strata
      t = ..2 # column for time
    )
  )

  return(survProbTab)
}


#test
# getSurvProbTab(kmData = kmData,
#                targetCohortId = 1L,
#                database = "cprdAurum",
#                era = "era_30")

getTimeTab <- function(kmData,
                       targetCohortId, # select target cohort
                       database, # select database
                       nLines = 10L # determine the number of lines to plot default is 8
) {

  # subset data based on targetId, database and era
  dat <- kmData %>%
    dplyr::filter(
      database == !!database,
      targetCohortId == !!targetCohortId
    )

  # determine the top 8 treatment lines and subset dat to only show these
  keep <- dat %>%
    count(strata) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice(1:nLines) %>%
    dplyr::pull(strata)

  # filter data to only keep top 8 treatment lines
  dat2 <- dat %>%
    dplyr::filter(
      strata %in% keep
    )
  #list probs 0.25, 0,5 and 0.75
  p <- c(0.25, 0.5, 0.75)
  # expand grid to make tabular structure of permutations. strata per p
  mm <- tidyr::expand_grid(keep, p)
  #function factory of time at a specified survProb

  timeTab <- purrr::pmap_dfr(
    mm,
    ~findTimeAtSurvProb(
      dat = dat2,
      strata = ..1,# column for strata
      p = ..2 # column for time
    )
  )

  return(timeTab)
}


# getTimeTab(kmData = kmData,
#            targetCohortId = 1L,
#            database = "cprdAurum",
#            era = "era_30")
