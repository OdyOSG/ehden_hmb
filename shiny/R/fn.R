
buildSankeyData <- function(dt) {

  # build links
  links <- dt %>%
    dplyr::select(event_cohort_name1:n) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = c(-row, -n),
                        names_to = 'column', values_to = 'source') %>%
    dplyr::mutate(column = match(column, names(dt))) %>%
    tidyr::drop_na(source) %>%
    dplyr::mutate(source = paste0(source, '__', column)) %>%
    dplyr::group_by(row) %>%
    dplyr::mutate(target = dplyr::lead(source, order_by = column)) %>%
    tidyr::drop_na(target, source) %>%
    dplyr::group_by(source, target) %>%
    dplyr::summarise(value = sum(n), .groups = 'drop') %>%
    dplyr::arrange(desc(value))


  # build nodes
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
    'links' = links,
    'nodes' = nodes
  )

  return(res)
}


plotSankey <- function(sankey) {

  links <- sankey$links
  nodes <- sankey$nodes

  label <- unique(links$type)
  label2 <- paste0("'", paste(label, collapse = "','"), "',", "'end'")

  kelly_colors <- unname(grafify::graf_palettes$kelly)[-1]

  col <- kelly_colors[seq_along(label)]
  col2 <- paste0("'", paste(col, collapse = "','"), "',", "'#1B1919FF'")

  myCol <- glue::glue('d3.scaleOrdinal() .domain([{label2}]) .range([{col2}])')

  #plot sankeyNetwork
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


plotYearlyIncidence <- function(dat) {

  plot_colors <- unname(grafify::graf_palettes$kelly)

  p <- dat %>%
    dplyr::rename(`Age Group` = AGE_GROUP_NAME) %>%
    dplyr::mutate(
      grp = paste(databaseId, OUTCOME_NAME, sep = "_"),
      START_YEAR = lubridate::ymd(START_YEAR, truncated = 2L),
      `Cohort Name` = OUTCOME_NAME
    ) %>%
    #ggplot(aes(x = START_YEAR, y = INCIDENCE_RATE_P1000PY, color = `Cohort Name`)) +
    ggplot(aes(x = START_YEAR, y = INCIDENCE_RATE_P1000PY, color = `Age Group`)) +
    geom_point() +
    geom_line(aes(group = grp)) +
    scale_x_date(name = "Year", date_labels = "%Y") +
    scale_color_manual(values = plot_colors) + #scale colors to kelly
    facet_grid(rows = vars(databaseId), cols = vars(`Age Group`)) +
    #facet_wrap(vars(databaseId)) +
    labs(
      y = "Incidence Rate (per 1000 person-years)"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1)
    )

  return(p)
}


plotYearlyIncidencePrevalence <- function(dat) {

  plot_colors <- unname(grafify::graf_palettes$kelly)

  p <- dat %>%
    dplyr::rename(`Age Group` = ageGroup) %>%
    dplyr::mutate(
      grp = paste(databaseName, outcome_cohort_name, sep = "_"),
      incidenceYear = lubridate::ymd(incidenceYear, truncated = 2L),
      `Cohort Name` = outcome_cohort_name
    ) %>%
    #ggplot(aes(x = incidenceYear, y = incidence_1000_pys, color = `Cohort Name`)) +
    ggplot(aes(x = incidenceYear, y = incidence_1000_pys, color = `Age Group`)) +
    geom_point() +
    geom_line(aes(group = grp)) +
    scale_x_date(name = "Year", date_labels = "%Y") +
    scale_color_manual(values = plot_colors) + #scale colors to kelly
    facet_grid(rows = vars(databaseName), cols = vars(`Age Group`)) +
    #facet_wrap(vars(databaseName)) +
    labs(
      y = "Incidence Rate (per 1000yrs)"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1)
    )

  return(p)
}


plotAgeDistribution <- function(dat) {

  noOfDbs <- length(unique(dat$`Database Name`))
  plotCols <- RColorBrewer::brewer.pal(n = noOfDbs+1, name = "Dark2")

  if (unique(dat$category) == "study_yr") {

    x_lab <- "Age in study's age categories"

  } else if (unique(dat$category) == "5_yr") {

    x_lab <- "Age in 5-year categories"

  } else {

    x_lab <- "Age in 10-year categories"

  }

  if (unique(dat$type) == "pct") {

    p <- dat %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = pct, fill = `Database Name`)) +
      ggplot2::geom_col(color = "white") +
      ggplot2::geom_text(aes(label = value),
                         size = 4,
                         vjust = -.2) +
      #ggplot2::facet_grid(rows = vars(cohortName), cols = vars(`Database Name`)) +
      ggplot2::facet_grid(rows = vars(`Database Name`), cols = vars(cohortName)) +
      ggplot2::scale_fill_manual(values = plotCols) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(suffix = ""),
        limits = c(0, 1)) +
      xlab(x_lab) +
      ylab("Percentage") +
      theme_bw() +
      theme(
        text = element_text(size = 15, family = "serif"),
        axis.text = element_text(size = 14, family = "serif"),
        legend.position = "bottom"
      )

  } else {

    p <- dat %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = count, fill = `Database Name`)) +
      ggplot2::geom_col(color = "white") +
      ggplot2::geom_text(aes(label = value),
                         size = 4,
                         vjust = -.2) +
      #ggplot2::facet_grid(rows = vars(cohortName), cols = vars(`Database Name`)) +
      ggplot2::facet_grid(rows = vars(`Database Name`), cols = vars(cohortName)) +
      ggplot2::scale_fill_manual(values = plotCols) +
      ggplot2::scale_y_continuous(
        limits = c(0, max(dat$count) + 50000L),
        labels = scales::label_comma()) +
      xlab(x_lab) +
      ylab("Person Count") +
      theme_bw() +
      theme(
        text = element_text(size = 15, family = "serif"),
        axis.text = element_text(size = 14, family = "serif"),
        legend.position = "bottom"
      )

  }


  return(p)
}


relabelStrata <- function(dat, oldLabels, newLabels) {

  for (i in seq_along(oldLabels)){
    dat$strata <- gsub(oldLabels[i], newLabels[i], dat$strata)
  }

  return(dat)
}


relabelOutcome <- function(dat, oldLabels, newLabels) {

  for (i in seq_along(oldLabels)){
    dat$outcomeCohortId <- gsub(oldLabels[i], newLabels[i], dat$outcomeCohortId)
  }

  return(dat)
}


# For strata ----------------------

plotKM <- function(dat) {

  plot_colors <- unname(grafify::graf_palettes$kelly)

  p <- ggplot(dat, aes(x = time, y = estimate, color = strata)) +
    geom_step(linewidth = 1.5) +
    scale_color_manual(values = plot_colors) + #scale colors to kelly
    scale_y_continuous(labels = scales::percent_format()) + # convert y axis to percent
    labs(x = 'Years', y = 'Probability of Survival') + # improve labels
    theme_classic() +
    theme(
      legend.text = element_text(size = 12)
    )

  return(p)
}


findSurvProbAtTime <- function(dat, strata, t) {

  label <- dplyr::case_when(
    t == 0.5 ~ "6 month",
    t == 1 ~ "1 year",
    t == 2 ~ "2 year"
  )

  survProb <- dat %>%
    dplyr::filter(
      strata == !!strata
    ) %>% #filter on strata
    dplyr::mutate(
      tt = abs(time - !!t) # find absolute value of time column with t
    ) %>% # this will find value closest to zero
    dplyr::arrange(tt) %>% #sort from smallest to largest
    dplyr::slice(1)  %>%
    dplyr::mutate(
      survivalTime = label
    ) %>%
    dplyr::select(
      database, `Cohort Name`, strata, survivalTime, estimate
    )

  return(survProb)
}


makeSurvProbTab <- function(dat) {

  # subset data
  # dat <- dat %>%
  #   dplyr::filter(
  #     database == !!database,
  #     targetId == !!targetId
  #   )

  # determine unique strata
  strataLines <- unique(dat$strata)

  # list time points
  t <- c(0.5, 1, 2)

  # make permutations
  permutations <- tidyr::expand_grid(
    strataLines,
    t
  )

  # create survProbTab
  survProbTab <- purrr::pmap_dfr(
    permutations,
    ~findSurvProbAtTime(
      dat = dat,
      strata = ..1,
      t = ..2
    )
  )

  # survProbTab <- relabelStrata(
  #   survProbTab,
  #   oldLabels = as.character(c(27:29, 31:35)),
  #   newLabels = c("oc", "danazol", "grha", "lglIUD",
  #                 "nsaids", "progestin", "tranexamicAcid", "ulipristalAcetate")
  # )

  survProbTab2 <- survProbTab %>%
    tidyr::pivot_wider(
      id_cols = c(database, `Cohort Name`, strata),
      names_from = survivalTime,
      values_from = estimate
    )

  return(survProbTab2)
}


# For outcome ----------------------------

plotKM2 <- function(dat) {

  plot_colors <- unname(grafify::graf_palettes$kelly)

  p <- ggplot(dat, aes(x = time, y = estimate, color = outcomeCohortId)) +
    geom_step(linewidth = 1.5) +
    scale_color_manual(values = plot_colors) + #scale colors to kelly
    scale_y_continuous(labels = scales::percent_format()) + # convert y axis to percent
    labs(x = 'Years', y = 'Probability of Survival') + # improve labels
    theme_classic() +
    theme(
      legend.text = element_text(size = 12)
    )

  return(p)
}


findSurvProbAtTime2 <- function(dat, outcome, t) {

  label <- dplyr::case_when(
    t == 0.5 ~ "6 month",
    t == 1 ~ "1 year",
    t == 2 ~ "2 year"
  )

  survProb <- dat %>%
    dplyr::filter(
      outcomeCohortId == !!outcome
    ) %>% #filter on strata
    dplyr::mutate(
      tt = abs(time - !!t) # find absolute value of time column with t
    ) %>% # this will find value closest to zero
    dplyr::arrange(tt) %>% #sort from smallest to largest
    dplyr::slice(1)  %>%
    dplyr::mutate(
      survivalTime = label
    ) %>%
    dplyr::select(
      database, `Cohort Name`, targetId, outcomeCohortId, survivalTime, estimate
    )

  return(survProb)
}


makeSurvProbTab2 <- function(dat) {


  # determine unique strata
  outcomeLines <- unique(dat$outcomeCohortId)

  # list time points
  t <- c(0.5, 1, 2)

  # make permutations
  permutations <- tidyr::expand_grid(
    outcomeLines,
    t
  )

  #debug(findSurvProbAtTime2)

  # create survProbTab
  survProbTab <- purrr::pmap_dfr(
    permutations,
    ~findSurvProbAtTime2(
      dat = dat,
      outcome = ..1,
      t = ..2
    )
  )

  # survProbTab <- relabelStrata(
  #   survProbTab,
  #   oldLabels = as.character(c(27:29, 31:35)),
  #   newLabels = c("oc", "danazol", "grha", "lglIUD",
  #                 "nsaids", "progestin", "tranexamicAcid", "ulipristalAcetate")
  # )

  survProbTab2 <- survProbTab %>%
    tidyr::pivot_wider(
      id_cols = c(database, `Cohort Name`, outcomeCohortId),
      names_from = survivalTime,
      values_from = estimate
    )

  return(survProbTab2)
}
