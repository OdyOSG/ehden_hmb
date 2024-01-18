# A. File Info -----------------------

# Task: Execute Study
# Description: The purpose of the _executeStudy.R script is to provide functions to execute the study.


# B. Functions ------------------------

## helpers -----------

find_config_block <- function(lines, startBlock = "# <<<", endBlock = "# >>>") {

  start <- which(lines == "# <<<")
  end <- which(lines == "# >>>")

  ll <- c(start + 1L, end - 1L)

  return(ll)
}


prep_studyTask <- function(lines, value) {

  # create new config
  blockLines <- find_config_block(lines)
  start <- blockLines[1]
  end <- blockLines[2]
  configBlock <- lines[rlang::seq2(start, end)]
  new_configBlock <- gsub("\\[block\\]", value, configBlock)


  lines2 <- c(lines[rlang::seq2(1, start - 1L)], new_configBlock, lines[rlang::seq2(end + 1, length(lines))]) |>
    paste(collapse = "\n")

  return(lines2)
}


runStudyTask <- function(file, configBlock, env = rlang::caller_env()) {

  rLines <- readr::read_lines(file)
  newLines <- prep_studyTask(lines = rLines, value = configBlock)
  exprs <- rlang::parse_exprs(newLines)
  res <- NULL
  for (i in seq_along(exprs)) {
    res <- eval(exprs[[i]], env)
  }

  invisible(res)
}
