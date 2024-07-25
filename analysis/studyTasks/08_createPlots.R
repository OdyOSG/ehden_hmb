# Dependencies ----------------

library(dplyr)
library(ggplot2)
library(ggsurvfit)

# C. Connection ----------------------

### Set connection Block
# <<<
configBlock <- "[block]"
# >>>


## TTD --------------
### Without - Single --------------

## Input path
path <- here::here("results", configBlock, "10_timeToDiscontinuation_rds")
resultsPath <- here::here("results", configBlock)

## Output path
outputPath <- here::here(resultsPath, "plots", "ttd", "wo") %>%
  fs::dir_create()

outputPath <- here::here(resultsPath, "plots", "ttd", "wo")

## List of files and names
listOfFiles <- list.files(path, full.names = TRUE, pattern = ".rds", recursive = TRUE)
nameOfFiles <- list.files(path, full.names = FALSE, pattern = ".rds", recursive = TRUE)
nameOfFiles2 <- gsub(pattern = ".rds", replacement = "", nameOfFiles)
nameOfFiles3 <- gsub(pattern = "tte", replacement = "ttd", nameOfFiles2)


## Create plots
for (i in 1:length(listOfFiles)) {

  tte <- readr::read_rds(listOfFiles[i])$Single

  ## Number of colors for lines
  colors <- colorspace::rainbow_hcl(unique(length(tte$strata)))

  tte |>
    ggsurvfit(size = 1) +
    scale_ggsurvfit(x_scales=list(breaks=c(0.5, 0:3))) +  # Breaks at 6m and 1-3 years
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    add_risktable(risktable_stats = "{n.risk} ({cum.event})",
                  risktable_height = 0.4,
                  hjust = 0,
                  size = 4, # increase font size of risk table statistics
                  theme =   # increase font size of risk table title and y-axis label
                    list(
                      theme_risktable_default(axis.text.y.size = 11,
                                              plot.title.size = 11),
                      theme(plot.title = element_text(face = "bold"),
                            plot.margin = unit(c(5.5, 50, 5.5, 5.5), "points"),
                            axis.text.x = element_text(hjust = -5)
                      )
                    )) +
    labs(x = "Follow-up time, years")

  ggplot2::ggsave(filename = here::here(outputPath, paste0(nameOfFiles3[i], "_single", ".png")),
                  width = 18, height = 14)

}


### Without - All --------------
## Create plots
for (i in 1:length(listOfFiles)) {

  tte <- readr::read_rds(listOfFiles[i])$All

  ## Number of colors for lines
  colors <- colorspace::rainbow_hcl(unique(length(tte$strata)))

  tte |>
    ggsurvfit(size = 1) +
    scale_ggsurvfit(x_scales=list(breaks=c(0.5, 0:3))) +  # Breaks at 6m and 1-3 years
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    add_risktable(risktable_stats = "{n.risk} ({cum.event})",
                  risktable_height = 0.4,
                  hjust = 0,
                  size = 4, # increase font size of risk table statistics
                  theme =   # increase font size of risk table title and y-axis label
                    list(
                      theme_risktable_default(axis.text.y.size = 11,
                                              plot.title.size = 11),
                      theme(plot.title = element_text(face = "bold"),
                            plot.margin = unit(c(5.5, 50, 5.5, 5.5), "points"),
                            axis.text.x = element_text(hjust = -5)
                      )
                    )) +
    labs(x = "Follow-up time, years")

  ggplot2::ggsave(filename = here::here(outputPath, paste0(nameOfFiles3[i], "_all", ".png")),
                  width = 18, height = 14)

}



### With - Single --------------
## Input path
path <- here::here("results", configBlock, "10_timeToDiscontinuation2_rds")

## Output path
outputPath <- here::here(resultsPath, "plots", "ttd", "with") %>%
  fs::dir_create()

outputPath <- here::here(resultsPath, "plots", "ttd", "with")

## List of files and names
listOfFiles <- list.files(path, full.names = TRUE, pattern = ".rds", recursive = TRUE)
nameOfFiles <- list.files(path, full.names = FALSE, pattern = ".rds", recursive = TRUE)
nameOfFiles2 <- gsub(pattern = ".rds", replacement = "", nameOfFiles)
nameOfFiles3 <- gsub(pattern = "tte", replacement = "ttd", nameOfFiles2)


for (i in 1:length(listOfFiles)) {

  tte <- readr::read_rds(listOfFiles[i])$Single

  ## Number of colors for lines
  colors <- colorspace::rainbow_hcl(unique(length(tte$strata)))

  tte |>
    ggsurvfit(size = 1) +
    scale_ggsurvfit(x_scales=list(breaks=c(0.5, 0:3))) +  # Breaks at 6m and 1-3 years
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    add_risktable(risktable_stats = "{n.risk} ({cum.event})",
                  #add_risktable(risktable_stats = "{n.risk} ({cum.censor})",
                  risktable_height = 0.4,
                  hjust = 0,
                  size = 4, # increase font size of risk table statistics
                  theme =   # increase font size of risk table title and y-axis label
                    list(
                      theme_risktable_default(axis.text.y.size = 11,
                                              plot.title.size = 11),
                      theme(plot.title = element_text(face = "bold"))
                    )) +
    theme(axis.text.x = element_text(hjust = 0)) +
    labs(x = "Follow-up time, years")

  ggplot2::ggsave(filename = here::here(outputPath, paste0(nameOfFiles3[i], "_single", ".png")),
                  width = 18, height = 14)

}


### Without - All --------------
for (i in 1:length(listOfFiles)) {

  tte <- readr::read_rds(listOfFiles[i])$All

  ## Number of colors for lines
  colors <- colorspace::rainbow_hcl(unique(length(tte$strata)))

  tte |>
    ggsurvfit(size = 1) +
    scale_ggsurvfit(x_scales=list(breaks=c(0.5, 0:3))) +  # Breaks at 6m and 1-3 years
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    add_risktable(risktable_stats = "{n.risk} ({cum.event})",
                  #add_risktable(risktable_stats = "{n.risk} ({cum.censor})",
                  risktable_height = 0.4,
                  hjust = 0,
                  size = 4, # increase font size of risk table statistics
                  theme =   # increase font size of risk table title and y-axis label
                    list(
                      theme_risktable_default(axis.text.y.size = 11,
                                              plot.title.size = 11),
                      theme(plot.title = element_text(face = "bold"))
                    )) +
    theme(axis.text.x = element_text(hjust = 0)) +
    labs(x = "Follow-up time, years")

  ggplot2::ggsave(filename = here::here(outputPath, paste0(nameOfFiles3[i], "_all", ".png")),
                  width = 18, height = 14)

}


## TTI --------------
## Input path
path <- here::here("results", configBlock, "12_timeToIntervention_rds")
resultsPath <- here::here("results", configBlock)

## Output path
outputPath <- here::here(resultsPath, "plots", "tti") %>%
  fs::dir_create()

outputPath <- here::here(resultsPath, "plots", "tti")

## List of files and names
listOfFiles <- list.files(path, full.names = TRUE, pattern = ".rds", recursive = TRUE)
nameOfFiles <- list.files(path, full.names = FALSE, pattern = ".rds", recursive = TRUE)
nameOfFiles2 <- gsub(pattern = ".rds", replacement = "", nameOfFiles)


for (i in 1:length(listOfFiles)) {

  tti <- readr::read_rds(listOfFiles[i])

  ## Number of colors for lines
  colors <- colorspace::rainbow_hcl(unique(length(tti$strata)))

  tti |>
    ggsurvfit(size = 1) +
    scale_ggsurvfit(x_scales=list(breaks=c(0.5, 0:3))) +  # Breaks at 6m and 1-3 years
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    add_risktable(risktable_stats = "{n.risk} ({cum.event})",
                  #add_risktable(risktable_stats = "{n.risk} ({cum.censor})",
                  risktable_height = 0.4,
                  hjust = 0,
                  size = 4, # increase font size of risk table statistics
                  theme =   # increase font size of risk table title and y-axis label
                    list(
                      theme_risktable_default(axis.text.y.size = 11,
                                              plot.title.size = 11),
                      theme(plot.title = element_text(face = "bold"))
                    )) +
    theme(axis.text.x = element_text(hjust = 0)) +
    labs(x = "Follow-up time, years")


  ggplot2::ggsave(filename = here::here(outputPath, paste0(nameOfFiles2[i], ".png")),
                  width = 18, height = 14)

}

