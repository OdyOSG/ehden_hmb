library(shiny)
library(htmltools)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(networkD3)
library(reactable)
library(ggplot2)
library(grafify)
library(markdown)
library(ggsurvfit)


## TTD --------------
### With - Single --------------

path <- here::here("shiny/data/ttd/with")

database <- c("optum", "cprdGold", "cprdAurum", "mrktscan")

cohort <- c(1, 1001, 1002, 1003)

allPaths <- tidyr::expand_grid(path, database, cohort) %>%
  dplyr::mutate(
    fullPath = fs::path(path, paste0("tte_", database, "_", cohort, ".rds"))
  )


for (i in 1:length(allPaths$fullPath)) {

  tte <- readr::read_rds(allPaths$fullPath[i])$Single

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
                            ),
                    )) +
    # theme(axis.text.x = element_text(hjust = -0.5),
    #       plot.margin = unit(c(5.5, 50, 5.5, 50), "points")) +
    labs(x = "Follow-up time, years")

  ggplot2::ggsave(filename = here::here("shiny/data/plots/ttd/with", paste0("ttd_single_", allPaths$database[i], "_", allPaths$cohort[i], ".png")),
                  width = 18, height = 14)

}


### With - All --------------
for (i in 1:length(allPaths$fullPath)) {

  tte <- readr::read_rds(allPaths$fullPath[i])$All

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

  ggplot2::ggsave(filename = here::here("shiny/data/plots/ttd/with", paste0("ttd_all_", allPaths$database[i], "_", allPaths$cohort[i], ".png")),
                  width = 18, height = 14)

}


### Without - Single --------------
path <- here::here("shiny/data/ttd/wo")

allPaths <- tidyr::expand_grid(path, database, cohort) %>%
  dplyr::mutate(
    fullPath = fs::path(path, paste0("tte_", database, "_", cohort, ".rds"))
  )


for (i in 1:length(allPaths$fullPath)) {

  tte <- readr::read_rds(allPaths$fullPath[i])$Single

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

  ggplot2::ggsave(filename = here::here("shiny/data/plots/ttd/wo", paste0("ttd_single_", allPaths$database[i], "_", allPaths$cohort[i], ".png")),
                  width = 18, height = 14)

}


### Without - All --------------
for (i in 1:length(allPaths$fullPath)) {

  tte <- readr::read_rds(allPaths$fullPath[i])$All

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

  ggplot2::ggsave(filename = here::here("shiny/data/plots/ttd/wo", paste0("ttd_all_", allPaths$database[i], "_", allPaths$cohort[i], ".png")),
                  width = 18, height = 14)

}


## TTI --------------
path <- here::here("shiny/data/tti")

allPaths <- tidyr::expand_grid(path, database, cohort) %>%
  dplyr::mutate(
    fullPath = fs::path(path, paste0("tti_", database, "_", cohort, ".rds"))
  )


for (i in 1:length(allPaths$fullPath)) {

  tti <- readr::read_rds(allPaths$fullPath[i])

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


  ggplot2::ggsave(filename = here::here("shiny/data/plots/tti", paste0("tti_", allPaths$database[i], "_", allPaths$cohort[i], ".png")),
                  width = 18, height = 14)

}

