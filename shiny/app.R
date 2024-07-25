# Shiny App Script

# Dependencies -------------------
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
options(shiny.fullstacktrace = FALSE)


# Variables ---------------------
title <- "EHDEN HMB"

underlyingDescription <- "Counts equal to 5 and below have been masked and replaced with '=<5'."
drugUtilizationDescription <- "Counts equal to 5 and below have been masked and replaced with '=<5."
drugUtilizationDescription2 <-  "Category 'Within time window' means that a drug exposure started and ended within the specified time window."
drugUtilizationDescription3 <-  "Category 'Complete follow-up' means that a drug exposure spanned throughout the specified time window."
clinicalCharacteristicsDescription <- "Counts equal to 5 and below have been masked and replaced with '=<5'. Characteristics are assessed within a time window of 365 to 0 days prior to index date"
incidenceDescription1 <- "Incidence rate is defined as Outcome Count/Person Years * 1000."
incidenceDescription2 <- "Incidence proportion is defined as Outcome Count/Persons at Risk * 100."
treatmentPatternsDescription <- "Treatment Patterns counts (Sequences) are restricted to 30. HMB cohort is the population whose index event is HMB and are censored after hysterectomy."
procedureAnalysisDescription <- "Counts equal to 5 and below have been masked and replaced with '=<5'.
In the 'Time to Intervention' tab we consider the time for only those who experienced the event, not the entire cohort population."

dashboardVersion <- "2.1.2"
dashboardDate <- Sys.Date()

# Functions ---------------
csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}

# App Ui ---------------------
## Header ------------
header <- dashboardHeader(title = title,
                          tags$li(
                            div(
                              img(
                                src = 'ehden.png',
                                title = "title",
                                height = "45px",
                                width = "50px"),
                              style = "padding-top:0px; padding-bottom:0px;"),
                            class = "dropdown")
)

## Sidebar  -----------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("About", tabName = "about", icon = shiny::icon("book", lib = "font-awesome")),
    menuItem("Cohorts", tabName = "cohorts", icon = shiny::icon("circle-user", lib = "font-awesome")),
    menuItem("Clinical Characteristics", tabName = "clinChar", icon = shiny::icon("clipboard", lib = "font-awesome")),
    menuItem("Incidence", tabName = "inci2", icon = shiny::icon("vial", lib = "font-awesome")),
    menuItem("Underlying Conditions", tabName = "cond", icon = shiny::icon("disease", lib = "font-awesome")),
    menuItem("Treatment Patterns",  tabName = "txPath", icon = shiny::icon("worm", lib = "font-awesome")),
    menuItem("Procedure Analysis",  tabName = "proc", icon = shiny::icon("x-ray", lib = "font-awesome"))
  ),
  h6(style = "position: absolute; bottom: 60px; left: 10px", glue::glue("Dashboard Created by:")),
  h6(style = "position: absolute; bottom: 45px; left: 15px", glue::glue("Odysseus Data Services")),
  h6(style = "position: absolute; bottom: 30px; left: 15px", glue::glue("Version {dashboardVersion}")),
  h6(style = "position: absolute; bottom: 15px; left: 15px", glue::glue("Date {dashboardDate}"))
)

## Body  -----------
body <- dashboardBody(
  tabItems(

    ### About Tab------
    tabItem(
      tabName = "about",

      #### Study description
      fluidRow(
        box(
          title = "Study Description",
          width = 12,
          status = "success",
          includeMarkdown("StudyDescription.md")
        )
      ),

      #### Study Information
      fluidRow(
        box(
          title = "Study Information",
          width = 12,
          status = "success",
          "Github Page:",a(href= "https://github.com/OdyOSG/ehden_hmb" ,"https://github.com/OdyOSG/ehden_hmb"),
          br(),
          "SAP Link:",a(href= "https://odyosg.github.io/ehden_hmb/sap.html", "https://odyosg.github.io/ehden_hmb/sap.html")
        )
      ),

      #### Database Information
      fluidRow(
        box(
          title = "Database Information",
          status = "success",
          width = 12,
          includeMarkdown("databaseInformation.md")
        )
      )
    ),

    ### Cohorts Tab -----------------
    tabItem(
      tabName = "cohorts",
      fluidRow(
        box(
          collapsible = T,
          collapsed = F,
          title = "Cohort & Strata Counts",
          width = 12,
          background = "light-blue"
        )
      ),

      fluidRow(
        tabBox(
          id = "cohortCounts",
          width = 12,

          #### Cohort Counts
          tabPanel("Cohort Counts",
                   fluidRow(
                     box(
                       status = "success",
                       column(width = 12,
                              pickerInput(
                                inputId = "databaseNameCohort",
                                label = "Database Name",
                                choices = databaseName,
                                selected = databaseName,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       )
                     )
                   ),
                   fluidRow(
                     box(width = 12,
                         reactableOutput("cohortCountsTab"),
                         csvDownloadButton("cohortCountsTab", filename = "cohortCounts.csv")
                     )
                   )
          ),

          #### Strata Counts
          tabPanel("Strata Counts",
                   fluidRow(
                     box(
                       status = "success",
                       column(width = 12,
                              pickerInput(
                                inputId = "databaseNameStrata",
                                label = "Database Name",
                                choices = databaseName,
                                selected = databaseName,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       )
                     )
                   ),
                   fluidRow(
                     box(width = 12,
                         reactableOutput("strataCountsTab"),
                         csvDownloadButton("strataCountsTab", filename = "strataCounts.csv"))
                   )
          )
        )
      )
    ),

    ### Clinical Characteristics Tab -----------------
    tabItem(
      tabName = "clinChar",
      fluidRow(
        box(
          collapsible = T,
          collapsed = F,
          title = "Clinical Characteristics",
          width = 12,
          background = "light-blue",
          textOutput("clinicalCharacteristicsDescription")
        )
      ),
      fluidRow(
        tabBox(
          id = "baselineChar",
          width = 12,
          tabPanel("Demographics",
                   fluidRow(
                     box(
                       status = "success",
                       column(width = 6,

                              pickerInput(
                                inputId = "databaseNameDemo",
                                label = "Database Name",
                                choices = databaseName,
                                selected = databaseName,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "cohortNameDemo",
                                label = "Cohort Name",
                                choices = cohortNameDemo,
                                selected = cohortNameDemo,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       )
                     ),
                     fluidRow(
                       box(
                         width = 12,
                         reactableOutput("demoCharTab"),
                         csvDownloadButton("demoCharTab", filename = "baselineDemographics.csv")
                       )
                     )
                   )

          ),
          tabPanel("Continuous",
                   fluidRow(
                     box(
                       status = "success",
                       column(width = 6,

                              pickerInput(
                                inputId = "databaseNameCts",
                                label = "Database Name",
                                choices = databaseName,
                                selected = databaseName,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "cohortNameCts",
                                label = "Cohort Name",
                                choices = cohortNameCts,
                                selected = cohortNameCts,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       )
                     ),
                     fluidRow(
                       box(
                         width = 12,
                         reactableOutput("ctsCharTab"),
                         csvDownloadButton("ctsCharTab", filename = "baselineContinuous.csv")
                       )
                     )
                   )

          ),
          tabPanel("Concept",
                   fluidRow(
                     box(
                       status = "success",
                       column(width = 6,

                              pickerInput(
                                inputId = "databaseNameConcept",
                                label = "Database Name",
                                choices = databaseName,
                                selected = databaseName,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "cohortNameConcept",
                                label = "Cohort Name",
                                choices = cohortNameConcept,
                                selected = cohortNameConcept,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),
                       ),
                       column(width = 6,

                              pickerInput(
                                inputId = "domainConcept",
                                label = "Domain",
                                choices = domainConceptChar,
                                selected = domainConceptChar,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       )
                     ),
                     fluidRow(
                       box(
                         width = 12,
                         reactableOutput("conceptCharTab")
                       )
                     )
                   )

          ),
          tabPanel("Cohorts",
                   fluidRow(
                     box(
                       status = "success",
                       column(width = 6,

                              pickerInput(
                                inputId = "databaseNameCohortCov",
                                label = "Database Name",
                                choices = databaseName,
                                selected = databaseName,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "cohortNameCohortCov",
                                label = "Cohort Name",
                                choices = cohortNameCohort,
                                selected = cohortNameCohort,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       ),
                       column(width = 6,

                              pickerInput(
                                inputId = "domainCohortCov",
                                label = "Domain",
                                choices = domainCohortChar,
                                selected = domainCohortChar,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       )
                     ),
                     fluidRow(
                       box(
                         width = 12,
                         reactableOutput("cohortCharTab"),
                         csvDownloadButton("cohortCharTab", filename = "baselineCohorts.csv")
                       )
                     )
                   )
          ),

          tabPanel("ICD10Chapters",
                   fluidRow(
                     box(
                       status = "success",
                       column(width = 6,

                              pickerInput(
                                inputId = "databaseNameIcd",
                                label = "Database Name",
                                choices = databaseName,
                                selected = databaseName,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "cohortNameIcd",
                                label = "Cohort Name",
                                choices = cohortNameICD,
                                selected = cohortNameICD,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       )
                     ),
                     fluidRow(
                       box(
                         width = 12,
                         reactableOutput("icdCharTab"),
                         csvDownloadButton("icdCharTab", filename = "baselineChapters.csv"))
                     )
                   )
          ),

          tabPanel("Age Distribution",

                   fluidRow(
                     box(
                       status = "success",
                       column(
                         width = 6,
                         pickerInput(
                           inputId = "ageDisType",
                           label = "Statistic",
                           choices = ageDisType,
                           selected = ageDisType,
                           options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                           multiple = FALSE
                         ),

                         pickerInput(
                           inputId = "ageDisCat",
                           label = "Age Group Category",
                           choices = ageDisCat,
                           selected = ageDisCat,
                           options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                           multiple = FALSE
                         )
                       ),

                       column(
                         width = 6,
                         pickerInput(
                           inputId = "ageDisDb",
                           label = "Database",
                           choices = ageDisDb,
                           selected = ageDisDb,
                           options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                           multiple = TRUE
                         )
                       )
                     )
                   ),

                   fluidRow(
                     box(
                       width = 12,
                       plotOutput("ageDis")
                     )
                   )
          )

        )
      )
    ),


    # ### Incidence Tab -----------------
    # tabItem(
    #   tabName = "inci",
    #
    #   fluidRow(
    #     box(
    #       collapsible = T,
    #       collapsed = F,
    #       title = "Incidence",
    #       width = 12,
    #       background = "light-blue",
    #       textOutput("clinicalOutcomesDescription")
    #     )
    #   ),
    #
    #   fluidRow(
    #     tabBox(
    #       id = "baselineChar",
    #       width = 12,
    #
    #       tabPanel("Table",
    #         fluidRow(
    #           box(
    #             status = "success",
    #             column(width = 6,
    #
    #                    pickerInput(
    #                      inputId = "databaseNameInci",
    #                      label = "Database Name",
    #                      choices = databaseInci,
    #                      selected = databaseInci,
    #                      options = shinyWidgets::pickerOptions(actionsBox = TRUE),
    #                      multiple = TRUE
    #                    ),
    #
    #                    pickerInput(
    #                      inputId = "cohortNameInci",
    #                      label = "Cohort Name",
    #                      choices = cohortName,
    #                      selected = cohortName,
    #                      options = shinyWidgets::pickerOptions(actionsBox = TRUE),
    #                      multiple = TRUE
    #                    )
    #
    #
    #             ),
    #             column(width = 6,
    #
    #                    pickerInput(
    #                      inputId = "yearInci",
    #                      label = "Year",
    #                      choices = yearInci,
    #                      selected = yearInci[1],
    #                      options = shinyWidgets::pickerOptions(actionsBox = TRUE),
    #                      multiple = TRUE
    #                    ),
    #
    #                    pickerInput(
    #                      inputId = "ageInci",
    #                      label = "Age Group",
    #                      choices = ageInci,
    #                      selected = "Total",
    #                      options = shinyWidgets::pickerOptions(actionsBox = TRUE),
    #                      multiple = TRUE
    #                    )
    #             )
    #           )
    #         ),
    #         fluidRow(
    #           box(
    #             width = 12,
    #             reactableOutput("inciTab"),
    #             csvDownloadButton("inciTab", filename = "incidence.csv")
    #           )
    #         )
    #       ),
    #       tabPanel("Yearly Trend",
    #
    #                fluidRow(
    #                 box(
    #                  status = "success",
    #                  column(
    #                         width = 6,
    #                           pickerInput(
    #                             inputId = "databaseNameYrInci",
    #                             label = "Database Name",
    #                             choices = databaseName,
    #                             selected = databaseName,
    #                             options = shinyWidgets::pickerOptions(actionsBox = TRUE),
    #                             multiple = TRUE
    #                           ),
    #
    #                           pickerInput(
    #                             inputId = "cohortNameYrInci",
    #                             label = "Cohort Name",
    #                             choices = cohortNameInciPlot,
    #                             selected = cohortNameInciPlot,
    #                             options = shinyWidgets::pickerOptions(actionsBox = TRUE),
    #                             multiple = TRUE
    #                           )
    #                         #)
    #                  ),
    #
    #                  column(
    #                    width = 6,
    #                    #box(status = "success",
    #                        pickerInput(
    #                          inputId = "ageGroupYrInci",
    #                          label = "Age Group",
    #                          choices = ageGroupInciPlot,
    #                          selected = ageGroupInciPlot,
    #                          options = shinyWidgets::pickerOptions(actionsBox = TRUE),
    #                          multiple = TRUE
    #                        )
    #                   #)
    #                  )
    #                 )
    #                ),
    #
    #                fluidRow(
    #                  box(
    #                    width = 12,
    #                    plotOutput("inciYearPlot")
    #                  )
    #                )
    #       )
    #     )
    #   )
    # ),


    ### Incidence Tab (IncidencePrevalence) -----------------
    tabItem(
      tabName = "inci2",

      fluidRow(
        box(
          collapsible = T,
          collapsed = F,
          title = "Incidence",
          width = 12,
          background = "light-blue",
          textOutput("clinicalOutcomesDescriptionInciPre1"),
          textOutput("clinicalOutcomesDescriptionInciPre2")
        )
      ),

      fluidRow(
        tabBox(
          id = "inciAna",
          width = 12,

          tabPanel("Table",
                   fluidRow(
                     box(
                       status = "success",
                       column(width = 6,

                              pickerInput(
                                inputId = "databaseInci2",
                                label = "Database Name",
                                choices = databaseInci2,
                                selected = databaseInci2,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "cohortNameInci2",
                                label = "Cohort Name",
                                choices = cohortNameInci2,
                                selected = cohortNameInci2,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )

                       ),
                       column(width = 6,

                              pickerInput(
                                inputId = "yearInci2",
                                label = "Year",
                                choices = yearInci2,
                                selected = "All",
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "ageInci2",
                                label = "Age Group",
                                choices = ageInci2,
                                selected = "Total",
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       )
                     )
                   ),
                   fluidRow(
                     box(
                       width = 12,
                       reactableOutput("inciTab2"),
                       csvDownloadButton("inciTab2", filename = "incidence2.csv")
                     )
                   )
          ),
          tabPanel("Yearly Trend",

                   fluidRow(
                     box(
                       status = "success",
                       column(
                         width = 6,
                         pickerInput(
                           inputId = "databaseInciPlot2",
                           label = "Database Name",
                           choices = databaseInciPlot2,
                           selected = databaseInciPlot2,
                           options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                           multiple = TRUE
                         ),

                         pickerInput(
                           inputId = "cohortNameInci2Plot",
                           label = "Cohort Name",
                           choices = cohortNameInci2Plot,
                           selected = cohortNameInci2Plot,
                           options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                           multiple = TRUE
                         )
                       ),

                       column(
                         width = 6,
                         pickerInput(
                           inputId = "ageInci2Plot",
                           label = "Age Group",
                           choices = ageInci2Plot,
                           selected = ageInci2Plot,
                           options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                           multiple = TRUE
                         )
                       )
                     )
                   ),

                   fluidRow(
                     box(
                       width = 12,
                       plotOutput("inciYearPlot2")
                     )
                   )
          )
        )
      )
    ),


    ### Underlying Conditions Tab -----------------
    tabItem(
      tabName = "cond",
      fluidRow(
        box(
          collapsible = T,
          collapsed = F,
          title = "Underlying Conditions",
          width = 12,
          background = "light-blue",
          textOutput("underlyingDescription")
        )
      ),
      fluidRow(
        box(
          status = "success",
          column(width = 6,

                 pickerInput(
                   inputId = "databaseNameCondPi",
                   label = "Database Name",
                   choices = databaseName,
                   selected = databaseName,
                   options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                   multiple = TRUE
                 ),

                 pickerInput(
                   inputId = "cohortNameCondPi",
                   label = "Cohort Name",
                   choices = condCohort,
                   selected = condCohort,
                   options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                   multiple = TRUE
                 )

          ),
          column(width = 6,

                 pickerInput(
                   inputId = "conditionNameCondPi",
                   label = "Condition Name",
                   choices = condCov,
                   selected = condCov,
                   options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                   multiple = TRUE
                 ),


                 pickerInput(
                   inputId = "timeWindowCondPi",
                   label = "Time Window",
                   choices = condTimeWindow,
                   selected = condTimeWindow,
                   options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                   multiple = TRUE
                 )
          )
        )
      ),
      fluidRow(
        box(
          width = 12,
          reactableOutput("condPiTab"),
          csvDownloadButton("condPiTab", filename = "underlyingConditions.csv")
        )
      )
    ),


    ### Treatment patterns Tab -----------------
    tabItem(
      tabName = "txPath",
      fluidRow(
        box(
          collapsible = T,
          collapsed = F,
          title = "Treatment Patterns",
          width = 12,
          background = "light-blue",
          textOutput("treatmentPatternsDescription"),
          textOutput("drugUtilizationDescription2"),
          textOutput("drugUtilizationDescription3"),
          textOutput("drugUtilizationDescription")
        )
      ),
      fluidRow(
        tabBox(
          id = "txPathTab",
          width = 12,

          ### Utilization Panel
          tabPanel("Drug Utilization",
                   fluidRow(
                     box(
                       status = "success",
                       column(width = 4,

                              pickerInput(
                                inputId = "databaseNameDuPi",
                                label = "Database Name",
                                choices = databaseName,
                                selected = databaseName,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "cohortNameDuPi",
                                label = "Cohort Name",
                                choices = drugCohort,
                                selected = drugCohort,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       ),
                       column(width = 4,

                              pickerInput(
                                inputId = "drugNameDuPi",
                                label = "Drug Name",
                                choices = drugCov,
                                selected = drugCov,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "timeWindowDuPi",
                                label = "Time Window",
                                choices = drugTimeWindow,
                                selected = drugTimeWindow,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       ),
                       column(width = 4,

                              pickerInput(
                                inputId = "drugCat",
                                label = "Category",
                                choices = drugCategory,
                                selected = drugCategory,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       )
                     ),
                     fluidRow(
                       box(
                         width = 12,
                         reactableOutput("duTab"),
                         csvDownloadButton("duTab", filename = "postIndexDrugUtilization.csv")
                       )
                     )
                   )
          ),

          ### Sequences Panel
          tabPanel("Sequences",
                   fluidRow(
                     box(
                      status = "success",
                      width = 4,
                      column(width = 6,

                       pickerInput(
                         inputId = "databaseNameSankey",
                         label = "Database Name",
                         choices = databaseName
                       ),

                       pickerInput(
                         inputId = "cohortNameSankey",
                         label = "Cohort Name",
                         choices = cohortName2
                       ),
                      ),
                      column(width = 6,

                       pickerInput(
                         inputId = "txTime",
                         label = "Time from index date",
                         choices = txTime
                       ),

                       pickerInput(
                         inputId = "txType",
                         label = "Type",
                         choices = txType
                       )
                      )
                     )
                   ),
                   fluidRow(
                     box(width = 12 ,
                         networkD3::sankeyNetworkOutput("txSankey",
                                                         width = "auto",
                                                         height = "500px")
                     )
                   ),
                   fluidRow(
                     box(
                       width = 12,
                       reactableOutput("txPatDat"))
                   )
          ),

          ### TTD Panel (without)
          tabPanel("Time to Discontinuation (without NSAIDs)",
                   fluidRow(
                     box(
                      status = "success",
                      column(width = 6,
                       height = "230px",
                       background = "light-blue",

                       pickerInput(
                         inputId = "databaseNameTtd",
                         label = "Database Name",
                         choices = databaseName
                       ),

                       pickerInput(
                         inputId = "cohortNameTtd",
                         label = "Cohort Name",
                         choices = tteCohorts$name
                       ),
                      ),
                      column(width = 6,

                       pickerInput(
                         inputId = "strataTtd",
                         label = "Drugs",
                         choices = ttdLine
                       )
                      )
                     )
                   ),
                   fluidRow(
                     column(width = 12, align = "center",
                       box(
                        width = 12,
                        plotOutput(height = "750px",
                                   width = "auto",
                                   "ttd_km_without")
                      )
                     ),

                   ),
                   fluidRow(
                     box(
                       width = 12,
                       reactableOutput("ttdSurvTab")
                     )
                   )
          ),

          ### TTD Panel (with)
          tabPanel("Time to Discontinuation (with NSAIDs)",
                   fluidRow(
                     box(
                       status = "success",
                       column(width = 6,
                              height = "230px",
                              background = "light-blue",

                              pickerInput(
                                inputId = "databaseNameTtd2",
                                label = "Database Name",
                                choices = databaseName
                              ),

                              pickerInput(
                                inputId = "cohortNameTtd2",
                                label = "Cohort Name",
                                choices = tteCohorts$name
                              ),
                       ),
                       column(width = 6,

                              pickerInput(
                                inputId = "strataTtd2",
                                label = "Drugs",
                                choices = ttdLine
                              )
                       )
                     )
                   ),
                   fluidRow(
                     column(width = 12, align = "center",
                            box(
                              width = 12,
                              plotOutput(height = "750px",
                                         width = "auto",
                                         "ttd_km_with")
                            )
                     ),

                   ),
                   fluidRow(
                     box(
                       width = 12,
                       reactableOutput("ttdSurvTab2")
                     )
                   )
          )
        )
      )
    ),


    ### Procedure Analysis Tab -----------------
    tabItem(
      tabName = "proc",
      fluidRow(

        box(
          collapsible = T,
          collapsed = F,
          title = "Procedure Analysis",
          width = 12,
          background = "light-blue",
          textOutput("procedureAnalysisDescription")
        ),

        tabBox(
          id = "procAnalysis",
          width = 12,
          tabPanel("Procedure Prevalence",
                   fluidRow(
                     box(
                       status = "success",
                       column(width = 6,

                              pickerInput(
                                inputId = "databaseNameProcPi",
                                label = "Database Name",
                                choices = databaseName,
                                selected = databaseName,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "cohortNameProcPi",
                                label = "Cohort Name",
                                choices = procTarget,
                                selected = procTarget[1],
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       ),
                       column(width = 6,

                              pickerInput(
                                inputId = "procNameProcPi",
                                label = "Procedure Name",
                                choices = procCohorts,
                                selected = procCohorts,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "timeWindowProcPi",
                                label = "Time Window",
                                choices = procTimeWindow,
                                selected = procTimeWindow,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       )
                     )
                   ),
                   fluidRow(
                     box(
                       width = 12,
                       reactableOutput("procTab"),
                       csvDownloadButton("procTab", filename = "postIndexProcedures.csv")
                     )
                   )
          ),

          ## Time to intervention
          tabPanel("Time to Intervention",
                   fluidRow(
                     box(
                     status = "success",
                     column(width = 6,
                              height = "180px",
                              background = "light-blue",

                              pickerInput(
                                inputId = "databaseNameTti",
                                label = "Database Name",
                                choices = databaseName
                              ),

                              pickerInput(
                                inputId = "cohortNameTti",
                                label = "Cohort Name",
                                choices = ttiCohorts$name
                              )
                      ),
                    ),
                   ),
                   fluidRow(
                     column(width = 12, align = "center",
                            box(
                             width = 12,
                             plotOutput(height = "750px",
                                        width = "auto",
                                        "tti_km")

                    )
                   )
                  ),
                  fluidRow(
                    box(
                       width = 12,
                       reactableOutput("ttiSurvTab")
                    )
                )
          )
        )
      )
    )
  )
)


# Bind ui elements
ui <- dashboardPage(
  header,
  sidebar,
  body
)


# App Server ------------------

server <- function(input, output, session){

  # About --------------

  ## Study Description
  output$studyDescription <- renderText({
    description
  })

  # Cohorts ----------------

  ## Cohort Counts
  output$cohortCountsTab <- renderReactable(
    cohortCounts %>%
      dplyr::select(-`Cohort Id`) %>%
      dplyr::filter(Database %in% input$databaseNameCohort) %>%
      reactable(
        columns = list(Subjects = colDef(name = "Person Count", format = colFormat(separators = TRUE)),
                       Entries = colDef(name = "Record Count", format = colFormat(separators = TRUE))
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 20
      )
  )


  ## Strata Counts
  output$strataCountsTab <- renderReactable(
    strataCounts %>%
      dplyr::select(-`Strata Cohort Id`) %>%
      dplyr::filter(Database %in% input$databaseNameStrata) %>%
      reactable(
        columns = list(Subjects = colDef(name = "Person Count", format = colFormat(separators = TRUE))),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 20
      )
  )


  # Clinical Characteristics -------------

  ## Box text
  output$clinicalCharacteristicsDescription <- renderText({
    clinicalCharacteristicsDescription
  })


  ## Demographics
  output$demoCharTab <- renderReactable(
    demoChar %>%
      dplyr::filter(databaseId %in% input$databaseNameDemo,
                    cohortName %in% input$cohortNameDemo
      ) %>%
      reactable(
        columns = list(
          databaseId = colDef(name = "Database Name"),
          cohortName = colDef(name = "Cohort Name"),
          id = colDef(name = "Covariate Id"),
          Covariate = colDef(name = "Covariate Name"),
          count = colDef(name = "Person Count"),
          pct = colDef(name = "Percentage")
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 20
      )
  )

  ## Continuous
  output$ctsCharTab <- renderReactable(
    ctsChar %>%
      dplyr::filter(databaseId %in% input$databaseNameCts,
                    cohortName %in% input$cohortNameCts
      ) %>%
      reactable(
        columns = list(
          databaseId = colDef(name = "Database Name"),
          cohortName = colDef(name = "Cohort Name"),
          covariateId = colDef(name = "Covariate Id"),
          name = colDef(name = "Covariate Name"),
          medianValue = colDef(name = "Median", format = colFormat(separators = TRUE)),
          iqr = colDef(name = "IQR", format = colFormat(separators = TRUE))
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 20
      )
  )

  ## Concepts
  output$conceptCharTab <- renderReactable(
    conceptChar  %>%
      dplyr::filter(databaseId %in% input$databaseNameConcept,
                    cohortName %in% input$cohortNameConcept,
                    domain %in% input$domainConcept
      ) %>%
      reactable(
        columns = list(
          databaseId = colDef(name = "Database Name"),
          domain = colDef(name = "Domain"),
          cohortName = colDef(name = "Cohort Name"),
          conceptId = colDef(name = "Concept Id"),
          name = colDef(name = "Concept Name"),
          count = colDef(name = "Person Count"),
          pct = colDef(name = "Percentage")
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 20
      )
  )

  ## Cohorts
  output$cohortCharTab <- renderReactable(
    cohortChar %>%
      dplyr::filter(databaseId %in% input$databaseNameCohortCov,
                    cohortName %in% input$cohortNameCohortCov,
                    domain %in% input$domainCohortCov
      ) %>%
      reactable(
        columns = list(
          databaseId = colDef(name = "Database Name"),
          cohortName = colDef(name = "Cohort Name"),
          domain = colDef(name = "Domain"),
          covariateName = colDef(name = "Covariate Name"),
          count = colDef(name = "Person Count"),
          pct = colDef(name = "Percentage")
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 20
      )
  )

  ## ICD10 Chapters
  output$icdCharTab <- renderReactable(
    icdChar %>%
      dplyr::filter(databaseId %in% input$databaseNameIcd,
                    cohortName %in% input$cohortNameIcd
      ) %>%
      reactable(
        columns = list(
          databaseId = colDef(name = "Database Name"),
          cohortName = colDef(name = "Cohort Name"),
          CATEGORY_CODE = colDef(name = "Concept Id"),
          categoryName = colDef(name = "Concept Name"),
          count = colDef(name = "Person Count"),
          pct = colDef(name = "Percentage")
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 20
      )
  )


  ### Create age distribution object for plotting
  subsetAgeDis <- reactive({
    allAge %>%
      dplyr::filter(
        `Database Name` %in% input$ageDisDb,
         category %in% input$ageDisCat,
         type %in% input$ageDisType
      )
  })

  ### Create age distribution plot
  output$ageDis <- renderPlot({
    subsetAgeDis() %>%
      plotAgeDistribution()
  })



  # Incidence -----------

  # ## Box text
  # output$clinicalOutcomesDescription <- renderText({
  #   incidenceDescription
  # })
  #
  # ## Incidence
  # output$inciTab <- renderReactable(
  #   incTab %>%
  #     dplyr::filter(databaseId %in% input$databaseNameInci,
  #                   START_YEAR %in% input$yearInci,
  #                   AGE_GROUP_NAME %in% input$ageInci,
  #                   OUTCOME_NAME %in% input$cohortNameInci) %>%
  #     reactable(
  #       columns = list(
  #         databaseId = colDef(name = "Database Name"),
  #         START_YEAR = colDef(name = "Year"),
  #         AGE_GROUP_NAME = colDef(name = "Age Group"),
  #         OUTCOME_NAME = colDef(name = "Cohort Name"),
  #         PERSONS_AT_RISK = colDef(name = "Persons at Risk", format = colFormat(separators = TRUE)),
  #         PERSON_DAYS = colDef(name = "Person Days", format = colFormat(separators = TRUE)),
  #         PERSON_YEARS = colDef(name = "Person Years", format = colFormat(separators = TRUE)),
  #         OUTCOMES = colDef(name = "Outcome Count", format = colFormat(separators = TRUE)),
  #         INCIDENCE_PROPORTION_P100P = colDef(name = "Incidence Proportion (per 100p)", format = colFormat(digits = 2)),
  #         INCIDENCE_RATE_P1000PY = colDef(name = "Incidence Rate (per 1000yrs)", format = colFormat(digits = 2))
  #       ),
  #       filterable = TRUE,
  #       searchable = TRUE,
  #       outlined = TRUE,
  #       bordered = TRUE,
  #       striped = TRUE,
  #       defaultPageSize = 20
  #     )
  # )
  #
  # ## subset inci
  # subsetInci <- reactive({
  #   incTab %>%
  #     dplyr::filter(
  #       START_YEAR != "All",
  #       AGE_GROUP_NAME %in% input$ageGroupYrInci,
  #       databaseId %in% input$databaseNameYrInci,
  #       OUTCOME_NAME %in% input$cohortNameYrInci
  #     )
  # })
  #
  # ### Make yearly incidence plot
  # output$inciYearPlot <- renderPlot({
  #   subsetInci() %>%
  #     plotYearlyIncidence()
  # })


  # Incidence (IncidencePrevalence) -----------

  ## Box text
  output$clinicalOutcomesDescriptionInciPre1 <- renderText({
    incidenceDescription1
  })

  ## Box text
  output$clinicalOutcomesDescriptionInciPre2 <- renderText({
    incidenceDescription2
  })

  ## Incidence
  output$inciTab2 <- renderReactable(
    incTab2 %>%
      dplyr::filter(databaseName %in% input$databaseInci2,
                    incidenceYear %in% input$yearInci2,
                    ageGroup %in% input$ageInci2) %>%
      reactable(
        columns = list(
          databaseName = colDef(name = "Database Name"),
          incidenceYear = colDef(name = "Year"),
          ageGroup = colDef(name = "Age Group"),
          outcome_cohort_name = colDef(name = "Cohort Name"),
          n_persons = colDef(name = "Persons at Risk", format = colFormat(separators = TRUE)),
          person_days = colDef(name = "Person Days", format = colFormat(separators = TRUE)),
          person_years = colDef(name = "Person Years", format = colFormat(separators = TRUE)),
          n_events = colDef(name = "Outcome Count", format = colFormat(separators = TRUE)),
          incidenceProp_100_ps = colDef(name = "Proportion (per 100p)", format = colFormat(digits = 2)),
          incidence_1000_pys = colDef(name = "Rate (per 1000yrs)", format = colFormat(digits = 2))
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 20
      )
  )

  ## subset inci
  subsetInci2 <- reactive({
    incTab2 %>%
      dplyr::filter(
        incidenceYear != "All",
        ageGroup %in% input$ageInci2Plot,
        databaseName %in% input$databaseInciPlot2
      )
  })

  ### Make yearly incidence plot
  output$inciYearPlot2 <- renderPlot({
    subsetInci2() %>%
      plotYearlyIncidencePrevalence()
  })


  # Underlying Conditions -------------------

  ## Box text
  output$underlyingDescription <- renderText({
    underlyingDescription
  })

  ## Prevalence
  output$condPiTab <- renderReactable(
    condPi %>%
      dplyr::filter(databaseId %in% input$databaseNameCondPi,
                    cohortName %in% input$cohortNameCondPi,
                    covariateName %in% input$conditionNameCondPi,
                    timeWindow %in% input$timeWindowCondPi) %>%
      reactable(
        columns = list(
          databaseId = colDef(name = "Database Name"),
          timeWindow = colDef(name = "Time Window"),
          cohortName = colDef(name = "Cohort Name"),
          covariateName = colDef(name = "Condition Cohort Name"),
          count = colDef(name = "Person Count"),
          pct = colDef(name = "Percentage")
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 20
      )
  )

  # Treatment Patterns -------------

  ## Box text
  output$treatmentPatternsDescription <- renderText({
    treatmentPatternsDescription
  })

  output$drugUtilizationDescription <- renderText({
    drugUtilizationDescription
  })

  output$drugUtilizationDescription2 <- renderText({
    drugUtilizationDescription2
  })

  output$drugUtilizationDescription3 <- renderText({
    drugUtilizationDescription3
  })


  ## Utilization
  duPiRe <- reactive({
    drugPi %>%
      dplyr::filter(
        cat %in% input$drugCat,
        databaseId %in% input$databaseNameDuPi,
        cohortName %in% input$cohortNameDuPi,
        covariateName %in% input$drugNameDuPi,
        timeWindow %in% input$timeWindowDuPi
      )
  })

  output$duTab <- renderReactable(
    duPiRe() %>% reactable(
      columns = list(
        databaseId = colDef(name = "Database Name"),
        timeWindow = colDef(name = "Time Window"),
        cat = colDef(name = "Category"),
        cohortName = colDef(name = "Cohort Name"),
        covariateName = colDef(name = "Drug Cohort Name"),
        count = colDef(name = "Person Count", format = colFormat(separators = TRUE)),
        pct = colDef(name = "Percentage", format = colFormat(percent = TRUE, digits = 2))
      ),
      filterable = TRUE,
      searchable = TRUE,
      outlined = TRUE,
      bordered = TRUE,
      striped = TRUE,
      defaultPageSize = 20
    )
  )


  ## Sequences
  ### Reactive pickers

  sankeyPick <- reactive({
    sankeyCohorts %>%
      dplyr::filter(name == input$cohortNameSankey) %>%
      dplyr::pull(id)
  })

  trtSankeyPlot <- reactive({
    txPatDatAll %>%
      dplyr::filter(
          databaseId == input$databaseNameSankey,
          time == input$txTime,
          type == input$txType,
          cohortId == sankeyPick()
        ) %>%
      dplyr::slice(1:20) %>%
      buildSankeyData() %>%
      plotSankey()

  })


  trtSankeyTab <- reactive({
    txPatDatAll %>%
      dplyr::filter(
        databaseId == input$databaseNameSankey,
        time == input$txTime,
        type == input$txType,
        cohortId == sankeyPick()
      ) %>%
      dplyr::arrange(desc(n)) %>%
      tidyr::unite(
        col = "sequence",
        event_cohort_name1:event_cohort_name5,
        sep = " | ",
        na.rm = TRUE
      ) %>%
      dplyr::select(
        databaseId, cohortName, sequence, n
      )
  })


  ### Treatment Patterns sankey
  output$txSankey <- networkD3::renderSankeyNetwork({
    trtSankeyPlot()
  })


  ### Treatment Patterns table
  output$txPatDat <- renderReactable(
    trtSankeyTab() %>%
      reactable(
        columns = list(
          databaseId = colDef(name = "Database Name"),
          cohortName = colDef(name = "Cohort Name"),
          sequence = colDef(name = "Sequence"),
          n = colDef(name = "Person Count", format = colFormat(separators = TRUE))
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 10
      )
  )


  ### Time To Discontinuation (TTD)

  ## Pickers

  # Cohort name
  ttdPick <- reactive({
    tteCohorts %>%
      dplyr::filter(name == input$cohortNameTtd) %>%
      dplyr::pull(name)
  })

  # Cohort ID (with)
  ttdPickIdWith <- reactive({
    tteCohorts %>%
      dplyr::filter(name == input$cohortNameTtd2) %>%
      dplyr::pull(id)
  })

  # Cohort ID (without)
  ttdPickIdWithout <- reactive({
    tteCohorts %>%
      dplyr::filter(name == input$cohortNameTtd) %>%
      dplyr::pull(id)
  })


  # Drug Line (with)
  ttdPickLineWith <- reactive ({

    tolower(ttdLine[ttdLine %in% c(input$strataTtd2)])

  })

  # Drug Line (without)
  ttdPickLineWithout <- reactive ({

    tolower(ttdLine[ttdLine %in% c(input$strataTtd)])

  })

  # Survprob table subset (without)
  ttdSubset <- reactive({

    dt1 <- ttd %>%
      dplyr::filter(
        database == input$databaseNameTtd,
        `Cohort Name` == input$cohortNameTtd
      )

    if (input$strataTtd == "Single") {
      dt1 <- dt1 %>%
        dplyr::filter(
          !grepl("\\+", strata)
        )
    }

    dt1

  })

  # Survprob table subset (with)
  ttdSubset2 <- reactive({

    dt1 <- ttd2 %>%
      dplyr::filter(
        database == input$databaseNameTtd2,
        `Cohort Name` == input$cohortNameTtd2
      )

    if (input$strataTtd2 == "Single") {
      dt1 <- dt1 %>%
        dplyr::filter(
          !grepl("\\+", strata)
        )
    }

    dt1

  })


  ### KM Plot (With)
  ttdKMPlotWith <- reactive ({

    ttd_km <- glue::glue('shiny/data/plots/ttd/with/ttd_{ttdPickLineWith()}_{input$databaseNameTtd2}_{ttdPickIdWith()}.png')

    return(ttd_km)

  })

  output$ttd_km_with <- shiny::renderImage({

    filename <- normalizePath(file.path(here::here({ttdKMPlotWith()})))

    list(src = filename, width = "85%", height = "750px")

    }, deleteFile = FALSE
  )

  ### KM Plot (Without)
  ttdKMPlotWithout <- reactive ({

    ttd_km <- glue::glue('shiny/data/plots/ttd/wo/ttd_{ttdPickLineWithout()}_{input$databaseNameTtd}_{ttdPickIdWithout()}.png')

    return(ttd_km)

  })

  output$ttd_km_without <- shiny::renderImage({

    filename <- normalizePath(file.path(here::here({ttdKMPlotWithout()})))

    list(src = filename, width = "85%", height = "750px")

    }, deleteFile = FALSE
  )



  ## Observer
  # observe({print(ttdKMPlotWith())})
  # observe({print(ttdPickLine())})
  # observe({print(ttdPickId())})
  # observe({print(ttiKMPlot())})


  # ## Find rds file (Without)
  # ttdKMRds <- reactive ({
  #
  #   if (input$cohortNameTtd == "hmb") {
  #     lineKM <- "1"
  #   } else if (input$cohortNameTtd == "hmb age_lt_30"){
  #     lineKM <- "1001"
  #   } else if (input$cohortNameTtd == "hmb age_30_45") {
  #     lineKM <- "1002"
  #   } else if (input$cohortNameTtd == "hmb age_45_55") {
  #     lineKM <- "1003"
  #   }
  #
  #   ttd_km_name <- here::here(glue::glue('shiny/data/ttd/wo/tte_{input$databaseNameTtd}_{lineKM}.rds'))
  #
  #   ttd_km <- readr::read_rds(ttd_km_name)
  #
  #   ttd_km_final <- ttd_km[[glue::glue("{input$strataTtd}")]]
  #
  #   return(ttd_km_final)
  # })
  #
  #
  # output$ttdKmPlot <- renderPlot(
  #   res = 80,
  #   {
  #
  #   tte <- ttdKMRds()
  #
  #   ## Number of colors for lines
  #   colors <- colorspace::rainbow_hcl(unique(length(tte$strata)))
  #
  #   tte |>
  #     ggsurvfit(size = 1) +
  #     scale_ggsurvfit(x_scales=list(breaks=c(0.5, 0:3))) +  # Breaks at 6m and 1-3 years
  #     scale_color_manual(values = colors) +
  #     scale_fill_manual(values = colors) +
  #     add_risktable(risktable_stats = "{n.risk} ({cum.event})",
  #     #add_risktable(risktable_stats = "{n.risk} ({cum.censor})",
  #                   risktable_height = 0.4,
  #                   hjust = 0,
  #                   size = 4, # increase font size of risk table statistics
  #                   theme =   # increase font size of risk table title and y-axis label
  #                     list(
  #                       theme_risktable_default(axis.text.y.size = 11,
  #                                               plot.title.size = 11),
  #                       theme(plot.title = element_text(face = "bold"))
  #                     )) +
  #     theme(axis.text.x = element_text(hjust = 0)) +
  #     labs(x = "Follow-up time, years")
  #
  # })
  #
  # ## Find rds file (With)
  # ttdKMRds2 <- reactive ({
  #
  #   if (input$cohortNameTtd2 == "hmb") {
  #     lineKM <- "1"
  #   } else if (input$cohortNameTtd2 == "hmb age_lt_30"){
  #     lineKM <- "1001"
  #   } else if (input$cohortNameTtd2 == "hmb age_30_45") {
  #     lineKM <- "1002"
  #   } else if (input$cohortNameTtd2 == "hmb age_45_55") {
  #     lineKM <- "1003"
  #   }
  #
  #   ttd_km_name <- here::here(glue::glue('shiny/data/ttd/with/tte_{input$databaseNameTtd2}_{lineKM}.rds'))
  #
  #   ttd_km <- readr::read_rds(ttd_km_name)
  #
  #   ttd_km_final <- ttd_km[[glue::glue("{input$strataTtd2}")]]
  #
  #   return(ttd_km_final)
  # })
  #
  #
  # output$ttdKmPlot2 <- renderPlot(
  #   res = 80,
  #   {
  #
  #     tte <- ttdKMRds2()
  #
  #     ## Number of colors for lines
  #     colors <- colorspace::rainbow_hcl(unique(length(tte$strata)))
  #
  #     tte |>
  #       ggsurvfit(size = 1) +
  #       scale_ggsurvfit(x_scales=list(breaks=c(0.5, 0:3))) +  # Breaks at 6m and 1-3 years
  #       scale_color_manual(values = colors) +
  #       scale_fill_manual(values = colors) +
  #       add_risktable(risktable_stats = "{n.risk} ({cum.event})",
  #       #add_risktable(risktable_stats = "{n.risk} ({cum.censor})",
  #                     risktable_height = 0.4,
  #                     hjust = 0,
  #                     size = 4, # increase font size of risk table statistics
  #                     theme =   # increase font size of risk table title and y-axis label
  #                       list(
  #                         theme_risktable_default(axis.text.y.size = 11,
  #                                                 plot.title.size = 11),
  #                         theme(plot.title = element_text(face = "bold"))
  #                       )) +
  #       theme(axis.text.x = element_text(hjust = 0)) +
  #       labs(x = "Follow-up time, years")
  #
  #   })


  ### SurvProb table (Without)
  output$ttdSurvTab <- renderReactable(
    makeSurvProbTab(ttdSubset()) %>%
      reactable(
        columns = list(
          database = colDef(name = "Database Name"),
          `Cohort Name` = colDef(name = "Cohort Name"),
          strata = colDef(name = "Drug Cohort Name"),
          `6 month` = colDef(name = "6 month Survival", format = colFormat(digits = 1, percent = TRUE)),
          `1 year` = colDef(name = "1 year Survival", format = colFormat(digits = 1, percent = TRUE)),
          `2 year` = colDef(name = "2 year Survival", format = colFormat(digits = 1, percent = TRUE))
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 20
      )
  )

  ### SurvProb table (With)
  output$ttdSurvTab2 <- renderReactable(
    makeSurvProbTab(ttdSubset2()) %>%
      reactable(
        columns = list(
          database = colDef(name = "Database Name"),
          `Cohort Name` = colDef(name = "Cohort Name"),
          strata = colDef(name = "Drug Cohort Name"),
          `6 month` = colDef(name = "6 month Survival", format = colFormat(digits = 1, percent = TRUE)),
          `1 year` = colDef(name = "1 year Survival", format = colFormat(digits = 1, percent = TRUE)),
          `2 year` = colDef(name = "2 year Survival", format = colFormat(digits = 1, percent = TRUE))
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 20
      )
  )


  # Procedure Analysis ---------------------------------

  ## Box text
  output$procedureAnalysisDescription <- renderText({
    procedureAnalysisDescription
  })

  ## Procedure Prevalence
  procPiRe <- reactive({
    procPi %>%
      dplyr::filter(
        databaseId %in% input$databaseNameProcPi,
        cohortName %in% input$cohortNameProcPi,
        covariateName %in% input$procNameProcPi,
        timeWindow %in% input$timeWindowProcPi
      )
  })

  output$procTab <- renderReactable(
    procPiRe() %>% reactable(
      columns = list(
        databaseId = colDef(name = "Database Name"),
        timeWindow = colDef(name = "Time Window"),
        cohortName = colDef(name = "Cohort Name"),
        covariateName = colDef(name = "Procedure Cohort Name"),
        count = colDef(name = "Person Count", format = colFormat(separators = TRUE)),
        pct = colDef(name = "Percentage", format = colFormat(digits = 1, percent = TRUE))
      ),
      filterable = TRUE,
      searchable = TRUE,
      outlined = TRUE,
      bordered = TRUE,
      striped = TRUE,
      defaultPageSize = 20
    )
  )


  ### Time To Intervention (TTI)

  ## Pickers

  # Cohort Name
  ttiPick <- reactive({
    ttiCohorts %>%
      dplyr::filter(name == input$cohortNameTti) %>%
      dplyr::pull(name)
  })

  # Cohort ID
  ttiPickId <- reactive({
    ttiCohorts %>%
      dplyr::filter(name == input$cohortNameTti) %>%
      dplyr::pull(id)
  })


  # Survprob table subset (Database, Cohort)
  ttiSubset <- reactive({

    tti2 %>%
      dplyr::filter(
        database == input$databaseNameTti,
        `Cohort Name` == input$cohortNameTti
      )
  })


  # ## Find rds file
  # ttiKMRds <- reactive ({
  #
  #   if (input$cohortNameTti == "hmb") {
  #     lineKM <- "1"
  #   } else if (input$cohortNameTti == "hmb age_lt_30"){
  #     lineKM <- "1001"
  #   } else if (input$cohortNameTti == "hmb age_30_45") {
  #     lineKM <- "1002"
  #   } else if (input$cohortNameTti == "hmb age_45_55") {
  #     lineKM <- "1003"
  #   }
  #
  #   ttd_km_name <- here::here(glue::glue('shiny/data/tti/tti_{input$databaseNameTti}_{lineKM}.rds'))
  #
  #   ttd_km <- readr::read_rds(ttd_km_name)
  #
  #   return(ttd_km)
  # })
  #
  #
  # output$ttiKmPlot <- renderPlot(
  #   res = 80,
  #   {
  #
  #     tti <- ttiKMRds()
  #
  #     ## Number of colors for lines
  #     colors <- colorspace::rainbow_hcl(unique(length(tti$strata)))
  #
  #     tti |>
  #       ggsurvfit(size = 1) +
  #       scale_ggsurvfit(x_scales=list(breaks=c(0.5, 0:3))) +  # Breaks at 6m and 1-3 years
  #       scale_color_manual(values = colors) +
  #       scale_fill_manual(values = colors) +
  #       add_risktable(risktable_stats = "{n.risk} ({cum.event})",
  #       #add_risktable(risktable_stats = "{n.risk} ({cum.censor})",
  #                     risktable_height = 0.4,
  #                     hjust = 0,
  #                     size = 4, # increase font size of risk table statistics
  #                     theme =   # increase font size of risk table title and y-axis label
  #                       list(
  #                         theme_risktable_default(axis.text.y.size = 11,
  #                                                 plot.title.size = 11),
  #                         theme(plot.title = element_text(face = "bold"))
  #                       )) +
  #       theme(axis.text.x = element_text(hjust = 0)) +
  #       labs(x = "Follow-up time, years")
  #
  #   })


  ### KM Plot
  ttiKMPlot <- reactive ({

    tti_km <- glue::glue('shiny/data/plots/tti/tti_{input$databaseNameTti}_{ttiPickId()}.png')

    return(tti_km)

  })

  output$tti_km <- shiny::renderImage({

    filename <- normalizePath(file.path(here::here({ttiKMPlot()})))

    list(src = filename, width = "85%", height = "750px")

    }, deleteFile = FALSE

  )


  ### SurvProb table
  output$ttiSurvTab <- renderReactable(
    makeSurvProbTab2(ttiSubset()) %>%
      reactable(
        columns = list(
          database = colDef(name = "Database Name"),
          `Cohort Name` = colDef(name = "Cohort Name"),
          outcomeCohortId = colDef(name = "Procedure Cohort Name"),
          `6 month` = colDef(name = "6 month Survival", format = colFormat(digits = 1, percent = TRUE)),
          `1 year` = colDef(name = "1 year Survival", format = colFormat(digits = 1, percent = TRUE)),
          `2 year` = colDef(name = "2 year Survival", format = colFormat(digits = 1, percent = TRUE))
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 20
      )
  )


  session$onSessionEnded(stopApp)  # Kills the session when browser tab is closed
}


shinyApp(ui = ui, server = server)
