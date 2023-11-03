# Client Shiny App Script

# Author: George Argyriou + Martin Lavallee
# Date 08/17/2023


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
options(shiny.fullstacktrace = FALSE)


# Variables ---------------------
title <- "EHDEN HMB"

description <- "The EHDEN HMB study......"
incidenceDescription <- "Incidence rate is calculated by 'Outcome Count'/'Person Days' * 100."
underlyingDescription <- "Counts equal to 5 and below have been masked and replaced with '<5'."
drugUtilizationDescription <- "Drug utilization counts equal to 5 and below have been masked and replaced with '<5."
treatmentPatternsDescription <- "Treatment Patterns counts (Sequences) are restricted to 30"
clinicalCharacteristicsDescription <- "Counts equal to 5 and below have been masked and replaced with '<5'."
procedureAnalysisDescription <- "Counts equal to 5 and below have been masked and replaced with '<5'."

dashboardVersion <- "0.0.3"
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
                              #add odysseus logo
                              img(
                                src = 'odysseus_logo.png',
                                title = "title",
                                height = "50px",
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
    menuItem("Incidence", tabName = "inci", icon = shiny::icon("vial", lib = "font-awesome")),
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
          textOutput("studyDescription")
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
          reactableOutput("databaseInformation")
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
          #title = "Cohort Counts",
          id = "cohortCounts",
          width = 12,
          tabPanel("Cohort Counts",
                   fluidRow(
                     box(
                       status = "success",
                       column(width = 12,
                              pickerInput(
                                inputId = "databaseNameCohort",
                                label = "Database",
                                choices = databaseName,
                                selected = databaseName[1],
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
          tabPanel("Strata Counts",
                   fluidRow(
                     box(
                       status = "success",
                       column(width = 12,
                              pickerInput(
                                inputId = "databaseNameStrata",
                                label = "Database",
                                choices = databaseName,
                                selected = databaseName[1],
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
                                label = "Database",
                                choices = databaseName,
                                selected = databaseName[1],
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "cohortNameDemo",
                                label = "Cohort Name",
                                choices = cohortName,
                                selected = cohortName[1],
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
                                label = "Database",
                                choices = databaseName,
                                selected = databaseName[1],
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "cohortNameCts",
                                label = "Cohort Name",
                                choices = cohortName,
                                selected = cohortName[1],
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
                                label = "Database",
                                choices = databaseName,
                                selected = databaseName[1],
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "cohortNameConcept",
                                label = "Cohort Name",
                                choices = cohortName,
                                selected = cohortName[1],
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
                                label = "Database",
                                choices = databaseName,
                                selected = databaseName[1],
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "cohortNameCohortCov",
                                label = "Cohort Name",
                                choices = cohortName,
                                selected = cohortName[1],
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              )
                       ),
                       column(width = 6,
                              pickerInput(
                                inputId = "domainCohortCov",
                                label = "Domain",
                                choices = domainConceptChar[1:2],
                                selected = domainConceptChar[1:2],
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
                                label = "Database",
                                choices = databaseName,
                                selected = databaseName[1],
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),

                              pickerInput(
                                inputId = "cohortNameIcd",
                                label = "Cohort Name",
                                choices = cohortName,
                                selected = cohortName[1],
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
                   )

        )
      )
    ),


    ### Incidence Tab -----------------
    tabItem(
      tabName = "inci",

      fluidRow(
        box(
          collapsible = T,
          collapsed = F,
          title = "Incidence",
          width = 12,
          background = "light-blue",
          textOutput("clinicalOutcomesDescription")
        )
      ),

      fluidRow(
        tabBox(
          id = "baselineChar",
          width = 12,

          tabPanel(
            "Table",
            fluidRow(
              box(
                status = "success",
                column(width = 6,

                       # pick database
                       pickerInput(
                         inputId = "databaseNameInci",
                         label = "Database",
                         choices = databaseName,
                         selected = databaseName,
                         options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                         multiple = TRUE
                       ),

                       # pick cohortName
                       pickerInput(
                         inputId = "cohortNameInci",
                         label = "Cohort Name",
                         choices = cohortName,
                         selected = cohortName[1],
                         options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                         multiple = TRUE
                       )
                       #,


                ),
                column(width = 6,

                       # pick year
                       pickerInput(
                         inputId = "yearInci",
                         label = "Year",
                         choices = yearInci,
                         selected = yearInci[1],
                         options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                         multiple = TRUE
                       )
                      )
              )
            ),
            fluidRow(
              box(
                width = 12,
                reactableOutput("inciTab"),
                csvDownloadButton("inciTab", filename = "incidence.csv")
              )
            )
          ),
          tabPanel("Yearly Trend",

                   fluidRow(
                     column(width = 6,

                     box(
                       #width = 12,
                       status = "success",

                       pickerInput(
                         inputId = "databaseNameYrInci",
                         label = "Database",
                         choices = databaseName,
                         selected = databaseName,
                         options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                         multiple = TRUE
                       ),

                       pickerInput(
                         inputId = "cohortNameYrInci",
                         label = "Cohort Name",
                         choices = cohortName,
                         selected = cohortName,
                         options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                         multiple = TRUE
                       )
                      )
                     )
                   ),

                   fluidRow(

                            box(
                              width = 12,
                              plotOutput("inciYearPlot")
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
                 # pick database
                 pickerInput(
                   inputId = "databaseNameCondPi",
                   label = "Database",
                   choices = databaseName,
                   selected = databaseName,
                   options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                   multiple = TRUE
                 ),
                 # pick cohort
                 pickerInput(
                   inputId = "cohortNameCondPi",
                   label = "Cohort Name",
                   choices = cohortName,
                   selected = cohortName[1],
                   options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                   multiple = TRUE
                 )

          ),
          column(width = 6,
            # pick outcome
            pickerInput(
              inputId = "conditionNameCondPi",
              label = "Condition Name",
              choices = condCohorts,
              selected = condCohorts,
              options = shinyWidgets::pickerOptions(actionsBox = TRUE),
              multiple = TRUE
            ),

            # pick year
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
                       column(width = 6,
                        pickerInput(
                         inputId = "databaseNameDuPi",
                         label = "Database",
                         choices = databaseName,
                         selected = databaseName,
                         options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = "cohortNameDuPi",
                         label = "Cohort Name",
                         choices = cohortName,
                         selected = cohortName[1],
                         options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                         multiple = TRUE
                       )
                     ),
                     column(width = 6,
                            pickerInput(
                              inputId = "drugNameDuPi",
                              label = "Drug Name",
                              choices = drugCohorts,
                              selected = drugCohorts,
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
                       pickerInput(
                         inputId = "databaseNameSankey",
                         label = "Database",
                         choices = databaseName
                       ),
                       pickerInput(
                         inputId = "cohortNameSankey",
                         label = "Cohort Name",
                         choices = cohortName2
                       )
                     )
                   ),
                   fluidRow(
                     box(width = 12 ,
                     sankeyNetworkOutput("txSankey",
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
          ### TTD Panel
          tabPanel("Time to Discontinuation",
                   fluidRow(
                     column(width = 9,
                     #box(
                       status = "success",
                       plotOutput("ttdKmPlot")
                      #),
                     ),
                     column(width = 3,
                     box(
                       width = 9,
                       height = "230px",
                       background = "light-blue",
                       pickerInput(
                         inputId = "databaseNameTtd",
                         label = "Database",
                         choices = databaseName
                       ),
                       pickerInput(
                         inputId = "cohortNameTtd",
                         label = "Cohort Name",
                         choices = cohortName
                       ),
                       pickerInput(
                         inputId = "strataTtd",
                         label = "Drugs",
                         choices = c("Single", "All")
                       )
                      )
                     ),
                   ),
                   fluidRow(
                     box(
                       width = 12,
                       reactableOutput("ttdSurvTab")
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
                                label = "Database",
                                choices = databaseName,
                                selected = databaseName,
                                options = shinyWidgets::pickerOptions(actionsBox = TRUE),
                                multiple = TRUE
                              ),
                              pickerInput(
                                inputId = "cohortNameProcPi",
                                label = "Cohort Name",
                                choices = cohortName,
                                selected = cohortName[1],
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
          tabPanel("Time to Intervention",
                   fluidRow(
                     column(width = 9,
                     #box(
                       status = "success",
                       plotOutput("ttiKmPlot")
                      #)
                     ),
                     column(width = 3,
                     box(
                       width = 9,
                       height = "180px",
                       background = "light-blue",
                       pickerInput(
                         inputId = "databaseNameTti",
                         label = "Database",
                         choices = databaseName
                       ),
                       pickerInput(
                         inputId = "cohortNameTti",
                         label = "Cohort Name",
                         choices = cohortName
                       )
                      )
                     ),
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

  ## Database Information
  # output$databaseInformation <- renderReactable(
  #   databaseMeta %>% reactable()
  # )


  # Cohorts ----------------

  ## Cohort Counts
  output$cohortCountsTab <- renderReactable(
    cohortCounts %>%
      dplyr::filter(Database %in% input$databaseNameCohort) %>%
      reactable(
        columns = list(Subjects = colDef(name = "Subjects", format = colFormat(separators = TRUE)),
                       Entries = colDef(name = "Entries", format = colFormat(separators = TRUE))
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
      dplyr::filter(Database %in% input$databaseNameStrata) %>%
      reactable(
        columns = list(Subjects = colDef(name = "Subjects", format = colFormat(separators = TRUE))),
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
          databaseId = colDef(name = "Database"),
          cohortDefinitionId = colDef(name = "Cohort Id"),
          cohortName = colDef(name = "Cohort Name"),
          id = colDef(name = "Covariate Id"),
          Covariate = colDef(name = "Covariate Name"),
          count = colDef(name = "Count"),
          pct = colDef(name = "Percentage")
          # n = colDef(name = "Count", format = colFormat(separators = TRUE)),
          # pct = colDef(name = "Percentage", format = colFormat(percent = TRUE, digits = 2))
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
          databaseId = colDef(name = "Database"),
          cohortDefinitionId = colDef(name = "Cohort Id"),
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
          databaseId = colDef(name = "Database"),
          domain = colDef(name = "Domain"),
          cohortDefinitionId = colDef(name = "Cohort Id"),
          cohortName = colDef(name = "Cohort Name"),
          conceptId = colDef(name = "Concept Id"),
          name = colDef(name = "Concept Name"),
          count = colDef(name = "Count"),
          pct = colDef(name = "Percentage")
          # n = colDef(name = "Count", format = colFormat(separators = TRUE)),
          # pct = colDef(name = "Percentage", format = colFormat(percent = TRUE, digits = 2))
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
          databaseId = colDef(name = "Database"),
          timeWindow = colDef(name = "Time Window"),
          cohortId = colDef(name = "Cohort Id"),
          cohortName = colDef(name = "Cohort Name"),
          domain = colDef(name = "Domain"),
          covariateId = colDef(name = "Covariate Id"),
          covariateName = colDef(name = "Covariate Name"),
          count = colDef(name = "Count"),
          pct = colDef(name = "Percentage")
          # count = colDef(name = "Count", format = colFormat(separators = TRUE)),
          # pct = colDef(name = "Percentage", format = colFormat(percent = TRUE, digits = 2))
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
          databaseId = colDef(name = "Database"),
          COHORT_ID = colDef(name = "Cohort Id"),
          cohortName = colDef(name = "Cohort Name"),
          CATEGORY_CODE = colDef(name = "Concept Id"),
          categoryName = colDef(name = "ICD10 Chapter"),
          count = colDef(name = "Count"),
          pct = colDef(name = "Percentage")
          # count = colDef(name = "Count", format = colFormat(separators = TRUE)),
          # pct = colDef(name = "Percentage", format = colFormat(percent = TRUE, digits = 2))
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 20
      )
  )


  # Incidence -----------

  ## Box text
  output$clinicalOutcomesDescription <- renderText({
    incidenceDescription
  })



  ## Incidence
  output$inciTab <- renderReactable(
    incTab %>%
      dplyr::filter(databaseId %in% input$databaseNameInci,
                    START_YEAR %in% input$yearInci,
                    OUTCOME_NAME %in% snakecase::to_snake_case(input$cohortNameInci)) %>%
      reactable(
        columns = list(
          databaseId = colDef(name = "Database"),
          START_YEAR = colDef(name = "Year"),
          OUTCOME_COHORT_DEFINITION_ID = colDef(name = "Cohort Id"),
          OUTCOME_NAME = colDef(name = "Cohort Name"),
          PERSONS_AT_RISK = colDef(name = "Persons at Risk", format = colFormat(separators = TRUE)),
          PERSON_DAYS = colDef(name = "Person Days", format = colFormat(separators = TRUE)),
          OUTCOMES = colDef(name = "Outcome Count", format = colFormat(separators = TRUE)),
          INCIDENCE_PROPORTION_P100P = colDef(name = "Incidence Proportion (per 100p)", format = colFormat(digits = 2)),
          INCIDENCE_RATE_P1000PY = colDef(name = "Incidence Rate (per 1000yrs)", format = colFormat(digits = 2))
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
  subsetInci <- reactive({
    incTab %>%
      dplyr::filter(
        START_YEAR != "All",
        databaseId %in% input$databaseNameYrInci,
        OUTCOME_NAME %in% snakecase::to_snake_case(input$cohortNameYrInci)
      )
  })

  ### Make yearly incidence plot
  output$inciYearPlot <- renderPlot({
    subsetInci() %>%
      plotYearlyIncidence()
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
          databaseId = colDef(name = "Database"),
          timeWindow = colDef(name = "Time Window"),
          cohortId = colDef(name = "Cohort Id"),
          cohortName = colDef(name = "Cohort Name"),
          covariateId = colDef(name = "Condition Id"),
          covariateName = colDef(name = "Condition Name"),
          count = colDef(name = "Count"),
          pct = colDef(name = "Percentage")
          # count = colDef(name = "Count", format = colFormat(separators = TRUE)),
          # pct = colDef(name = "Percentage", format = colFormat(percent = TRUE, digits = 2))
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


  ## Utilization
  duPiRe <- reactive({
    drugPi %>%
      dplyr::filter(
        databaseId %in% input$databaseNameDuPi,
        cohortName %in% input$cohortNameDuPi,
        covariateName %in% input$drugNameDuPi,
        timeWindow %in% input$timeWindowDuPi
      )
  })

  output$duTab <- renderReactable(
    duPiRe() %>% reactable(
      columns = list(
        databaseId = colDef(name = "Database"),
        timeWindow = colDef(name = "Time Window"),
        cohortId = colDef(name = "Cohort Id"),
        cohortName = colDef(name = "Cohort Name"),
        covariateId = colDef(name = "Drug Id"),
        covariateName = colDef(name = "Drug Name"),
        count = colDef(name = "Count", format = colFormat(separators = TRUE)),
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
        cohortId == sankeyPick()) %>%
      dplyr::slice(1:20) %>%
      buildSankeyData() %>%
      plotSankey()

  })

  trtSankeyTab <- reactive({
    txPatDatAll %>%
      dplyr::filter(
        databaseId == input$databaseNameSankey,
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
          databaseId = colDef(name = "Database"),
          cohortName = colDef(name = "Cohort Name"),
          sequence = colDef(name = "Sequence"),
          n = colDef(name = "Count", format = colFormat(separators = TRUE))
        ),
        filterable = TRUE,
        searchable = TRUE,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        defaultPageSize = 10
      )
  )

  ## Time To discontinuation

  # convert cohort name to an id
  ttdPick <- reactive({
    tteCohorts %>%
      dplyr::filter(name == input$cohortNameTtd) %>%
      dplyr::pull(id)
  })

  # subset to those with strata
  ttdSubset <- reactive({

    dt1 <- ttd %>%
      dplyr::filter(
        database == input$databaseNameTtd,
        targetId == ttdPick()
      )
    if (input$strataTtd == "Single") {
      dt1 <- dt1 %>%
        dplyr::filter(
          !grepl("\\+", strata)
        )
    }

    dt1

  })
  # make km plot
  output$ttdKmPlot <- renderPlot({
    plotKM(ttdSubset())
  })

  ## get survProb table
  output$ttdSurvTab <- renderReactable(
    makeSurvProbTab(ttdSubset()) %>%
      reactable(
        columns = list(
          database = colDef(name = "Database"),
          targetId = colDef(name = "Target Cohort"),
          strata = colDef(name = "Drug Cohort"),
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
        databaseId = colDef(name = "Database"),
        timeWindow = colDef(name = "Time Window"),
        cohortId = colDef(name = "Target Cohort Id"),
        cohortName = colDef(name = "Target Cohort Name"),
        covariateId = colDef(name = "Procedure Id"),
        covariateName = colDef(name = "Procedure Name"),
        count = colDef(name = "Count", format = colFormat(separators = TRUE)),
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


  ## Procedure Time To intervention

  # convert cohort name to an id
  ttiPick <- reactive({
    tteCohorts %>%
      dplyr::filter(name == input$cohortNameTti) %>%
      dplyr::pull(id)
  })

  # subset to those with strata
  ttiSubset <- reactive({

    tti %>%
      dplyr::filter(
        database == input$databaseNameTti,
        targetId == ttiPick()
      )
  })
  # make km plot
  output$ttiKmPlot <- renderPlot({
    plotKM2(ttiSubset())
  })

  ## get survProb table
  output$ttiSurvTab <- renderReactable(
    makeSurvProbTab2(ttiSubset()) %>%
      reactable(
        columns = list(
          database = colDef(name = "Database"),
          targetId = colDef(name = "Target Cohort"),
          outcomeCohortId = colDef(name = "Procedure Cohort"),
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

