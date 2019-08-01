#---------------------------------------------------------------------------------
# Name - app.R
# Desc - Shiny App for
# Version - 1.0.0
# Author - Mickaël Canouil
# Source code -
#---------------------------------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")

library(shiny)
library(shinydashboard)

library(parallel)
library(ggplot2)
library(grid)
library(scales)
library(broom)
library(xlsx)
library(lme4)
library(tidyr)

ui <- dashboardPage(
  dashboardHeader(
    title = HTML("PRJCT"),
    dropdownMenuOutput("messageMenu")
  ),
  dashboardSidebar(
    sidebarUserPanel(
      name = a(tags$i(style = "color:#1995dc", icon("envelope", lib = "glyphicon")), "Mickaël Canouil", href = "mailto:mickael.canouil@cnrs.fr"),
      subtitle = tags$span(style = "color:#1995dc", "(Biostatistician)")
    ),
    hr(),
    sidebarMenu(
      tags$p(
        style = "text-align:center",
        actionButton(
          inputId = "reset",
          label = HTML(paste(
            icon("warning-sign", lib = "glyphicon"),
            "Reset Database",
            icon("warning-sign", lib = "glyphicon")
          )),
          width = "85%",
          style = "color:#EE2C2C"
        )
      ),
      hr(),
      menuItem(
        text = "Base Tab",
        tabName = "BaseTab",
        icon = tags$i(style = "color:#1995dc", icon("import", lib = "glyphicon"))
      ),
      hr(),
      HTML(
        '<li class="false">
          <a aria-expanded="true" data-toggle="tab" data-value="Slider">
            <i style="color:#1995dc"><i class="glyphicon glyphicon-cog"></i></i>
            <span>Slider</span>
          </a>
       </li>'
      ),
      sliderInput(
        inputId = "Slider",
        label = NULL,
        min = 1,
        max = 3,
        value = 2,
        step = 0.25
      ),
      hr(),
      menuItem(
        text = "Quality Control",
        menuSubItem(
          text = "First Tab",
          tabName = "FirstTab"
        ),
        menuSubItem(
          text = "Second Tab",
          tabName = "SecondTab"
        ),
        menuSubItem(
          text = "Summary Tab",
          tabName = "SummaryTab"
        ),
        icon = tags$i(style = "color:#1995dc", icon("search", lib = "glyphicon"))
      ),
      menuItem(
        text = "Analysis",
        menuSubItem(
          text = "First Analysis",
          tabName = "FirstAnalysis"
        ),
        menuSubItem(
          text = "Second Analysis",
          tabName = "SecondAnalysis"
        ),
        icon = tags$i(style = "color:#1995dc", icon("tasks", lib = "glyphicon"))
      ),
      hr()
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/theme.css")),
    tabItems(
      tabItem(
        tabName = "BaseTab",
        fluidRow(
          valueBoxOutput("messageBox"),
          valueBoxOutput("progressBox"),
          valueBoxOutput("clearBox")
        ),
        fluidRow(
          box(
            width = 12,
            collapsible = FALSE,
            title = "Box",
            solidHeader = TRUE,
            status = "info"
          )
        ),
        fluidRow(
          box(
            width = 12,
            collapsible = FALSE,
            title = "Box",
            solidHeader = TRUE,
            status = "primary"
          )
        )
      ),
      tabItem(
        tabName = "FirstTab",
        fluidRow(
          box(
            width = 6,
            collapsible = FALSE,
            title = "Box",
            solidHeader = TRUE,
            status = "primary"
          ),
          box(
            width = 6,
            collapsible = FALSE,
            title = "Box",
            solidHeader = TRUE,
            status = "primary"
          )
        )
      ),
      tabItem(
        tabName = "SecondTab",
        fluidRow(
          box(
            width = 6,
            collapsible = FALSE,
            title = "Box",
            solidHeader = TRUE,
            status = "primary"
          ),
          box(
            width = 6,
            collapsible = FALSE,
            title = "Box",
            solidHeader = TRUE,
            status = "primary"
          )
        ),
        fluidRow(
          box(
            width = 12,
            collapsible = FALSE,
            title = "Box",
            solidHeader = TRUE,
            status = "primary"
          )
        )
      ),
      tabItem(
        tabName = "SummaryTab",
        fluidRow(
          box(
            width = 12,
            collapsible = FALSE,
            title = "Box",
            solidHeader = TRUE,
            status = "primary"
          )
        )
      ),
      tabItem(
        tabName = "FirstAnalysis",
        fluidRow(
          box(
            width = 12,
            collapsible = FALSE,
            title = "Box",
            solidHeader = TRUE,
            status = "info"
          )
        ),
        fluidRow(
          box(
            width = 6,
            collapsible = FALSE,
            title = "Box",
            solidHeader = TRUE,
            status = "primary"
          ),
          box(
            width = 6,
            collapsible = FALSE,
            title = "Box",
            solidHeader = TRUE,
            status = "primary"
          )
        )
      ),
      tabItem(
        tabName = "SecondAnalysis",
        fluidRow(
          box(
            "",
            width = 12,
            collapsible = FALSE,
            title = "Box",
            solidHeader = TRUE,
            status = "info"
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$reset, {
    # file.remove("")
  })

  # output$ <- renderDataTable({
  # withProgress(message = "Loading...", value = NULL, {
  # })
  # }, options = list(searching = FALSE))

  output$messageMenu <- renderMenu({
    dropdownMenu(
      type = "notifications",
      notificationItem(
        text = paste("Data computed on", format(Sys.Date(), "%Y-%d-%m"), "!"),
        icon = icon("exclamation-sign", lib = "glyphicon"),
        status = "info"
      )
    )
  })

  output$messageBox <- renderValueBox({
    valueBox(
      value = 0,
      subtitle = "Message",
      icon = icon("edit", lib = "glyphicon"),
      color = "blue"
    )
  })

  output$progressBox <- renderValueBox({
    valueBox(
      value = 0,
      subtitle = "Progess",
      icon = icon("share", lib = "glyphicon"),
      color = "maroon"
    )
  })

  computeQC <- reactive({
    withProgress(message = "Loading...", value = NULL, {
    })
  })
  output$QC_plot1 <- renderPlot({
  })

  output$clearBox <- renderValueBox({
    valueBox(
      value = 0,
      subtitle = "Clear",
      icon = icon("check", lib = "glyphicon"),
      color = "green"
    )
  })
}

shinyApp(ui, server)
