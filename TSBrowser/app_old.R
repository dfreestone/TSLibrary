library(shiny)
library(shinydashboard)

library(tidyverse)
library(TSExperiment)

#TODO(David):
#  - The current version is nicely modularized, but does not render correctly
#     Not sure why
#
#  - Seems like we should use this more like launch_stan whatever, that is, it
#      it takes in a dataframe (or can load one), and is only for viewing, once we
#      know exactly what experiment it is. Analysis is still done in RStudio
#

# UI Header
# ==============================================================================
NotificationsMenu = dropdownMenuOutput("NotificationMenu")
Header = dashboardHeader(title="TS Dashboard", titleWidth="92%", NotificationsMenu)

# UI Sidebar
# ==============================================================================
WelcomeMenu = menuItem("Welcome", tabName="Welcome", icon=icon("user-circle-o"))
CreateExperimentMenu = menuItem("Create Experiment", tabName="CreateExperiment", icon=icon("plus-square-o"))
BoxChecksMenu = menuItem("Box Checks", tabName="BoxChecks", icon=icon("check-square-o"))
AnalysisMenu = menuItem("Analysis", tabName="Analysis", icon=icon("bar-chart"))
MiscMenu = menuItem("Misc", tabName="Misc", icon=icon("list"))

# Create it
Sidebar = dashboardSidebar(width=150, WelcomeMenu, CreateExperimentMenu, BoxChecksMenu,
                           AnalysisMenu, MiscMenu)

# UI Body
# ==============================================================================

# Welcome page -----------------------------------------------------------------
Welcome_page = tabItem(tabName="Welcome", htmlOutput("WelcomeText"))

# Boxchecks page ---------------------------------------------------------------
BoxChecks_FoodAmountPanel = tabPanel("Food Amount", box(plotOutput("PlotFoodAmount", width="900px", height="900px")))
BoxChecks_DetectionLatencyPanel = tabPanel("Detection Latency", box(plotOutput("PlotDetectionLatency", width="900px", height="900px")))
BoxChecks_BlockedPelletsPanel =  tabPanel("Blocked Pellets", box(plotOutput("PlotBlockedPellets", width="900px", height="900px")))

# Pull the panels together in a box
BoxChecks_TabBox = tabBox(side="left", width="500px", height="1000px", id="tab_boxchecks",
                          BoxChecks_FoodAmountPanel, BoxChecks_DetectionLatencyPanel, BoxChecks_BlockedPelletsPanel)
# And render the page.
BoxChecks_page = tabItem(tabName="BoxChecks", BoxChecks_TabBox)

# Create Experiment page -------------------------------------------------------
Experiment_NameInput = textInput("ExperimentName",
                                 "Enter the experiment name (must be different than an existing active experiment)",
                                 placeholder="Consistent Weber")

Experiment_DropBoxPathInput = textInput("DropboxPath", "Path to Dropbox Folder", value=DropBoxPaths()$LocalActiveExperimentPath)
Experiment_ProtocolFileInput = textInput("ProtocolFile", "Path to Protocol File", placeholder=file.path("~", "Desktop", "h_protocol.mpc"))
Experiment_ConditionsFileInput = textInput("ConditionsFile", "Path to Conditions File", placeholder=file.path("~", "Desktop", "h_conditions.csv"))
Experiment_EventCodesFileInput = textInput("EventCodesFile", "Path to Eventcodes File", placeholder=file.path("~", "Desktop", "h_eventcodes.csv"))
Experiment_SameDirectoryCheckBox = checkboxInput("SameDirectoryCheckBox", "The conditions and eventcodes files are in the same directory", value=FALSE)
# Lump these into a box
Experiment_CriticalFilesBox = box(title="Paths to critical files", status="warning", solidHeader=TRUE,
                                  Experiment_NameInput, Experiment_DropBoxPathInput, Experiment_ProtocolFileInput,
                                  Experiment_SameDirectoryCheckBox, Experiment_ConditionsFileInput, Experiment_EventCodesFileInput)

# The progress / create box
Experiment_ProgressBox = valueBoxOutput("ProgressBox")
Experiment_DetailsText = verbatimTextOutput("ExperimentDetails")
Experiment_CreateButton = actionButton("SubmitCreate", "Create Experiment")
Experiment_CreateBox = box(title="Experiment Details", status="info", solidHeader=TRUE,
                           Experiment_ProgressBox, Experiment_DetailsText, Experiment_CreateButton)

# Push these into a tab panel
Experiment_TabPanel = tabPanel("Experiment", fluidRow(column(12, Experiment_CriticalFilesBox, Experiment_CreateBox)))

Experiment_ProtocolHelpPanel = tabPanel("Protocol", htmlOutput("ProtocolHelpText"))
Experiment_ConditionsHelpPanel = tabPanel("Conditions", htmlOutput("ConditionsHelpText"))
Experiment_EventCodesHelpPanel = tabPanel("Event codes", htmlOutput("EventCodesHelpText"))

# Tabbox to put everything together
Experiment_CreateTabPanel = tabBox(side="left", width="500px", height="1000px", id="tab_createexperiment",
                                   Experiment_TabPanel, Experiment_ProtocolHelpPanel, Experiment_ConditionsHelpPanel, Experiment_EventCodesHelpPanel)
# And create the page
Experiment_page =tabItem(tabName="CreateExperiment", fluidRow(column(12, Experiment_CreateTabPanel)))

# Analysis page ----------------------------------------------------------------
Analysis_ExperimentPathInput = textInput("ExperimentPath", "Path to experiment", placeholder=ExperimentPath(ActiveExperiments()))
Analysis_TrialDefinitionInput = textInput("TrialDefinition", "Trial definition", placeholder="Trial_start, long_trial, wait_vt")
Analysis_ProtocolValuesSelectInput = selectInput("ProtocolTypeForAnalysis", "Protocol type", choices=c("DRL", "Switch", "Peak"))
Analysis_VisualizationSelectInput = uiOutput("PlotTypeForProtocol")
Analysis_CreatePlotAnalysisButton = actionButton("CreatePlot", "Create Plot")

Analysis_ExperimentInfoRow = fluidRow(column(6, Analysis_ExperimentPathInput), column(6, Analysis_TrialDefinitionInput))
Analysis_VisualizationRow = fluidRow(column(3, selectInput("ProtocolTypeForAnalysis", "Protocol type", choices=c("DRL", "Switch", "Peak"))),
                                     column(3, uiOutput("PlotTypeForProtocol")),
                                     column(3, actionButton("CreatePlot", "Create Plot")))

Analysis_Box = box(title="Experiment information", status="info", solidHeader=TRUE, width="1000px",
                   Analysis_ExperimentInfoRow, Analysis_VisualizationRow)
Analysis_Plot = fluidRow(height="700px", column(12, plotOutput("AnalysisPlot")))

# And render the page
Analysis_page = tabItem(tabName="Analysis", Analysis_Box, Analysis_Plot)

# Connect all the pages.
Pages = tabItems(Welcome_page, Experiment_page, BoxChecks_page, Analysis_page)

# Now make the body
Body = dashboardBody(Pages) # show the welcome page to start.

# And render the UI
ui <- dashboardPage(Header, Sidebar, Body)


# TODO(David): Clean this up like the dashboard
server <- function(input, output, session){


  # Static Elements
  # ============================================================================

  # TODO(David): Add a notification when the user creates an experiment.
  output$NotificationMenu = renderMenu({
    msgs = apply(MessageData(), 1, function(row){
      notificationItem(text=row[["message"]], status=row[["status"]],
                       icon(row[["icon"]]))
    })

    dropdownMenu(type="notification", .list=msgs)
  })

  # Welcome Screen
  # ============================================================================
  output$WelcomeText = renderUI({HTML(readLines("www/WelcomeText.txt"))})

  # Box Checks
  # ============================================================================
  # Most recent data
  # TODO(David): Must figure out how to only load this if they click boxchecks
  data = reactive({
    if (RecentExperimentActivity()){
      df = ReadActiveExperimentFiles()
      return(list(data=df,
                  boxchecks=list(foodamt = CumulativeFoodAmount(df, TRUE),
                                 pelletlat = DetectionLatency(df, TRUE),
                                 blockedpellet = CumulativeBlockedDeliveries(df, TRUE))))
    } else{
      return(list(data=data_frame(),
                  boxchecks=list(foodamt=NULL, pelletlat=NULL, blockedpellet=NULL)))
    }
  })

  output$PlotFoodAmount = renderPlot({print(data()$boxchecks$foodamt)})
  output$PlotDetectionLatency = renderPlot({print(data()$boxchecks$pelletlat)})
  output$PlotBlockedPellets = renderPlot({print(data()$boxchecks$blockedpellet)})


  # Create Experiment
  # ============================================================================
  # TODO(David): Add examples directly into the rendered UI.
  output$ProtocolHelpText = renderUI({HTML(readLines("www/ProtocolHelpText.txt"))})
  output$ConditionsHelpText = renderUI({HTML(readLines("www/ConditionsHelpText.txt"))})
  output$EventCodesHelpText = renderUI({HTML(readLines("www/EventCodesHelpText.txt"))})

  output$ExperimentDetails = renderText({ExperimentDetailsMessage(progress()$progress,
                                                                  input$ExperimentName, input$ConditionsFile)})

  experimentwritten = reactive({
    progress = 100
    color = "green"
    icon = "thumbs-up"
    text = "Experiment Created"
    created = input$SubmitCreate>0 & ExperimentID(basename(input$ProtocolFile)) %in% ActiveExperiments()
    return(list(progress=progress, icon=icon, color=color, text=text, created=created))
  })

  progress = reactive({

      ValidExperimentName = FALSE
      if (input$ExperimentName != ""){
        ValidExperimentName = !any(agrep(input$ExperimentName, ExperimentName(ActiveExperiments()), ignore.case=TRUE))
      }
      info = c(file.exists(input$ProtocolFile),
               file.exists(input$ConditionsFile),
               file.exists(input$EventCodesFile),
               ValidExperimentName,
               !is.null(input$DropboxPath))
      progress = 100 * mean(info)

      if (progress==100){
        color = "orange"
        icon = "hand-spock-o"
        text = "Ready for Creation"
      } else {
        color = "red"
        icon = "thumbs-down"
        text = "Progress"
      }
    return(list(progress=progress, icon=icon, color=color, text=text, created=FALSE))
  })

  output$ProgressBox = renderValueBox({
    if (experimentwritten()$created){
      progress = experimentwritten()$progress
      text = experimentwritten()$text
      icon = experimentwritten()$icon
      color = experimentwritten()$color
    } else{
      progress = progress()$progress
      text = progress()$text
      icon = progress()$icon
      color = progress()$color
    }
    progress_str = paste0(progress, "%")
    valueBox(progress_str, text, icon=icon(icon), color=color)
  })


  observeEvent(input$SameDirectoryCheckBox, updateTextInput(session, "ConditionsFile",
                                                            value=ifelse((input$SameDirectoryCheckBox & file.exists(input$ProtocolFile)),
                                                                         Sys.glob(file.path(dirname(input$ProtocolFile), paste0(ExperimentID(basename(input$ProtocolFile)), "_conditions.csv"))),
                                                                         input$ConditionsFile)))
  observeEvent(input$SameDirectoryCheckBox, updateTextInput(session, "EventCodesFile",
                                                            value=ifelse((input$SameDirectoryCheckBox & file.exists(input$ProtocolFile)),
                                                                         Sys.glob(file.path(dirname(input$ProtocolFile), paste0(ExperimentID(basename(input$ProtocolFile)), "_eventcodes.csv"))),
                                                                         input$EventCodesFile)))
  observeEvent(input$SubmitCreate, {CreateActiveExperiment(input$ExperimentName, input$DropboxPath, input$ConditionsFile,
                                                           input$ProtocolFile, input$EventCodesFile)})

  # Analysis
  # ============================================================================
  protocol_selected = reactive({
    switch(input$ProtocolTypeForAnalysis,
           DRL = c("Scatter", "Distribution"),
           Switch = c("Raster", "Distribution"),
           Peak = c("Raster", "Distribution"))

  })

  output$PlotTypeForProtocol = renderUI({selectInput("RenderedPlotTypeForProtocol",
                                                     paste0("Visualization for ", input$ProtocolTypeForAnalysis),
                                                     choices=protocol_selected())})

  dataForAnalysis = eventReactive(input$CreatePlot, {data_frame(x=rnorm(100, mean=10, sd=3), y=rnorm(100, mean=5, sd=3))})
  output$AnalysisPlot = renderPlot({
    ax = ggplot(dataForAnalysis()) +
      geom_point(aes(x=x, y=y))
    print(ax)
    })
} # server

CreatePlot <- function(experimentpath, trialdefinition, protocoltype, plot_type){

}

ExperimentDetailsMessage <- function(progress, experimentName, conditionsFile){
  if (progress>=100){
    conditions = read_csv(conditionsFile, skip=1)
    sprintf(paste0("Name       : (%s) %s \n",
                   "Lab        : %s \n",
                   "Species    : %s \n",
                   "Strain     : %s \n",
                   "Sex        : %s \n",
                   "Supplier   : %s \n",
                   "No. Animals: %d \n",
                   "Weights?   : %s \n",
                   "Day Onset  : %s \n",
                   "Day Offset : %s \n"),
            ExperimentID(basename(conditionsFile)),
            experimentName,
            paste0(unique(conditions$Lab), collapse=", "),
            paste0(unique(conditions$Species), collapse=", "),
            paste0(unique(conditions$Strain), collapse=", "),
            paste0(unique(conditions$Sex), collapse=", "),
            paste0(unique(conditions$Supplier), collapse=", "),
            nrow(conditions),
            ifelse(all(!is.na(conditions$Arrival_Weights)), "Yes", "No"),
            paste0(unique(conditions$'D(9)')/(60*60), collapse=", "),
            paste0(unique(conditions$'D(19)')/(60*60)), collapse=", ")
  }
}

MessageData <- function(){
  df = data_frame()

  # Check for recent data.
  activity = RecentExperimentActivity()
  if (activity>0){
    df = bind_rows(df, data_frame(message=sprintf("Recent Data Activity (%d files)", activity), icon="bar-chart", status="primary"))
  } else {
    df = bind_rows(df, data_frame(message="No Recent Data Activity", icon="bar-chart", status="warning"))
  }

  # Other notification checks?
  return(df)
}


shinyApp(ui = ui, server = server)
