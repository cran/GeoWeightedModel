library(beepr)
library(cartography)
library(dplyr)
library(DT)
library(GWmodel)
library(raster)
library(readxl)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinybusy)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(sp)
data(USelect)
################################################################################
header <- shinydashboard:: dashboardHeader(
  title = "GeoWeightedModels",
  titleWidth = 320,
  tags$li(class = "dropdown",
          actionLink("stop_radiant",
                     "Stop",
                     icon = icon("power-off"),
                     onclick = "setTimeout(function(){window.close();}, 100); "
                     )
          )
  )
################################################################################
sidebar <- shinydashboard::dashboardSidebar(
  width = 320,
 shinydashboard::sidebarMenu(
    shinydashboard::menuItem("About ",
             tabName = "tab0"
             ),
    shinydashboard::menuItem("Load data",
             tabName = "tab1",
             icon = icon("file-import")
             ),

    shinydashboard::menuItem(div(tags$img(src = "distance.png",
                          width="20px",
                          height="20px"),
                 "Distance matrix"),
             tabName = "tab2"
    ),
    shinydashboard::menuItem("Bandwidth selection",
             tabName = "tab3",
             icon = icon("wifi")
    ),
    shinydashboard::menuItem("Spatial autocorrelation",
                             tabName = "tab6",
                             icon = icon("chart-bar")
    ),
    shinydashboard:: menuItem("Geographically Weighted Summary Statistics",
             tabName = "tab4",
             icon = icon("wpexplorer")
    ),
    shinydashboard::menuItem("Models",
             tabName = "tab5",
             icon = icon("globe-americas"),
             shinydashboard::menuSubItem("GW Regression",
                         tabName = "tab52"
                        # icon = tags$img(src = "normal1.png",
                                        # width="30px")
                         ),
             shinydashboard:: menuSubItem("GW Principal Component Analysis",
                         tabName = "tab53"
                         #icon = icon("chart-bar")
                         ),
             shinydashboard::menuSubItem("GW Discriminant Analysis",
                         tabName = "tab54"
                         #icon = icon("chart-bar")
                         )
             )
    )
  )
################################################################################
body <- shinydashboard::dashboardBody(shinybusy::add_busy_spinner(spin = "pixel",
                                                       height = "100px",
                                                       width = "100px",
                                                       color = "blue",
                                                       position ="top-right"),
tags$head(tags$style(
HTML('/* logo when hovered */.skin-blue .main-header .logo:hover {
                                background-color: #7da2d1;
}
* { font-family: Garamond; }
/* navbar (rest of the header) */.skin-blue .main-header .navbar {
background-color: #7da2d1;
}
/* main sidebar */.skin-blue .main-sidebar { background-color:#7da2d1;}
/* active selected tab in the sidebarmenu
*/.skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
background-color: #0091ff;
}
/* other links in the sidebarmenu */
.skin-blue .main-sidebar .sidebar .sidebar-menu a{ background-color: #ccdceb;
color: #000000;}
/* other links in the sidebarmenu when hovered */
.skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
background-color: #69c3ff;}
/* toggle button when hovered  */
.skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #9bc2e8;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #7dcdd1;
                                }'
)
                                        )
                                      ),
  shinydashboard::tabItems(
    shinydashboard::tabItem(
      tabName = "tab0",
      fluidRow(
        shinydashboard::tabBox(id = "tabset1",
               height = 400,
               width = 12,
               tabPanel(
                 " ",
                 h2(em(strong(
                   "GeoWeightedModels:
                   An R package for Geographically Weighted Models")
                 ),
                 align = "center"),
                 br(),
                 h4("Javier De La Hoz-M, María José Fernández Gómez
                    & Susana Mendes",
                    align="center"),
                 br(),
                 div(img(src = "icono.png",
                         height = 400,
                         width = 400),
                     style="text-align: center;"),
                 br(),
                 tags$head(
                   tags$style("h4 {font-family:Garamond}")
                 ),
                 tags$h4("GeoWeightedModels is an application developed in
                         Shiny to carry out some
                         models of a particular branch of spatial statistics,
                         named Geographically Weighted Models.
                         Includes functionsfor Exploratory Spatial Data
                         Analysis,various forms of Geographically
                         Weighted Regression,
                         Geographically Weighted Principal Component Analysis,
                         and Geographically Weighted Discriminant Analysis",
                         align="left"),


               )
        )
      )
      ),
    shinydashboard::tabItem(tabName = "tab1",
                            fluidRow(column(width = 12,shinydashboard::box(width = 12,
                               title = "Load data ",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                sidebarPanel(
                                  shinyWidgets::actionBttn("helpload",
                                             "Help",
                                             icon = icon("question-circle"),
                                             style = "stretch",
                                             block = FALSE,
                                             color = "primary"
                                ),
                                shinyBS::bsModal(id="helploaddata",
                                                   title = "",
                                                   trigger = "helpload",
                                                   size="large",
                                                   tags$iframe(
                                                     src = "Upload-data.html",
                                                     width = "100%",
                                                     height = "1000px",
                                                     frameborder = 0,
                                                     scrolling = "auto"
                                                   )
                                ),
                                br(), br(),
                                shinyWidgets::prettyCheckbox(
                                  inputId = "example",
                                  label = "Use example data set?",
                                  value = FALSE,
                                  status = "success"
                                ), br(),
                                  fileInput("file1",
                                            "Upload the file",
                                            accept = c(".xlsx", ".xls"),
                                            multiple = FALSE),
                                  selectInput(inputId = "worksheet",
                                              label="Worksheet Name",
                                              choices=NULL
                                  ),actionButton(inputId = "getData",
                                                 label="Get data"),
                                  selectizeInput('colID',
                                                 'Select ID for merge data',
                                                 choices = NULL,
                                                 multiple = FALSE)
                                ),mainPanel(DT::DTOutput("table1"),
                                            DT::DTOutput("example")

                                ))),
                               column(width = 12,  shinydashboard::box(width = 12,
                                title = "Load shapefiles ",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                sidebarPanel(fileInput("filemap",
                                                       "Upload map (shapefile)",
                                                       accept=c('.shp',
                                                                '.dbf','.prj',
                                                                '.shx','.xml'),
                                                       multiple = TRUE),
                                             br(),
                                             actionButton(
                                               inputId = "getshape",
                                               label="Get shapefiles")
                                             ),
                                mainPanel(verbatimTextOutput("shpi")
                                ))))

                            ),

    shinydashboard::tabItem(
      tabName = "tab2",
      shinydashboard::box(
        width = 12,
        title = "Distance matrix",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        sidebarPanel(
          shinyWidgets::actionBttn("helpdMat",
                                   "Help",
                                   icon = icon("question-circle"),
                                   style = "stretch",
                                   block = FALSE,
                                   color = "primary"
          ),
          shinyBS::bsModal(id="helpdistmat",
                           title = "",
                           trigger = "helpdMat",
                           size="large",
                           tags$iframe(
                             src = "Distance_Matrix.html",
                             width = "100%",
                             height = "1000px",
                             frameborder = 0,
                             scrolling = "auto"
                           )
          ),
          numericInput('focus',
                           'Focus',
                           0,
                           min = 1,
                           max = Inf),
          numericInput('power',
                       'Power (Minkowski distance)',
                       2,
                       min = 1,
                       max = Inf),

              sliderInput("theta",
                          "Theta (Angle in radians)",
                          min = 0,
                          max = 2,
                          value = 0,
                          step = 0.05),
          shinyWidgets::switchInput("longlat",
                                        inputId = "longlat",
                                        onLabel = "TRUE",
                                        offLabel = "FALSE",
                                        size = "mini")
              ,
          shinyalert::useShinyalert(),
          shinyjs::useShinyjs(),
          shinyWidgets::actionBttn(
                        inputId = "Run1", # run dMat
                        label = "Run",
                        style = "float",
                        block = TRUE,
                        color = "danger"
                      )
          ),
        mainPanel(helpText("Distance matrix"),

          DT::DTOutput("distmatrix")))
      ),
    shinydashboard::tabItem(
      tabName = "tab3",fluidRow(column(width = 12,
      shinydashboard:: box(width = 12,
          title = "Bandwidth selection",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          sidebarPanel(
            shinyWidgets::actionBttn("helpbw",
                                     "Help",
                                     icon = icon("question-circle"),
                                     style = "stretch",
                                     block = FALSE,
                                     color = "primary"
            ),
            shinyBS::bsModal(id="helpbandsel",
                             title = "",
                             trigger = "helpbw",
                             size="large",
                             tags$iframe(
                               src = "Help_bw.html",
                               width = "100%",
                               height = "1000px",
                               frameborder = 0,
                               scrolling = "auto"
                             )
            ),
            shinyWidgets::pickerInput(
              inputId = "bandSelect",
              label = "Choose type:",
              choices = c("Select type",
                          "bw.gwr",
                          "bw.ggwr",
                          #"bw.gtwr",
                          "bw.gwda",
                          "bw.gwpca"),
              options = list(style = "btn-primary")
            ),
            conditionalPanel(
              condition = "input.bandSelect == 'bw.ggwr'",
              div(style="display: inline-block; vertical-align:top;
                  width: 100px;",
                  selectizeInput('dependientggwr',
                                 'Dependient',
                                 choices = NULL,
                                 multiple = TRUE,
                                 options = list(maxItems = 1)
                  )
              ),
              div(style="display: inline-block;vertical-align:top;
                  width: 150px;",
                  selectizeInput('independientggwr',
                                 'Independient(s)',
                                 choices = NULL,
                                 multiple = TRUE)
              ),
              div(style="display: inline-block;vertical-align:top;
                  width: 150px;",
                  shinyWidgets::pickerInput(inputId = "familyggwr",
                              label = "Family",
                              choices = family
                  )),
              div(style="display: inline-block;vertical-align:top;
                  width: 100px;",
                  shinyWidgets::pickerInput(
                    inputId = "approachggwr",
                    label = "Approach",
                    choices = c("CV","AIC"))),
              div(style="display: inline-block;vertical-align:top;
                  width: 100px;",
                  shinyWidgets::pickerInput(
                    inputId = "kernelggwr",
                    label = "Kernel",choices = kernel)),

              div(style="display: inline-block;vertical-align:top;
                  width: 200px;",
                  numericInput(
                    'powerggwr',
                    'Power (Minkowski distance)',
                    2,
                    min = 1,
                    max = Inf)),
              div(style="display: inline-block;vertical-align:top;
                  width: 200px;",
                  sliderInput("thetaggwr",
                              "Theta (Angle in radians)",
                              min = 0,
                              max = 2,
                              value = 0,
                              step = 0.05 )),
              div(style="display: inline-block;vertical-align:top;
                  width: 150px;",
                  shinyWidgets::switchInput(
                    "longlat",
                    inputId = "longlatggwr",
                    onLabel = "TRUE",
                    offLabel = "FALSE",
                    size = "mini")),
              div(style="display: inline-block;vertical-align:top;
                  width: 100px;",
                  shinyWidgets::switchInput("adaptive",
                              inputId = "adaptativeggwr",
                              onLabel = "TRUE",
                              offLabel = "FALSE",
                              size = "mini") )
              ),
            conditionalPanel(
              condition = "input.bandSelect == 'bw.gwda'",
              div(style="display: inline-block;vertical-align:top;
                  width: 100px;",
                  selectizeInput(
                    'depbwgda',
                    'Dependient',
                    choices = NULL,
                    multiple = TRUE,
                    options = list(maxItems = 1))),
              div(style="display: inline-block;vertical-align:top;
                  width: 150px;",
                  selectizeInput(
                    'indepbwgda',
                    'Independient(s)',
                    choices = NULL,
                    multiple = TRUE)),
              div(style="display: inline-block;vertical-align:top;
                  width: 150px;",
                  shinyWidgets::switchInput(
                    "COV.gw",
                    inputId = "COVbwgda",
                    onLabel = "TRUE",
                    offLabel = "FALSE",
                    size = "mini")),
              div(style="display: inline-block;vertical-align:top;
                  width: 150px;",
                  shinyWidgets::switchInput(
                    "prior.gw",
                    inputId = "priorbwgda",
                    onLabel = "TRUE",
                    offLabel = "FALSE",
                    size = "mini")),
              div(style="display: inline-block;vertical-align:top;
                  width: 150px;",
                  shinyWidgets::switchInput(
                    "mean.gw",
                    inputId = "meanbwgda",
                    onLabel = "TRUE",
                    offLabel = "FALSE",
                    size = "mini")),
              div(style="display: inline-block;vertical-align:top;
                  width: 150px;",
                  shinyWidgets::switchInput(
                    "longlat",
                    inputId = "longlatbwgda",
                    onLabel = "TRUE",
                    offLabel = "FALSE",
                    size = "mini")),
              div(style="display: inline-block;vertical-align:top;
                  width: 150px;",
                  shinyWidgets::switchInput(
                    "wqda",
                    inputId = "wqdabwgda",
                    onLabel = "TRUE",
                    offLabel = "FALSE",
                    size = "mini")),
              div(style="display: inline-block;vertical-align:top;
                  width: 100px;",
                  shinyWidgets::switchInput(
                    "adaptive",
                    inputId = "adaptativebwgda",
                    onLabel = "TRUE",
                    offLabel = "FALSE",
                    size = "mini")),
              div(style="display: inline-block;vertical-align:top;
                  width: 100px;",
                  shinyWidgets::pickerInput(
                    inputId = "kernelbwgda",
                    label = "Kernel",
                    choices = kernel)),
              div(style="display: inline-block;vertical-align:top;
                  width: 200px;",
                  numericInput(
                    'powerbwgda',
                    'Power (Minkowski distance)',
                    2,
                    min = 1,max = Inf)),
              div(style="display: inline-block;vertical-align:top;
                  width: 200px;",
                  sliderInput(
                    "thetabwgda",
                    "Theta (Angle in radians)",
                    min = 0,
                    max = 2,
                    value = 0,
                    step = 0.05))),
            conditionalPanel(
              condition = "input.bandSelect == 'bw.gwpca'",
              selectizeInput(
                    'varpca',
                    'Variables',
                    choices = NULL,
                    multiple = TRUE ),
              numericInput(
                    'kpca',
                    'Number of components k :',
                    2,
                    min = 1,
                    max =Inf),
            shinyWidgets::switchInput(
                    "Robust",
                    inputId = "robustpca",
                    onLabel = "TRUE",
                    offLabel = "FALSE",
                    size = "mini"),
              shinyWidgets::pickerInput(
                    inputId = "kernelpca",
                    label = "Kernel",
                    choices = kernel),
               shinyWidgets::switchInput("adaptive",
                              inputId = "adaptativepca",
                              onLabel = "TRUE",
                              offLabel = "FALSE",
                              size = "mini"),

                  numericInput(
                    'powerpca',
                    'Power (Minkowski distance)',
                    2,
                    min = 1,max = Inf),
            sliderInput(
                    "thetapca","Theta (Angle in radians)",
                    min = 0,
                    max = 2,
                    value = 0,
                    step = 0.05),
             shinyWidgets::switchInput(
                    "longlat",
                    inputId = "longlatpca",
                    onLabel = "TRUE",
                    offLabel = "FALSE",
                    size = "mini")),
            conditionalPanel(
              condition = "input.bandSelect == 'bw.gwr'",
              div(style="display: inline-block;vertical-align:top;
                  width: 100px;",
                  selectizeInput(
                    'dependientgwr',
                    'Dependient',
                    choices = NULL,
                    multiple = TRUE,
                    options = list(maxItems = 1))),
              div(style="display: inline-block;vertical-align:top;
                  width: 150px;",
                  selectizeInput(
                    'independientgwr',
                    'Independient(s)',
                    choices = NULL,multiple = TRUE)),
              div(style="display: inline-block;vertical-align:top;
                  width: 100px;",
                  shinyWidgets::pickerInput(
                    inputId = "approachgwr",
                    label = "Approach",
                    choices = c("CV","AIC"))),
              div(style="display: inline-block; vertical-align:top;
                  width: 100px;",
                  shinyWidgets::pickerInput(
                    inputId = "kernelgwr",
                    label = "Kernel",
                    choices = kernel)),
              div(style="display: inline-block;vertical-align:top;
                  width: 200px;",
                  numericInput(
                    'powergwr',
                    'Power (Minkowski distance)',
                    2,
                    min = 1,max = Inf)),
              div(style="display: inline-block;vertical-align:top;
                  width: 200px;",
                  sliderInput(
                    "thetagwr",
                    "Theta (Angle in radians)",
                    min = 0,
                    max = 2,
                    value = 0,
                    step = 0.05)),
              div(style="display: inline-block;vertical-align:top;
                  width: 150px;",
                  shinyWidgets::switchInput(
                    "longlat",
                    inputId = "longlatgwr",
                    onLabel = "TRUE",
                    offLabel = "FALSE",
                    size = "mini")),
              div(style="display: inline-block;vertical-align:top;
                  width: 100px;",
                  shinyWidgets::switchInput(
                    "adaptive",
                    inputId = "adaptativegwr",
                    onLabel = "TRUE",
                    offLabel = "FALSE",
                    size = "mini")
              )),
            shinyalert::useShinyalert(),
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "Runbw",
              label = "Run",
              style = "float",
              block = TRUE,
              color = "danger"),
            ),
          mainPanel(verbatimTextOutput("selbw"))
      )))
    ),
    shinydashboard::tabItem(
      tabName = "tab4",fluidRow(column(width = 12,
      shinydashboard::box(width = 16,
          title = "Geographically Weighted Summary Statistics",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          height = 66000,
          sidebarPanel(shinyWidgets::actionBttn("helpgwss",
                                                "Help",
                                                icon = icon("question-circle"),
                                                style = "stretch",
                                                block = FALSE,
                                                color = "primary"
          ),
          shinyBS::bsModal(id="helpgeowss",
                           title = "",
                           trigger = "helpgwss",
                           size="large",
                           tags$iframe(
                             src = "Help_gwss.html",
                             width = "100%",
                             height = "1000px",
                             frameborder = 0,
                             scrolling = "auto"
                           )
          ),
          selectizeInput(
            'vargwss',
            'Variables',
            choices = NULL,
            multiple = TRUE),
          div(style="display: inline-block;vertical-align:top; width: 100px;",
                           shinyWidgets::pickerInput(
                             inputId = "kernelgwss",
                             label = "Kernel",
                             choices = kernel)),
                       div(style="display: inline-block;vertical-align:top;
                           width: 150px;",
                           shinyWidgets::prettyRadioButtons(
                             inputId = "selbw",
                             label = "Distance bandwidth:",
                             choices = c("Manual","Automatic"))),
                       conditionalPanel(
                         condition = "input.selbw == 'Manual'",
                         div(style="display: inline-block;vertical-align:top;
                             width: 200px;",
                             numericInput('bwgwss',
                                          'bw',
                                          10,
                                          min = 1,
                                          max =Inf))),
                       div(style="display: inline-block; vertical-align:top;
                           width: 200px;",
                           numericInput(
                             'powergwss',
                             'Power (Minkowski distance)',
                             2,
                             min = 1,
                             max = Inf)),
                       div(style="display: inline-block;vertical-align:top;
                           width: 200px;",
                           sliderInput(
                             "thetagwss",
                             "Theta (Angle in radians)",
                             min = 0,
                             max = 2,
                             value = 0,
                             step = 0.05)),
                       div(style="display: inline-block;vertical-align:top;
                           width: 100px;",
                           shinyWidgets::switchInput(
                             "longlat",
                             inputId = "longlatgwss",
                             onLabel = "TRUE",
                             offLabel = "FALSE",
                             size = "mini")),
                       div(style="display: inline-block;vertical-align:top;
                           width: 100px;",
                           shinyWidgets::switchInput(
                             "quantile",
                             inputId = "quantilegwss",
                             onLabel = "TRUE",
                             offLabel = "FALSE",
                             size = "mini")),
                       div(style="display: inline-block;vertical-align:top;
                           width: 100px;",
                           shinyWidgets::switchInput(
                             "adaptive",
                             inputId = "adaptativegwss",
                             onLabel = "TRUE",
                             offLabel = "FALSE",
                             size = "mini")),
                       shinyalert::useShinyalert(),
                       shinyjs::useShinyjs(),
                       shinyWidgets::actionBttn(
                         inputId = "Rungwss",
                         label = "Run",
                         style = "float",
                         block = TRUE,
                         color = "danger")),
          mainPanel(
            tabsetPanel(
              tabPanel(
                "Summary",
                helpText(h3("Summary Statistics:")),
                DT::DTOutput("DFgwss"),
                br(),
                verbatimTextOutput("gwss")),
              tabPanel("Plot",
                       helpText("click in Dropdown Button for customize
                                and download plot"),
                       shinyWidgets::dropdown(
                         tags$h3("List of Input"),
                         selectInput('plotgwss',
                                     'Select variable',choices = NULL),
                         textInput("maingwss",
                                   label = "Main title",
                                   value = "Write main title..."),

                         shinyWidgets::pickerInput(
                           inputId = "colorgwss",
                           label = "Select pallete",
                           choices = Pallete_color),
                         helpText(h4("North arrow position")),
                         numericInput("latgwss", "Latitude",
                                      min = -90, max = 90, value = 32
                         ),
                         numericInput("longwss", "Longitude",
                                      min = -180, max = 180, value = -74
                         ),
                         numericInput("scalegwss", "scale Arrow",
                                      min = 1, max = 10, value = 3
                         ),
                         shinyWidgets::actionBttn(
                           inputId = "Runplotgwss",
                           label = "Plot",
                           style = "float",
                           color = "success"
                         ),
                      radioButtons("butdowngwss", "Select the Option",
                                   choices = list("png","pdf")),
                         downloadButton(outputId = "downgwss",
                                        label = "Download the plot"),
                         style = "unite",
                         #icon = icon("gear"),
                         status = "primary", width = "300px",
                         animate = shinyWidgets::animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                           exit = animations$fading_exits$fadeOutRightBig)
                       ),
                       plotOutput("mapgwss",width = "100%"))
              )
          ))))),
    shinydashboard::tabItem(
      tabName = "tab52",fluidRow(column(width = 12,height = 6000,
      shinydashboard::box(width = 12,
                          title = "Geographically Weighted Regression",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          height = 66000,
                          sidebarPanel(
                            shinyWidgets::actionBttn(
                              "helpgwr",
                              "Help",
                              icon = icon("question-circle"),
                              style = "stretch",
                              block = FALSE,
                              color = "primary"),
                            shinyBS::bsModal(
                              id="helpgeowr",
                              title = "",
                              trigger = "helpgwr",
                              size="large",
                              tags$iframe(
                                src = "help_gwr.html",
                                width = "100%",
                                height = "1000px",
                                frameborder = 0,
                                scrolling = "auto")
                              ),
                            shinyWidgets::pickerInput(
                              inputId = "Selectgwr",
                              label = "Choose:",
                              choices = c(
                                "Select",
                                "Local collinearity diagnostics",
                                "Basic GWR model",
                                "Robust GWR model",
                                "Generalised GWR models",
                                "Heteroskedastic GWR",
                                "Mixed GWR",
                                "Scalable GWR"),
                              options = list(
                                style = "btn-primary")),
                            conditionalPanel(
                              condition = "input.Selectgwr == 'Local collinearity diagnostics'",
                              div(style="display: inline-block;vertical-align:top;
                                                     width: 100px;",
                                  selectizeInput('dependientbgwrd',
                                                 'Dependient',
                                                 choices = NULL,
                                                 multiple = TRUE,
                                                 options = list(maxItems = 1))),
                              div(style="display: inline-block;vertical-align:top;
                              width: 150px;",
                              selectizeInput(
                                'independientbgwrd',
                                'Independient(s)',
                                choices = NULL,
                                multiple = TRUE)),
                              div(style="display: inline-block;vertical-align:top;
                              width: 100px;",
                              shinyWidgets::switchInput(
                                "longlat",
                                inputId = "longlatbgwrd",
                                onLabel = "TRUE",
                                offLabel = "FALSE",
                                size = "mini")),
                              div(style="display: inline-block;vertical-align:top;
                              width: 100px;",
                              shinyWidgets::switchInput("adaptive",
                                                        inputId = "adaptativebgwrd",
                                                        onLabel = "TRUE",
                                                        offLabel = "FALSE",
                                                        size = "mini")),
                              div(
                                style="display: inline-block;vertical-align:top;
                          width: 150px;",
                          shinyWidgets::prettyRadioButtons(
                            inputId = "selbgwrd",
                            label = "Distance bandwidth:",
                            choices = c("Automatic","Manual")
                          )),
                          conditionalPanel(condition = "input.selbgwrd == 'Manual'",
                                           div(style="display: inline-block;
                                           vertical-align:top; width: 200px;",
                                           numericInput('bwbgwrd',
                                                        'bw',
                                                        10,
                                                        min = 1,
                                                        max =Inf))),
                          div(style="display: inline-block;
                          vertical-align:top; width: 100px;",
                          shinyWidgets::pickerInput(
                            inputId = "kernelbgwrd",
                            label = "Kernel",
                            choices = kernel
                          )),
                          div(style="display: inline-block;
                                       vertical-align:top; width: 200px;",
                              numericInput('powerbgwrd',
                                           'Power (Minkowski distance)',
                                           2,
                                           min = 1,
                                           max = Inf)),
                          div(style="display: inline-block;
                                       vertical-align:top; width: 200px;",
                              sliderInput("thetabgwrd",
                                          "Theta (Angle in radians)",
                                          min = 0,
                                          max = 2,
                                          value = 0,
                                          step = 0.05
                              )),

                          shinyalert::useShinyalert(),
                          shinyjs::useShinyjs(),
                          shinyWidgets::actionBttn(
                            inputId = "Runbgwrd",
                            label = "Run",
                            style = "float",
                            block = TRUE,
                            color = "danger"
                          )),
                        conditionalPanel(
                          condition = "input.Selectgwr == 'Basic GWR model'",
                          div(style="display: inline-block;vertical-align:top;
                                                     width: 100px;",
                              selectizeInput('dependientbgwr',
                                             'Dependient',
                                             choices = NULL,
                                             multiple = TRUE,
                                             options = list(maxItems = 1))),
                          div(style="display: inline-block;vertical-align:top;
                              width: 150px;",
                              selectizeInput(
                                'independientbgwr',
                                'Independient(s)',
                                choices = NULL,
                                multiple = TRUE)),
                          div(style="display: inline-block;vertical-align:top;
                              width: 100px;",
                              shinyWidgets::switchInput(
                                "longlat",
                                inputId = "longlatbgwr",
                                onLabel = "TRUE",
                                offLabel = "FALSE",
                                size = "mini")),
                          div(style="display: inline-block;vertical-align:top;
                              width: 100px;",
                              shinyWidgets::switchInput(
                                "cv",
                                inputId = "cvbgwr",
                                onLabel = "TRUE",
                                offLabel = "FALSE",
                                size = "mini")),
                          div(style="display: inline-block;vertical-align:top;
                              width: 100px;",
                          shinyWidgets::switchInput("adaptive",
                                      inputId = "adaptativebgwr",
                                      onLabel = "TRUE",
                                      offLabel = "FALSE",
                                      size = "mini")),
                          div(
                            style="display: inline-block;vertical-align:top;
                          width: 150px;",
                      shinyWidgets::prettyRadioButtons(
                        inputId = "selbgwr",
                        label = "Distance bandwidth:",
                        choices = c("Automatic","Manual")
                          )),
                      conditionalPanel(condition = "input.selbgwr == 'Manual'",
                                       div(style="display: inline-block;
                                           vertical-align:top; width: 200px;",
                                           numericInput('bwbgwr',
                                                        'bw',
                                                        10,
                                                        min = 1,
                                                        max =Inf))),
                      div(style="display: inline-block;
                          vertical-align:top; width: 100px;",
                          shinyWidgets::pickerInput(
                            inputId = "kernelbgwr",
                            label = "Kernel",
                            choices = kernel
                          )),
                      div(style="display: inline-block;
                                       vertical-align:top; width: 200px;",
                          numericInput('powerbgwr',
                                       'Power (Minkowski distance)',
                                       2,
                                       min = 1,
                                       max = Inf)),
                      div(style="display: inline-block;
                                       vertical-align:top; width: 200px;",
                          sliderInput("thetabgwr",
                                      "Theta (Angle in radians)",
                                      min = 0,
                                      max = 2,
                                      value = 0,
                                      step = 0.05
                          )),

                      shinyalert::useShinyalert(),
                      shinyjs::useShinyjs(),
                      shinyWidgets::actionBttn(
                        inputId = "Runbgwr",
                        label = "Run",
                        style = "float",
                        block = TRUE,
                        color = "danger"
                      )),
                  conditionalPanel(
                    condition = "input.Selectgwr == 'Generalised GWR models'",
                                       div(
                                         style="display: inline-block;
                                         vertical-align:top; width: 150px;",
                                         selectizeInput(
                                           'dependientGGWR',
                                           'Dependient',
                                           choices = NULL,
                                           multiple = TRUE,
                                           options = list(maxItems = 1)
                                           )),
                    div(style="display: inline-block;vertical-align:top;
                        width: 150px;",
                        selectizeInput(
                          'independientGGWR',
                          'Independient(s)',
                          choices = NULL,
                          multiple = TRUE)),
                    div(style="display: inline-block;vertical-align:top;
                        width: 100px;",
                        shinyWidgets::switchInput(
                          "longlat",
                          inputId = "longlatGGWR",
                          onLabel = "TRUE",
                          offLabel = "FALSE",
                          size = "mini")),
                    div(style="display: inline-block;vertical-align:top;
                        width: 100px;",
                        shinyWidgets::switchInput(
                          "cv",
                          inputId = "cvGGWR",
                          onLabel = "TRUE",
                          offLabel = "FALSE",
                          size = "mini")),
                    div(style="display: inline-block;vertical-align:top;
                        width: 100px;",
                        shinyWidgets::switchInput(
                          "adaptive",
                          inputId = "adaptativeGGWR",
                          onLabel = "TRUE",
                          offLabel = "FALSE",
                          size = "mini")),
                    div(style="display: inline-block;vertical-align:top;
                        width: 150px;",
                        shinyWidgets::prettyRadioButtons(
                          inputId = "selGGWR",
                          label = "Distance bandwidth:",
                          choices = c("Automatic","Manual"))),
                    conditionalPanel(
                      condition = "input.selGGWR == 'Manual'",
                      div(style="display: inline-block;vertical-align:top;
                          width: 200px;",
                          numericInput('bwGGWR',
                                       'bw',
                                       10,
                                       min = 1,
                                       max =Inf))),
                    div(style="display: inline-block; vertical-align:top;
                        width: 100px;",
                        shinyWidgets::pickerInput(
                          inputId = "kernelGGWR",
                          label = "Kernel",
                          choices = kernel)),
                    div(style="display: inline-block;vertical-align:top;
                        width: 150px;",
                        shinyWidgets::pickerInput(
                          inputId = "familyGGWR",
                          label = "Family",
                          choices = family)),
                    div(style="display: inline-block; vertical-align:top;
                        width: 200px;",
                        numericInput(
                          'powerGGWR',
                          'Power (Minkowski distance)',
                          2,
                          min = 1,
                          max = Inf)),
                    div(style="display: inline-block; vertical-align:top;
                        width: 100px;",
                        numericInput('maxiterGGWR',
                                     'maxiter',
                                     20,
                                     min = 2,
                                     max = Inf)),
                    div(style="display: inline-block;
                        vertical-align:top; width: 200px;",
                        sliderInput("thetaGGWR",
                                    "Theta (Angle in radians)",
                                    min = 0,
                                    max = 2,
                                    value = 0,
                                    step = 0.05
                                    )),
                    shinyalert::useShinyalert(),
                    shinyjs::useShinyjs(),
                    shinyWidgets::actionBttn(
                      inputId = "RunGGWR",
                      label = "Run",
                      style = "float",
                      block = TRUE,
                      color = "danger")),
              conditionalPanel(
                condition = "input.Selectgwr == 'Robust GWR model'",
                                       div(style="display: inline-block;
                      vertical-align:top; width: 100px;",
                      selectizeInput('dependientRobust',
                                     'Dependient',
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = list(maxItems = 1)
                      )),
                      div(style="display: inline-block;
                      vertical-align:top; width: 150px;",
                      selectizeInput('independientRobust',
                                     'Independient(s)',
                                     choices = NULL,
                                     multiple = TRUE)),
                      div(style="display: inline-block;vertical-align:top;
                          width: 100px;",
                          shinyWidgets::switchInput("longlat",
                                      inputId = "longlatRobust",
                                      onLabel = "TRUE",
                                      offLabel = "FALSE",
                                      size = "mini")),
                div(style="display: inline-block;vertical-align:top;
                          width: 100px;",
                          shinyWidgets::switchInput("Filtered",
                                      inputId = "filtered",
                                      onLabel = "TRUE",
                                      offLabel = "FALSE",
                                      size = "mini")),
                      div(style="display: inline-block;vertical-align:top;
                          width: 100px;",
                          shinyWidgets::switchInput("adaptive",
                                      inputId = "adaptativeRobust",
                                      onLabel = "TRUE",
                                      offLabel = "FALSE",
                                      size = "mini")

                      ),

                      div(style="display: inline-block;vertical-align:top;
                          width: 150px;",
                          shinyWidgets::prettyRadioButtons(
                            inputId = "selRobust",
                            label = "Distance bandwidth:",
                            choices = c("Automatic","Manual"))),
                      conditionalPanel(
                        condition = "input.selRobust == 'Manual'",
                        div(style="display: inline-block;vertical-align:top;
                            width: 200px;",
                            numericInput('bwRobust',
                                         'bw',
                                         10,
                                         min = 1,
                                         max =Inf))),
                      div(style="display: inline-block; vertical-align:top;
                          width: 100px;",
                          numericInput('maxiterRobust',
                                       'maxiter',
                                       20,
                                       min = 2,
                                       max = Inf)),
                      div(style="display: inline-block; vertical-align:top;
                          width: 100px;",
                          shinyWidgets::pickerInput(
                            inputId = "kernelRobust",
                            label = "Kernel",
                            choices = kernel
                          )),
                      div(style="display: inline-block;
                                       vertical-align:top; width: 200px;",
                          numericInput('powerRobust',
                                       'Power (Minkowski distance)',
                                       2,
                                       min = 1,
                                       max = Inf)),
                      div(style="display: inline-block;
                                       vertical-align:top; width: 200px;",
                          sliderInput("thetaRobust",
                                      "Theta (Angle in radians)",
                                      min = 0,
                                      max = 2,
                                      value = 0,
                                      step = 0.05
                          )),

                      shinyalert::useShinyalert(),
                      shinyjs::useShinyjs(),
                      shinyWidgets::actionBttn(
                        inputId = "RunRobust",
                        label = "Run",
                        style = "float",
                        block = TRUE,
                        color = "danger")),
              conditionalPanel(
                condition = "input.Selectgwr == 'Heteroskedastic GWR'",
                                       div(style="display: inline-block;
                      vertical-align:top; width: 100px;",
                      selectizeInput('dependienthetero',
                                     'Dependient',
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = list(maxItems = 1)
                      )),
                      div(style="display: inline-block;
                      vertical-align:top; width: 150px;",
                      selectizeInput('independienthetero',
                                     'Independient(s)',
                                     choices = NULL,
                                     multiple = TRUE)),
                      div(style="display: inline-block;vertical-align:top;
                          width: 100px;",
                          shinyWidgets::switchInput("longlat",
                                      inputId = "longlathetero",
                                      onLabel = "TRUE",
                                      offLabel = "FALSE",
                                      size = "mini")),
                      div(style="display: inline-block;vertical-align:top;
                          width: 100px;",
                          shinyWidgets::switchInput("adaptive",
                                      inputId = "adaptativehetero",
                                      onLabel = "TRUE",
                                      offLabel = "FALSE",
                                      size = "mini")

                      ),

                      div(style="display: inline-block;vertical-align:top;
                          width: 150px;",
                          shinyWidgets::prettyRadioButtons(
                            inputId = "selhetero",
                            label = "Distance bandwidth:",
                            choices = c("Automatic","Manual"))),
                conditionalPanel(
                  condition = "input.selhetero == 'Manual'",
                  div(style="display: inline-block;vertical-align:top;
                      width: 200px;",
                      numericInput('bwhetero',
                                   'bw',
                                   10,
                                   min = 1,
                                   max =Inf))),
                div(style="display: inline-block; vertical-align:top;
                    width: 100px;",
                    numericInput(
                      'maxiterhetero',
                      'maxiter',
                      50,
                      min = 2,
                      max = Inf)),
                      div(style="display: inline-block; vertical-align:top;
                          width: 100px;",
                          shinyWidgets::pickerInput(
                            inputId = "kernelhetero",
                            label = "Kernel",
                            choices = kernel
                          )),
                      div(style="display: inline-block;
                                       vertical-align:top; width: 200px;",
                          numericInput('powerhetero',
                                       'Power (Minkowski distance)',
                                       2,
                                       min = 1,
                                       max = Inf)),
                      div(style="display: inline-block;
                                       vertical-align:top; width: 200px;",
                          sliderInput("thetahetero",
                                      "Theta (Angle in radians)",
                                      min = 0,
                                      max = 2,
                                      value = 0,
                                      step = 0.05
                          )),

                      shinyalert::useShinyalert(),
                      shinyjs::useShinyjs(),
                      shinyWidgets::actionBttn(
                        inputId = "Runhetero",
                        label = "Run",
                        style = "float",
                        block = TRUE,
                        color = "danger"
                      )
                ),
              conditionalPanel(condition = "input.Selectgwr == 'Mixed GWR'",
                                       div(style="display: inline-block;
                      vertical-align:top; width: 100px;",
                      selectizeInput('dependientmixed',
                                     'Dependient',
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = list(maxItems = 1)
                      )),
                      div(style="display: inline-block;
                      vertical-align:top; width: 150px;",
                      selectizeInput('independientmixed',
                                     'Independient(s)',
                                     choices = NULL,
                                     multiple = TRUE)),
                      div(style="display: inline-block;
                      vertical-align:top; width: 150px;",
                      selectizeInput('fixedvars',
                                     ' fixed var',
                                     choices = NULL,
                                     multiple = TRUE)),
                      div(style="display: inline-block;vertical-align:top;
                          width: 150px;",
                          shinyWidgets::switchInput("Intercep.fixed",
                                      inputId = "intercepfixed",
                                      onLabel = "TRUE",
                                      offLabel = "FALSE",
                                      size = "mini")),
                      div(style="display: inline-block;vertical-align:top;
                          width: 100px;",
                          shinyWidgets::switchInput("Diagnostic",
                                      inputId = "diagnostic",
                                      onLabel = "TRUE",
                                      offLabel = "FALSE",
                                      size = "mini")),
                      div(style="display: inline-block;vertical-align:top;
                          width: 100px;",
                          shinyWidgets::switchInput("longlat",
                                      inputId = "longlatmixed",
                                      onLabel = "TRUE",
                                      offLabel = "FALSE",
                                      size = "mini")),
                      div(style="display: inline-block;vertical-align:top;
                          width: 100px;",
                          shinyWidgets::switchInput("adaptive",
                                      inputId = "adaptativemixed",
                                      onLabel = "TRUE",
                                      offLabel = "FALSE",
                                      size = "mini")
                      ),

                      div(style="display: inline-block;vertical-align:top;
                          width: 150px;",
                          shinyWidgets::prettyRadioButtons(
                            inputId = "selmixed",
                            label = "Distance bandwidth:",
                            choices = c("Automatic","Manual")
                            )),
                      conditionalPanel(
                        condition = "input.selmixed == 'Manual'",
                        div(style="display: inline-block;vertical-align:top;
                            width: 200px;",
                            numericInput(
                              'bwmixed',
                              'bw',
                              10,
                              min = 1,
                              max =Inf))),
                      div(style="display: inline-block; vertical-align:top;
                          width: 100px;",
                          shinyWidgets::pickerInput(
                            inputId = "kernelmixed",
                            label = "Kernel",
                            choices = kernel
                          )),
                      div(style="display: inline-block;
                                       vertical-align:top; width: 200px;",
                          numericInput('powermixed',
                                       'Power (Minkowski distance)',
                                       2,
                                       min = 1,
                                       max = Inf)),
                      div(style="display: inline-block;
                                       vertical-align:top; width: 200px;",
                          sliderInput("thetamixed",
                                      "Theta (Angle in radians)",
                                      min = 0,
                                      max = 2,
                                      value = 0,
                                      step = 0.05
                          )),

                      shinyalert::useShinyalert(),
                      shinyjs::useShinyjs(),
                      shinyWidgets::actionBttn(
                        inputId = "Runmixed",
                        label = "Run",
                        style = "float",
                        block = TRUE,
                        color = "danger"
                      )
                      ),
              conditionalPanel(
                condition = "input.Selectgwr == 'Scalable GWR'",
                                       div(style="display: inline-block;
                      vertical-align:top; width: 100px;",
                      selectizeInput('dependientscal',
                                     'Dependient',
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = list(maxItems = 1)
                      )),
                      div(style="display: inline-block;
                      vertical-align:top; width: 150px;",
                      selectizeInput('independientscal',
                                     'Independient(s)',
                                     choices = NULL,
                                     multiple = TRUE)),
                      div(style="display: inline-block;vertical-align:top;
                          width: 100px;",
                          numericInput('bwscal',
                                       'bw.adapt',
                                       10,
                                       min = 1,
                                       max =Inf)),
                      div(style="display: inline-block;vertical-align:top;
                          width: 100px;",
                          numericInput('polinom',
                                       'polynomial',
                                       4,
                                       min = 1,
                                       max = 10)),
                      div(style="display: inline-block; vertical-align:top;
                          width: 100px;",
                          shinyWidgets::pickerInput(
                            inputId = "kernelscal",
                            label = "Kernel",
                            choices = c("gaussian",
                                        "exponential")
                          )),
                      div(style="display: inline-block;
                                       vertical-align:top; width: 200px;",
                          numericInput('powerscal',
                                       'Power (Minkowski distance)',
                                       2,
                                       min = 1,
                                       max = Inf)),
                      div(style="display: inline-block;
                                       vertical-align:top; width: 200px;",
                          sliderInput("thetascal",
                                      "Theta (Angle in radians)",
                                      min = 0,
                                      max = 2,
                                      value = 0,
                                      step = 0.05
                          )),
                      div(style="display: inline-block;vertical-align:top;
                          width: 100px;",
                          shinyWidgets::switchInput("longlat",
                                      inputId = "longlatscal",
                                      onLabel = "TRUE",
                                      offLabel = "FALSE",
                                      size = "mini")),

                      shinyalert::useShinyalert(),
                      shinyjs::useShinyjs(),
                      shinyWidgets::actionBttn(
                        inputId = "Runscal",
                        label = "Run",
                        style = "float",
                        block = TRUE,
                        color = "danger"
                      )
                      )
                                                ),

                      mainPanel(
                        tabsetPanel(
                          tabPanel("Summary",
                                   conditionalPanel(condition = "input.Selectgwr == 'Local collinearity diagnostics'",
                                                    helpText(h3("SDF:")),
                                                     DT::DTOutput("gwrbasicdVIF"),
                                   ),
                                   conditionalPanel(condition = "input.Selectgwr == 'Basic GWR model'",
                                   verbatimTextOutput("gwrbasic")),
                                   conditionalPanel(condition ="input.Selectgwr == 'Generalised GWR models'",
                                   verbatimTextOutput("GGWRbasic")),
                                   conditionalPanel(condition ="input.Selectgwr == 'Robust GWR model'",
                                   verbatimTextOutput("GGWRobust")),
                                   conditionalPanel(condition ="input.Selectgwr == 'Heteroskedastic GWR'",
                                   verbatimTextOutput("GWRhetero")),
                                   conditionalPanel(condition ="input.Selectgwr == 'Mixed GWR'",
                                   verbatimTextOutput("GWRmixed")),
                                   conditionalPanel(condition ="input.Selectgwr == 'Scalable GWR'",
                                   verbatimTextOutput("GWRscalable")),
                                   br(),

                                   conditionalPanel(condition = "input.Selectgwr == 'Basic GWR model'",
                                                    helpText(h3("SDF:")),
                                                    DT::DTOutput("SDFgwrbasic")),
                                   conditionalPanel(condition ="input.Selectgwr == 'Generalised GWR models'",
                                                    helpText(h3("SDF:")),
                                                    DT::DTOutput("SDFGGWRbasic")),
                                   conditionalPanel(condition ="input.Selectgwr == 'Robust GWR model'",
                                                    helpText(h3("SDF:")),
                                                    DT::DTOutput("SDFGWRRobust")),
                                   conditionalPanel(condition ="input.Selectgwr == 'Heteroskedastic GWR'",
                                                    helpText(h3("SDF:")),
                                                    DT::DTOutput("SDFGWRhetero")),
                                   conditionalPanel(condition ="input.Selectgwr == 'Mixed GWR'",
                                                    helpText(h3("SDF:")),
                                                    DT::DTOutput("SDFGWRmixed")),
                                   conditionalPanel(condition ="input.Selectgwr == 'Scalable GWR'",
                                                    helpText(h3("SDF:")),
                                                    DT::DTOutput("SDFGWRscalable"))
                      ),
                      tabPanel(
                        "Plot",
                        conditionalPanel(
                          condition = "input.Selectgwr == 'Local collinearity diagnostics'",
                          helpText("click in Dropdown Button for
                                   customize and download plot"),
                          shinyWidgets::dropdown(
                            tags$h3("List of Input"),
                            selectInput('plotgwrd',
                                        'Select',
                                        choices = NULL),
                            textInput("maingwrd",
                                      label = "Main title",
                                      value = "Write main title..."),
                            shinyWidgets::pickerInput(
                              inputId = "colorgwrd",
                              label = "Select pallete",
                              choices = Pallete_color),
                            br(),
                            helpText(h4("North arrow position")),
                            numericInput("latgwrd",
                                         "Latitude",
                                         min = -90,
                                         max = 90,
                                         value = 32),
                            numericInput("scalegwrd",
                                         "scale Arrow",
                                         min = 1,
                                         max = 10,
                                         value = 3),
                            numericInput("longwrd",
                                         "Longitude",
                                         min = -180,
                                         max = 180,
                                         value = -74),
                            shinyWidgets::actionBttn(
                              inputId = "Runplotgwrd",
                              label = "Plot",
                              style = "float",
                              color = "success"),
                            radioButtons("butdowngwrd",
                                         "Select the Option",
                                         choices = list("png",
                                                        "pdf")),
                            downloadButton(
                              outputId = "downgwrd",
                              label = "Download the plot"),
                            style = "unite",
                            #icon = icon("gear"),
                            status = "primary",
                            width = "300px",
                            animate = shinyWidgets::animateOptions(
                              enter = animations$fading_entrances$fadeInLeftBig,
                              exit = animations$fading_exits$fadeOutRightBig))),
                        conditionalPanel(
                          condition = "input.Selectgwr == 'Basic GWR model'",
                          helpText("click in Dropdown Button for
                                   customize and download plot"),
                          shinyWidgets::dropdown(
                            tags$h3("List of Input"),
                            selectInput('plotgwr',
                                        'Select variable',
                                        choices = NULL),
                            textInput("maingwr",
                                      label = "Main title",
                                      value = "Write main title..."),
                            shinyWidgets::pickerInput(
                              inputId = "colorgwr",
                              label = "Select pallete",
                              choices = Pallete_color),
                            br(),
                            helpText(h4("North arrow position")),
                            numericInput("latgwr",
                                         "Latitude",
                                         min = -90,
                                         max = 90,
                                         value = 32),
                            numericInput("scalegwr",
                                         "scale Arrow",
                                         min = 1,
                                         max = 10,
                                         value = 3),
                            numericInput("longwr",
                                         "Longitude",
                                         min = -180,
                                         max = 180,
                                         value = -74),
                            shinyWidgets::actionBttn(
                              inputId = "Runplotgwr",
                              label = "Plot",
                              style = "float",
                              color = "success"),
                            radioButtons("butdowngwr",
                                         "Select the Option",
                                         choices = list("png",
                                                        "pdf")),
                            downloadButton(
                              outputId = "downgwr",
                              label = "Download the plot"),
                            style = "unite",
                            #icon = icon("gear"),
                            status = "primary",
                            width = "300px",
                            animate = shinyWidgets::animateOptions(
                              enter = animations$fading_entrances$fadeInLeftBig,
                              exit = animations$fading_exits$fadeOutRightBig))),
                  conditionalPanel(
                    condition = "input.Selectgwr == 'Generalised GWR models'",
                    helpText("click in Dropdown Button for customize
                             and download plot"),
                    shinyWidgets::dropdown(
                      tags$h3("List of Input"),
                      selectInput('plotggwr',
                                  'Select variable',
                                  choices = NULL),
                      textInput("mainggwr",
                                label = "Main title",
                                value = "Write main title..."),
                      shinyWidgets::pickerInput(
                        inputId = "colorggwr",
                        label = "Select pallete",
                        choices = Pallete_color),
                      br(),
                      helpText(h4("North arrow position")),
                      numericInput("latggwr",
                                   "Latitude",
                                   min = -90,
                                   max = 90,
                                   value = 32),
                      numericInput("scaleggwr",
                                   "scale Arrow",
                                   min = 1,
                                   max = 10,
                                   value = 3),
                      numericInput("longgwr",
                                   "Longitude",
                                   min = -180,
                                   max = 180,
                                   value = -75),
                      shinyWidgets::actionBttn(
                        inputId = "Runplotggwr",
                        label = "Plot",
                        style = "float",
                        color = "success"
                        ),
                      radioButtons("butdownggwr",
                                   "Select the Option",
                                   choices = list("png",
                                                  "pdf")),
                      downloadButton(outputId = "downggwr",
                                     label = "Download the plot"),
                      style = "unite",
                      #icon = icon("gear"),
                      status = "primary", width = "300px",
                      animate = shinyWidgets::animateOptions(
                        enter = animations$fading_entrances$fadeInLeftBig,
                        exit = animations$fading_exits$fadeOutRightBig)
                      )),
        conditionalPanel(
          condition = "input.Selectgwr == 'Robust GWR model'",
          helpText("click in Dropdown Button for customize and download plot"),
          shinyWidgets::dropdown(
            tags$h3("List of Input"),
            selectInput('plotRgwr',
                        'Select variable',
                        choices = NULL),
            textInput("mainRgwr",
                      label = "Main title",
                      value = "Write main title..."),
            shinyWidgets::pickerInput(
              inputId = "colorRgwr",
              label = "Select pallete",
              choices = Pallete_color),
            br(),
            helpText(h4("North arrow position")),
            numericInput("latRgwr", "Latitude",
                         min = -90,
                         max = 90,
                         value = 32),
            numericInput("scaleRgwr",
                         "scale Arrow",
                         min = 1,
                         max = 10,
                         value = 2 ),
            numericInput("lonRgwr",
                         "Longitude",
                         min = -180,
                         max = 180,
                         value = -74),
            shinyWidgets::actionBttn(
              inputId = "RunplotRgwr",
              label = "Plot",
              style = "float",
              color = "success"),
            radioButtons("butdownRgwr",
                         "Select the Option",
                         choices = list("png","pdf")),
            downloadButton(
              outputId = "downRgwr",
              label = "Download the plot"),
            style = "unite",
            #icon = icon("gear"),
            status = "primary", width = "300px",
            animate = shinyWidgets::animateOptions(
              enter = animations$fading_entrances$fadeInLeftBig,
              exit = animations$fading_exits$fadeOutRightBig))),
    conditionalPanel(
      condition = "input.Selectgwr == 'Heteroskedastic GWR'",
      helpText("click in Dropdown Button for customize and download plot"),
      shinyWidgets::dropdown(
        tags$h3("List of Input"),
        selectInput('plotHgwr',
                    'Select variable',
                    choices = NULL),
        textInput("mainHgwr",
                  label = "Main title",
                  value = "Write main title..."),
        shinyWidgets::pickerInput(
          inputId = "colorHgwr",
          label = "Select pallete",
          choices = Pallete_color),
        br(),
        helpText(h4("North arrow position")),
        numericInput("latHgwr",
                     "Latitude",
                     min = -90,
                     max = 90,
                     value = 32),
        numericInput("scaleHgwr",
                     "scale Arrow",
                     min = 1,
                     max = 10,
                     value = 3 ),
        numericInput("lonHgwr",
                     "Longitude",
                     min = -180,
                     max = 180,
                     value = -74),
        shinyWidgets::actionBttn(
          inputId = "RunplotHgwr",
          label = "Plot",
          style = "float",
          color = "success"),
        radioButtons("butdownHgwr",
                     "Select the Option",
                     choices = list("png","pdf")),
        downloadButton(outputId = "downHgwr",
                       label = "Download the plot"),
        style = "unite",
        #icon = icon("gear"),
        status = "primary",
        width = "300px",
        animate = shinyWidgets::animateOptions(
          enter = animations$fading_entrances$fadeInLeftBig,
          exit = animations$fading_exits$fadeOutRightBig))),
    conditionalPanel(
      condition = "input.Selectgwr == 'Mixed GWR'",
      helpText("click in Dropdown Button for customize and download plot"),
      shinyWidgets::dropdown(
        tags$h3("List of Input"),
        selectInput('plotMgwr',
                    'Select variable',
                    choices = NULL),
        textInput("mainMgwr",
                  label = "Main title",
                  value = "Write main title..."),
        shinyWidgets::pickerInput(
          inputId = "colorMgwr",
          label = "Select pallete",
          choices = Pallete_color),
        br(),
        helpText(h4("North arrow position")),
        numericInput("latMgwr", "Latitude",
                     min = -90,
                     max = 90,
                     value = 32),
        numericInput("scaleMgwr",
                     "scale Arrow",
                     min = 1,
                     max = 10,
                     value = 2),
        numericInput("lonMgwr",
                     "Longitude",
                     min = -180,
                     max = 180,
                     value = -74),
        shinyWidgets::actionBttn(
          inputId = "RunplotMgwr",
          label = "Plot",
          style = "float",
          color = "success"),
        radioButtons("butdownMgwr",
                     "Select the Option",
                     choices = list("png","pdf")),
        downloadButton(outputId = "downMgwr",
                       label = "Download the plot"),
        style = "unite",
        #icon = icon("gear"),
        status = "primary", width = "300px",
        animate = shinyWidgets::animateOptions(
          enter = animations$fading_entrances$fadeInLeftBig,
          exit = animations$fading_exits$fadeOutRightBig)
        )),
    conditionalPanel(
      condition = "input.Selectgwr == 'Scalable GWR'",
      helpText("click in Dropdown Button for customize and download plot"),
      shinyWidgets::dropdown(
        tags$h3("List of Input"),
        selectInput('plotSgwr',
                    'Select variable',choices = NULL),
        textInput("mainSgwr",
                  label = "Main title",
                  value = "Write main title..."),
        shinyWidgets::pickerInput(
          inputId = "colorSgwr",
          label = "Select pallete",
          choices = Pallete_color),
        br(),
        helpText(h4("North arrow position")),
        numericInput("latSgwr",
                     "Latitude",
                     min = -90,
                     max = 90,
                     value = 32),
        numericInput("scaleSgwr",
                     "scale Arrow",
                     min = 1,
                     max = 10,
                     value = 3),
        numericInput("lonSgwr",
                     "Longitude",
                     min = -180,
                     max = 180,
                     value = -74),
        shinyWidgets::actionBttn(
          inputId = "RunplotSgwr",
          label = "Plot",
          style = "float",
          color = "success"
          ),
        radioButtons("butdownSgwr",
                     "Select the Option",
                     choices = list("png","pdf")),
        downloadButton(outputId = "downSgwr",
                       label = "Download the plot"),
        style = "unite",
        #icon = icon("gear"),
        status = "primary", width = "300px",
        animate = shinyWidgets::animateOptions(
          enter = animations$fading_entrances$fadeInLeftBig,
          exit = animations$fading_exits$fadeOutRightBig))),
    conditionalPanel(condition = "input.Selectgwr == 'Local collinearity diagnostics'",
                     plotOutput("mapgwrbasicd") ),
    conditionalPanel(condition = "input.Selectgwr == 'Basic GWR model'",
                     plotOutput("mapgwrbasic") ),
    conditionalPanel( condition = "input.Selectgwr == 'Scalable GWR'",
                      plotOutput("mapSgwr")),
    conditionalPanel( condition = "input.Selectgwr == 'Mixed GWR'",
                      plotOutput("mapMgwr")),
    conditionalPanel( condition = "input.Selectgwr == 'Heteroskedastic GWR'",
                      plotOutput("mapHgwr")),
    conditionalPanel( condition = "input.Selectgwr == 'Robust GWR model'",
                      plotOutput("mapRgwr")),
    conditionalPanel( condition = "input.Selectgwr == 'Generalised GWR models'",
                      plotOutput("mapggwr"))
))))))),
shinydashboard::tabItem(
  tabName = "tab53",fluidRow(column(width = 12,
  shinydashboard::box(
    width = 12,
    title = "Geographically Weighted Principal Components Analysis",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    height = 66000,
    sidebarPanel(shinyWidgets::actionBttn(
      "helpgwpca",
      "Help",
      icon = icon("question-circle"),
      style = "stretch",
      block = FALSE,
      color = "primary"
                ),
      shinyBS::bsModal(
        id="helpgeowpca",
        title = "",
        trigger = "helpgwpca",
        size="large",
        tags$iframe(
          src = "Help_gwpca.html",
          width = "100%",
          height = "1000px",
          frameborder = 0,
          scrolling = "auto")),
      selectizeInput(
        'vargwpca',
        'Variables',
        choices = NULL,
        multiple = TRUE),
      numericInput(
        'kgwpca',
        'k',
        2,
        min = 1,
        max =Inf),
      pickerInput(
        inputId = "kernelgwpca",
        label = "Kernel",
        choices = kernel),
      switchInput(
        "Robust",
        inputId = "robustgwpca",
        onLabel = "TRUE",
        offLabel = "FALSE",
        size = "mini"),
      switchInput(
        "adaptive",
        inputId = "adaptativegwpca",
        onLabel = "TRUE",
        offLabel = "FALSE",
        size = "mini"),
      shinyWidgets::switchInput(
        "longlat",
        inputId = "longlatgwpca",
        onLabel = "TRUE",
        offLabel = "FALSE",
        size = "mini"),
      shinyWidgets::prettyRadioButtons(
        inputId = "selgwpca",
        label = "Distance bandwidth:",
        choices = c("Automatic",
                    "Manual")),
      conditionalPanel(condition = "input.selgwpca == 'Manual'",
                       div(style="display: inline-block;vertical-align:top;
                           width: 200px;",
                           numericInput(
                             'bwgwpca',
                             'bw',
                             10,
                             min = 1,
                             max =Inf))),
      numericInput(
        'powergwpca',
        'Power (Minkowski distance)',
        2,
        min = 1,
        max = Inf),
      sliderInput(
        "thetagwpca",
        "Theta (Angle in radians)",
        min = 0,
        max = 2,
        value = 0,
        step = 0.05),
      shinyalert::useShinyalert(),
      shinyjs::useShinyjs(),
      shinyWidgets::actionBttn(
        inputId = "Rungwpca",
        label = "Run",
        style = "float",
        block = TRUE,
        color = "danger")),
    mainPanel(tabsetPanel(tabPanel(
      "Summary",
      helpText(h3("Summary :")),
      verbatimTextOutput("gwpca"),
      DT::DTOutput("DFgwpca"),
      br(),
      helpText(h3("Localised loadings :")),
      div(style="display: inline-block;vertical-align:top; width: 200px;",
          numericInput(
            'loading',
            'Component',
            1,
            min = 1,
            max = Inf)),
      DT::DTOutput("loadinggwpca")),
      tabPanel("Percent Total Variation",
               helpText("click in Dropdown Button
                        for customize and download plot"),
               shinyWidgets::dropdown(
                 tags$h3("List of Input"),
                 numericInput('loadingVgwpca',
                              'Number of components',
                              2,
                              min = 2,
                              max = Inf),
                 textInput("mainVgwpca",
                           label = "Main title",
                           value = "Write main title..."),
                 shinyWidgets::pickerInput(
                   inputId = "colorVgwpca",
                   label = "Select pallete",
                   choices = Pallete_color),
                 helpText(h4("North arrow position")),
                 numericInput("latgwpca",
                              "Latitude",
                              min = -90,
                              max = 90,
                              value = 32
                              ),
                 numericInput("scalegwpca",
                              "scale Arrow",
                              min = 1,
                              max = 10,
                              value = 3
                              ),
                 numericInput("longwpca",
                              "Longitude",
                              min = -180,
                              max = 180,
                              value = -74),
                 shinyWidgets::actionBttn(
                   inputId = "RunplotVgwr",
                   label = "Plot",
                   style = "float",
                   color = "success"),
                 radioButtons("butdownVgwpca",
                              "Select the Option",
                              choices = list("png","pdf")),
                 downloadButton("downVgwpca",
                                label = "Download the plot"),
                 style = "unite",
                 #icon = icon("gear"),
                 status = "primary", width = "300px",
                 animate = shinyWidgets::animateOptions(
                   enter = animations$fading_entrances$fadeInLeftBig,
                   exit = animations$fading_exits$fadeOutRightBig)),
               plotOutput("Vargwpca",
                          width = "100%")
               ),
      tabPanel(
        "Plot winning variable",
        helpText("click in Dropdown Button for customize and download plot"),
        shinyWidgets::dropdown(
          tags$h3("List of Input"),
          numericInput(
            'loadingwingwpca',
            'Components',
            1,
            min = 1,
            max = Inf),
          textInput(
            "mainwingwpca",
            label = "Main title",
            value = "Write main title..."),
          #shinyWidgets::pickerInput(
          # inputId = "colorwingwpca",
          # label = "Select pallete",
          #choices = Pallete_color),
          # helpText(h4("North arrow position")),
          numericInput("latwingwpca",
                       "Latitude",
                       min = -90,
                       max = 90,
                       value = 32),
          numericInput("scalewingwpca",
                       "scale Arrow",
                       min = 1,
                       max = 10,
                       value = 3),
          numericInput("lonwingwpca",
                       "Longitude",
                       min = -180,
                       max = 180,
                       value = -74),
          shinyWidgets::actionBttn(
            inputId = "Runplotwingwpca",
            label = "Plot",
            style = "float",
            color = "success"),
          radioButtons("butdownwingwpca",
                       "Select the Option",
                       choices = list("png","pdf")),
          downloadButton(
            "downwingwpca",
            label = "Download the plot"),
          style = "unite",
          #icon = icon("gear"),
          status = "primary", width = "300px",
          animate = shinyWidgets::animateOptions(
            enter = animations$fading_entrances$fadeInLeftBig,
            exit = animations$fading_exits$fadeOutRightBig)),
        plotOutput("wingwpca",width = "100%"))

                                      )
                ))))),
shinydashboard::tabItem(
  tabName = "tab54",fluidRow(column(width = 12,
  shinydashboard::box(
    width = 12,
    title = "Geographically Weighted Discriminant Analysis",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    height = 66000,
    sidebarPanel(shinyWidgets::actionBttn(
      "helpgwda",
      "Help",
      icon = icon("question-circle"),
      style = "stretch",
      block = FALSE,
      color = "primary"),
      shinyBS::bsModal(
        id="helpgeowda",
        title = "",
        trigger = "helpgwda",
        size="large",
        tags$iframe(
          src = "Help_gwda.html",
          width = "100%",
          height = "1000px",
          frameborder = 0,
          scrolling = "auto" )),
      selectizeInput('dependgwda',
                     'Grouping factor',
                     choices = NULL,
                     multiple = TRUE,
                     options = list(maxItems = 1)),
      selectizeInput('indepentgwda',
                     'Discriminators',
                     choices = NULL,
                     multiple = TRUE),
      shinyWidgets::switchInput("mean.gw",
                                inputId = "meangwda",
                                onLabel = "TRUE",
                                offLabel = "FALSE",
                                size = "mini"),
      shinyWidgets::switchInput(
        "COV.gw",
        inputId = "COVgwda",
        onLabel = "TRUE",
        offLabel = "FALSE",
        size = "mini"),
      shinyWidgets::switchInput(
        "prior.gw",
        inputId = "priorgwda",
        onLabel = "TRUE",
        offLabel = "FALSE",
        size = "mini"),
      shinyWidgets::switchInput(
        "longlat",
        inputId = "longlatgwda",
        onLabel = "TRUE",
        offLabel = "FALSE",
        size = "mini"),
      shinyWidgets::switchInput(
        "wqda",
        inputId = "wqdagwda",
        onLabel = "TRUE",
        offLabel = "FALSE",
        size = "mini"),
      shinyWidgets::switchInput(
        "adaptive",
        inputId = "adaptativegwda",
        onLabel = "TRUE",
        offLabel = "FALSE",
        size = "mini"),
      shinyWidgets::prettyRadioButtons(
        inputId ="selgwda",
        label = "Distance bandwidth:",
        choices = c("Automatic","Manual")
                        ),
      conditionalPanel(condition = "input.selgwda == 'Manual'",
                       div(style="display: inline-block;vertical-align:top;
                           width: 150px;",
                           numericInput('bwgwda',
                                        'bw',
                                        10,
                                        min = 1,
                                        max =Inf))),
      div(style="display: inline-block; vertical-align:top;
          width: 200px;",
          numericInput(
            'powergwda',
            'Power (Minkowski distance)',
            2,
            min = 1,
            max = Inf)),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          shinyWidgets:: pickerInput(
            inputId = "kernelgwda",
            label = "Kernel",
            choices = kernel)),
      div(style="display: inline-block; vertical-align:top; width: 200px;",
          sliderInput(
            "thetagwda",
            "Theta (Angle in radians)",
            min = 0,
            max = 2,
            value = 0,
            step = 0.05)),
      shinyalert::useShinyalert(),
      shinyjs::useShinyjs(),
      shinyWidgets::actionBttn(
        inputId = "Rungwda",
        label = "Run",
        style = "float",
        block = TRUE,
        color = "danger") ),# ffinsidebarpanel54
    mainPanel(tabsetPanel(
      tabPanel(
        "Summary",
        verbatimTextOutput("gwda"),
        helpText(h3("Confusion matrix")),
        verbatimTextOutput("predict"),
        helpText(h3(" SDF")),
        DT::DTOutput("DFgwda")
        ),
      tabPanel(
        "Plot",
        helpText("click in Dropdown Button for customize and download plot"),
        shinyWidgets::dropdown(
          tags$h3("List of Input"),
          selectInput('plotgwda',
                      'Select variable',
                      choices = NULL),
          textInput("maingwda",
                    label = "Main title",
                    value = "Write main title..."),
          shinyWidgets::pickerInput(
            inputId = "colorgwda",
            label = "Select pallete",
            choices = Pallete_color),
          br(),
          helpText(h4("North arrow position")),
          numericInput("latgwda",
                       "Latitude",
                       min = -90,
                       max = 90,
                       value = 32),
          numericInput(
            "longwda",
            "Longitude",
            min = -180,
            max = 180,
            value = -74),
          numericInput(
            "scalegwda",
            "scale North arrow ",
            min = 1,
            max = 20,
            value = 3),
          shinyWidgets::actionBttn(
            inputId = "Runplotgwda",
            label = "Plot",
            style = "float",
            color = "success"),
          radioButtons("butdowngwda",
                       "Select the Option",
                       choices = list("png","pdf")),
          downloadButton(outputId = "downgwda",
                         label = "Download the plot"),
          style = "unite",
          #icon = icon("gear"),
          status = "primary", width = "300px",
          animate = shinyWidgets::animateOptions(
            enter = animations$fading_entrances$fadeInLeftBig,
            exit = animations$fading_exits$fadeOutRightBig)),
        plotOutput("mapgwda"))))
                    )))),
shinydashboard::tabItem(tabName = "tab6",
                        fluidRow(column(width = 12,shinydashboard::box(
                          width = 12,
                          title = "Spatial autocorrelation",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          height = 66000,
                          sidebarPanel(shinyWidgets::actionBttn(
                            "helpauto",
                            "Help",
                            icon = icon("question-circle"),
                            style = "stretch",
                            block = FALSE,
                            color = "primary"),
                            shinyBS::bsModal(id="helpautoc",
                                             title = "",
                                             trigger = "helpauto",
                                             size="large",
                                             tags$iframe(
                                               src = "Help_Autocorrelation.html",
                                               width = "100%",
                                               height = "1000px",
                                               frameborder = 0,
                                               scrolling = "auto" )),
                                          pickerInput('varauto',
                                           'Variable',
                                           choices = NULL
                                           ),
                            br(),
                            helpText(h4("neighbourhood weights list option")),
                            shinyWidgets::switchInput("Zero.policy",
                                                      inputId = "zeropolicy1",
                                                      onLabel = "TRUE",
                                                      offLabel = "FALSE",
                                                      size = "mini"),
                            radioButtons("style",
                                         label = "Style",
                                         choices = list("W" = "W",
                                                        "B" = "B",
                                                        "C" = "C",
                                                        "U"= "U",
                                                        "minmax"= "minmax",
                                                        "S"="S"
                                         ), selected = "W"),
                            br(),
                            helpText(h4("Global and local Moran option")),
                            numericInput("numsim",
                                         label = "Number of permutations",
                                         value = 500),
                            radioButtons("alternative",
                                         label = "alternative",
                                         choices = list("greater"="greater",
                                                        "less"= "less",
                                                        "two.sided"= "two.sided"),
                                         selected = "greater"),
                            shinyalert::useShinyalert(),
                          shinyjs::useShinyjs(),
                          shinyWidgets::actionBttn(
                            inputId = "Runauto",
                            label = "Run",
                            style = "float",
                            block = TRUE,
                            color = "danger")),
                          mainPanel(tabsetPanel(
                            tabPanel(
                              "Summary",
                              br(),
                              verbatimTextOutput("variablename"),
                              br(),
                              helpText( "Moran I test under randomisation"),
                              verbatimTextOutput("moran"),
                              helpText( "Monte-Carlo simulation of Moran I"),
                              verbatimTextOutput("moranmc"),
                              helpText( "Local Moran's I statistic summary"),
                              verbatimTextOutput("localmoran"),
                              helpText( "Monte-Carlo simulation of Local Moran's I statistic summary"),
                              verbatimTextOutput("localmoranperm"),
                              br(),
                              helpText( "Local Moran's I statistic"),
                              DT::DTOutput("DFauto"),
                            ),
                            tabPanel(
                              "Plot",
                              helpText("click in Dropdown Button for customize and download plot"),
                              shinyWidgets::dropdown(
                                tags$h3("List of Input"),
                                selectInput('plotauto',
                                            'Select',
                                            choices = c("lmoran_i" = "lmoran_i",
                                                        "lmoran_p" = "lmoran_p")),

                                shinyWidgets::pickerInput(
                                  inputId = "colorauto",
                                  label = "Select pallete",
                                  choices = Pallete_color),
                                br(),
                                helpText(h4("North arrow position")),
                                numericInput("latauto",
                                             "Latitude",
                                             min = -90,
                                             max = 90,
                                             value = 32),
                                numericInput(
                                  "longauto",
                                  "Longitude",
                                  min = -180,
                                  max = 180,
                                  value = -74),
                                numericInput(
                                  "scaleauto",
                                  "scale North arrow ",
                                  min = 1,
                                  max = 20,
                                  value = 3),
                                shinyWidgets::actionBttn(
                                  inputId = "Runplotauto",
                                  label = "Plot",
                                  style = "float",
                                  color = "success"),
                                radioButtons("butdownauto",
                                             "Select the Option",
                                             choices = list("png","pdf")),
                                downloadButton(outputId = "downgauto",
                                               label = "Download the plot"),
                                style = "unite",
                                #icon = icon("gear"),
                                status = "primary", width = "300px",
                                animate = shinyWidgets::animateOptions(
                                  enter = animations$fading_entrances$fadeInLeftBig,
                                  exit = animations$fading_exits$fadeOutRightBig)),
                              plotOutput("mapauto"))
                          ))

                                        )))

                        )# fintab6
    )#fintabitemS
  )# findashboardbody

################################################################################
shinydashboard::dashboardPage(header, sidebar, body)
