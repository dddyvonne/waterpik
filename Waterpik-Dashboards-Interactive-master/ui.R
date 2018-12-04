ui <- navbarPage(
  useShinyjs(),
  title = div(img(src = "Alchemy_logo_RBG.png", width = "100px")),
  tags$head(includeCSS("styles.css")),
  theme = "bootstrap.css",
  windowTitle = "Waterpik Dashboard",
  selected = "Dentist",
  tabPanel("Dentist",
           div(
             id = "cluster_div",
             div(
               id = "cluster_right",
               leafletOutput("mymap", height = 500),
               hr(),
               fluidRow(
                 #tags$h3("IDNs"),
                 column(1),
                 #column(10, DT::dataTableOutput('table_idns')),
                 column(1)
               ),
               hr(),
               fluidRow(
                 tags$h3("Offices"),
                 column(1),
                 column(10, DT::dataTableOutput('table_hospitals')),
                 column(1)
               ),
               hr(),
               fluidRow(
                 tags$h3("Dentist Information"),
                 column(1),
                 column(10, DT::dataTableOutput('table_dentists')),
                 column(1)
               ),
               hr(),
               fluidRow(
                 #tags$h3("Hospital Contact Information"),
                 column(1),
                # column(10, DT::dataTableOutput('table_hospital_contact')),
                 column(1)
               ),
               br(),
               br(),
               br(),
               br(),
               br()
             ),
             div(
               id = "cluster_left",
               absolutePanel(
                 id = "cluster_filters",
                 class = "panel",
                 fixed = TRUE,
                 top = 60,
                 left = 20,
                 width = "300",
                 bottom = "auto",
                 right = "auto",
                 div(
                   id = "cluster_sliders_div",
          
                  uiOutput('input_score'),
                   br(),
                  uiOutput('input_name'),
                   br()
                  ),
                 div(
                   id = 'filter_button_div',
                   hr(),
                   column(
                     12,
                     align = "center",
                     actionButton("input_filter_button",
                                  "Filter Offices")
                   )
                 )
             ))
           )
  )
)
