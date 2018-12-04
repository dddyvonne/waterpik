# Function to update the hospital table
update_hospital_table <- function(output, hospital) {
  
  output$table_hospitals <- DT::renderDataTable({
    datatable(hospital[,c(1:5)], 
              rownames = FALSE, 
              selection = "single",
              options = list("searching" = FALSE,
                             "scrollX" = TRUE,
                             "info" = FALSE))
  })
  
}

update_dentist_table <- function(output, dentist){ 
  output$table_dentists <- DT::renderDataTable({
    datatable(dentist[,c(1:4)],
              rownames = FALSE,
              selection = "single",
              options = list("searching" = FALSE,
                             "scrollX" = TRUE,
                             "info" = FALSE))
  }
  )
}








# Function to update the IDN Table
update_idn_table <- function(output, idn) {
  
  output$table_idns <- DT::renderDataTable({
    datatable(idn[, -2], 
              rownames = FALSE, 
              selection = "single",
              options = list("searching" = FALSE,
                             "scrollX" = TRUE,
                             "info" = FALSE))
  })
}

# Function to update Hospital contact information
#update_hospital_contact_table <- function(output, hospitals) {
  
#  output$table_hospital_contact <- DT::renderDataTable({
#   datatable(hospitals[, -1], 
#              rownames = FALSE, 
#              escape = FALSE,
#              selection = "single",
#              options = list("searching" = FALSE,
#                             "scrollX" = TRUE,
#                             "info" = FALSE))
#  })
  
#}

update_idn_contact_table <- function(output, idns) {
  
  output$table_idn_contact <- DT::renderDataTable({
    datatable(idns[, -1], 
              rownames = FALSE, 
              escape = FALSE,
              selection = "single",
              options = list("searching" = FALSE,
                             "scrollX" = TRUE,
                             "info" = FALSE))
  })
  
}

# Function to update map markers with hospitals
update_map <- function(output, hospital) {
  
  leafletProxy("mymap") %>%
    clearMarkers() %>%
    fitBounds(-79.1563,
              43.6195,
              -79.6369,
              43.8501) %>%

    addMarkers(
      data = hospital,
      layerId = ~Name,
      popup = ~Name,
      label = ~Name
    )
}

server <- function(input, output, session) {
  
  # Remove the useless <li> created by navbar header
  shinyjs::runjs("$('.navbar-nav li').first().remove();")
  
 reactive_vals <- reactiveValues(dentist_filter = dentist_df,
                                 hospital_filter = hospitals_df)
  
 output$input_rating <- renderUI({
   #choice <- (c("All", sort(unique(hospitals_df$rating)))
   selectInput(
     inputId = "input_rating", 
     label = "Rating",
     choices = sort(unique(hospitals_df$rating)),
     selected = sort(unique(hospitals_df$rating)))
   
 })
 
 output$input_score <- renderUI({
   checkboxGroupInput(
     inputId = 'input_score',
     label = 'Score',
     choices = sort(unique(hospitals_df$Score)),
     selected = sort(unique(hospitals_df$Score)),
     inline = T,
     width = '100%'
   )
 })
 
 output$input_name <- renderUI({
   
   available <- c('All Offices',hospitals_df[hospitals_df$Score %in% input$input_score,]$Name)
  
   selectInput(
     inputId = "input_name", 
     label = "Name",
     choices = unique(available),
     selected = NULL)
   
 })
 
 output$reset_filter <- renderUI({
   reset('input_name')
   reset('input_score')
 })
   #reactive_vals <- reactiveValues(idn_filter = idn_df,
   #                               hospital_filter = hospitals_df)
  
  
  # Initial update with hospital and IDN tables
  update_hospital_table(output, hospitals_df)
  update_dentist_table(output, dentist_df[dentist_df$`Office Name`!= '',])
  #update_idn_table(output, idn_df)
  #update_hospital_contact_table(output, hospital_contacts)
  #update_idn_contact_table(output, idn_contacts)
  
  
  
  observeEvent(input$input_filter_button, {
    
    # Clear Map Markers
    leafletProxy("mymap") %>%
      clearMarkers()
    
    #update_map(hospitals_df)
    #if (length(input$input_idn) == 0)
    #  idn_name <- idn_df$IDN
    #else
    #  idn_name <- input$input_idn
    
    #if (input$input_rating == '')
    #  idn_sales <- unique(hospitals_df$rating)
    #else
    #  idn_sales <- input$input_rating
    
    if(input$input_name == 'All Offices')
      hospitals_df<- hospitals_df[hospitals_df$Score %in% input$input_score,]
    else
      hospitals_df<- hospitals_df[hospitals_df$Name == input$input_name,]
      
    
    # Filter the hospital Table
   filter_hospital <- hospitals_df %>%
     filter(#rating %in% input$input_rating,
            #address %in% input$input_states,
            if(input$input_name == "All Offices")
            Score %in% input$input_score
            else
            Name %in% input$input_name,
            Score %in% input$input_score
            )
            
            
    
    
    
    
    #filter idn table
     #filter_idn <- idn_df %>%
    #  filter(`QuVA Customer` %in% input$input_current_customer,
     #        IDN %in% idn_name,
      #       `Final Tier` %in% input$input_tier,
       #      `Number of Hospitals` >= input$input_num_hospitals[1],
        #     `Number of Hospitals` <= input$input_num_hospitals[2],
         #    `Sales Rep` %in% idn_sales,
          #   `Total Beds` >= input$input_num_beds[1],
           #  `Total Beds` <= input$input_num_beds[2])
    
    
    # Filter dentist by office name
    filter_dentist <- dentist_df[dentist_df$`Office Name` %in% filter_hospital$Name,]
    #filter_hospital <- hospitals_df[hospitals_df$rating %in% filter_idn$`Definitive IDN ID`, ]

    
    #if (length(input$input_states) != 0) {
    #  s <- state2abbr(input$input_states)
    #  filter_hospital <- hospitals_df[hospitals_df$State %in% s, ]
      #filter_idn <- filter_idn[filter_idn$`Definitive IDN ID` %in% filter_hospital$`Definitive IDN ID`, ]
  #  }
    
    #filter_idn_contacts <- idn_contacts[idn_contacts$`Definitive IDN ID` %in% filter_idn$`Definitive IDN ID`, ]
    
    # Update reactive values to know which ones are showing
    reactive_vals$hospital_filter <- filter_hospital
    reactive_vals$dentsit_filter <- filter_dentist
    
    # Update hopistal contacts
    filter_hospital_contact <- hospital_contacts[hospital_contacts$`Definitive ID` %in% filter_hospital$`Definitive ID`,]
    
    # Update the tables
    update_hospital_table(output, filter_hospital)
    update_dentist_table(output,filter_dentist)
    update_map(output,filter_hospital)

  })
  
  
  # Toronto GTA Map
  output$mymap <- renderLeaflet({
    leaflet()  %>%
     fitBounds(-79.1563,
               43.6195,
               -79.6369,
               43.8501)%>%

      addTiles() %>%
      addMarkers(
        data = hospitals_df,
        layerId = ~Name,
        popup = ~Name,
        label = ~Name
      )
  })
  #updateCheckboxInput(sessions, 'box_A', value = TRUE)
  #updateCheckboxInput(sessions, 'box_B', value = TRUE)
  
  
  #observeEvent(input$table_idns_rows_selected,{
   # update
    #idn_id <- reactive_vals$idn_filter$`Definitive IDN ID`[input$table_idns_rows_selected]
    
   # idn_contact <- idn_contacts[idn_contacts$`Definitive IDN ID` == idn_id, ]
  #  hos <- hospitals[hospitals$`Definitive IDN ID` == idn_id, ]
    
   # reactive_vals$hospital_filter <- hos
    #hos_contact <- hospital_contacts[hospital_contacts$`Definitive ID` %in% hos$`Definitive ID`, ]
    
  #  update_hospital_table(output, hos)
  #  update_idn_contact_table(output, idn_contact)
  #  update_hospital_contact_table(output, hos_contact)
  #  update_map(output, hos)
  #})
  
  # Update contact information when clicking on a hospital
 
  
  observeEvent(input$mymap_marker_click, {
    hospital_con <- hospitals_df[hospitals_df$Name == input$mymap_marker_click$id, ]
    dentist_click <- dentist_df[dentist_df$`Office Name` == hospital_con$Name, ]
    update_hospital_table(output, hospital_con)
    update_dentist_table(output, dentist_click)
  })

  
  
   observeEvent(input$table_hospitals_rows_selected, {
    dentist_office_name <- reactive_vals$hospital_filter$Name[input$table_hospitals_rows_selected]
    dentist_con <- dentist_df[dentist_df$`Office Name` == dentist_office_name, ]
    update_dentist_table(output, dentist_con)
    
  })
  
   
  # observeEvent(input$reset_filter, {
  #   #input$input_score = sort(unique(hospitals_df$Score))
  #   checkboxGroupInput(
  #     inputId = 'input_score',
  #     label = 'Score',
  #     choices = sort(unique(hospitals_df$Score)),
  #     selected = sort(unique(hospitals_df$Score)),
  #     inline = T,
  #     width = '100%'
  #   )
  #   update_hospital_table(output, hospitals_df)
  #   update_dentist_table(output, dentist_df)
  #   update_map(output,hospitals_df)
  # })
}
