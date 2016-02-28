library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)




smartcities <- read.csv("smartcities.csv")

smartcities$State.UT <- as.character(smartcities$State.UT)
smartcities$Status <- as.character(smartcities$Status)

ui <- navbarPage(theme = "bootstrap.min4.css", 
                 
                 titlePanel(HTML('<a style="color:#FF9933; font-size:32px; font-family:Verdana; line-height:0.4; text-decoration:none;" href="https://avinashr.shinyapps.io/Smart_Cities_India">Smart Cities Mission</a>'), windowTitle = "Smart Cities Mission"),
                 
                 tabPanel(tags$h3(tags$b("About")),
                          
                          div(HTML('<center><img src="Logo.jpg" width="300" height="400"></center>'),
                              br(),
                              HTML('<center><a style="color:#FF9933; font-size:20px;" target="_blank" href="http://smartcities.gov.in/SmartCitiesPPT/Smart%20City%20Logo.jpg">Image Source: smartcities.gov.in</a></center>'),
                              br(),
                              HTML('<p style="font-size:32px; font-family:Verdana;"align="justify"><h2>About Smart Cities Mission</h2></p>'),
                              HTML('<p style="font-size:24px; font-family:Verdana;"align="justify">&nbsp;&nbsp;&nbsp;&nbsp;Smart Cities Mission is an urban renewal and retrofitting program by the Government of India with a mission to develop 100 cities all over the country making them citizen friendly and sustainable. The Union Ministry of Urban Development is responsible for implementing the mission in collaboration with the state governments of the respective cities. The government of India under Prime Minister Narendra Modi has a vision of developing 100 smart cities as satellite towns of larger cities and by modernizing the existing mid-sized cities."</p>'),
                              HTML('<p style="font-size:24px; font-family:Verdana;"align="justify">&nbsp;&nbsp;&nbsp;&nbsp;The 100 potential smart cities were nominated by all the states and union territories based on Stage 1 criteria, prepared smart city plans which were evaluated in stage 2 of the competition for prioritizing cities for financing. In the first round of this stage, 20 top scorers were chosen for financing during 2015-16. The remaining will be asked to make up the deficiencies identified by the Apex Committee in the Ministry of Urban Development for participation in the next two rounds of competition. 40 cities each will be selected for financing during the next rounds of competition."</p>'),
                              HTML('<a style="color:#FF9933; font-size:20px;" target="_blank" href="https://en.wikipedia.org/wiki/Smart_Cities_Mission">Source: Wikipedia</a>'),
                              br(),
                              br(),
                              HTML('<p style="font-size:32px; font-family:Verdana;"align="justify"><h2>About the Smart Cities Mission App</h2></p>'),
                              HTML('<p style="font-size:24px; font-family:Verdana;"align="justify">&nbsp;&nbsp;&nbsp;&nbsp;This App has been created by <a style="color:#FF9933;"target="_blank" href="http://www.avinash.social">Avinash Manure</a> as a personal project in Data Visualization. It has been programmed in R language and uses the Shiny library for the visualization. It is not the official Website/App for the Smart Cities Program and uses data from different government/non-government Websites as a primary data source.</p>')
                              , style = 'max-width:1000px;width:98%;margin:auto;')),       
                 
                 
                 
                 
                 tabPanel(tags$h3(tags$b("Selection Process")),
                          div(HTML('<center><img src="Selection.PNG" width=auto></center>'),
                              HTML('<center><a style="color:#FF9933; font-size:20px;" target="_blank" href="http://smartcities.gov.in/writereaddata/Process%20of%20Selection.pdf">Source: smartcities.gov.in</a></center>')
                              
                          ), style = 'overflow-x: scroll;'),
                 
                 tabPanel(tags$h3(tags$b("Map")),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("states", selected = "All", label = tags$h2("State"),
                                          choices = c("All", sort(smartcities$State.UT))), radioButtons("shortlisted", selected = "Round One (Total: 97)", tags$h2("List of Smart Cities"), c("Round One (Total: 97)" = "all", "Selected Cities (Total: 20)" = "selected", "On Fast Track Upgrade (Total: 23)" = "upgrade", "Round Two (Total: 54)" = "two")),
                              htmlOutput("source", label = "Sources")
                            ),
                            mainPanel(
                              leafletOutput("map", height = 654)
                            )
                          )
                 ),
                 
                 tabPanel(tags$h3(tags$b("Table")),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("states1", selected = "All", label = tags$h2("State"),
                                          choices = c("All", sort(smartcities$State.UT))), radioButtons("shortlisted1", selected = "Round One (Total: 97)", tags$h2("List of Smart Cities"), c("Round One (Total: 97)" = "all", "Selected Cities (Total: 20)" = "selected", "On Fast Track Upgrade (Total: 23)" = "upgrade", "Round Two (Total: 54)" = "two")),
                              htmlOutput("source1", label = "Sources")
                            ),
                            div(mainPanel(
                              dataTableOutput("table")
                            ), style = 'overflow-x: scroll;')
                          )
                          
                          
                          
                          
                          
                          
                 ))



server <- function(input, output, session) {
  
  output1 <- reactive({
    a <- filter(smartcities, State.UT == input$states, Status == input$shortlisted)
    
    a
  })
  
  output2 <- reactive({
    b <- filter(smartcities, State.UT == input$states, All == "all")
    
    b
  })
  
  output3 <- reactive({
    c <- filter(smartcities, Status == input$shortlisted)
    
    c
  })
  
  output4 <- reactive({
    d <- filter(smartcities, State.UT == input$states1, Status == input$shortlisted1)
    
    d
  })
  
  output5 <- reactive({
    e <- filter(smartcities, State.UT == input$states1, All == "all")
    
    e
  })
  
  output6 <- reactive({
    f <- filter(smartcities, Status == input$shortlisted1)
    
    f
  })
  
  
  
  output$map <- renderLeaflet(
    
    if(input$shortlisted == "all" & input$states != "All")(
      
      
      leaflet(data = output2()) %>% setView(lng = 80, lat = 20, zoom = 5) %>% addTiles() %>% addMarkers(~Longitude, ~Latitude, popup = ~paste(sep = "<br/>", paste(tags$b("City:"), City), paste(tags$b("Population:"), Population), paste(tags$b("Score:"), Score))))
    
    else(
      
      if(input$shortlisted == "all" & input$states == "All")
        
        (leaflet(data = smartcities) %>% setView(lng = 80, lat = 20, zoom = 5) %>% addTiles() %>% addMarkers(~Longitude, ~Latitude, popup = ~paste(sep = "<br/>", paste(tags$b("City:"), City), paste(tags$b("Population:"), Population), paste(tags$b("Score:"), Score))))
      
      
      
      else(
        
        if(input$shortlisted != "all" & input$states == "All")
          
          (leaflet(data = output3()) %>% setView(lng = 80, lat = 20, zoom = 5) %>% addTiles() %>% addMarkers(~Longitude, ~Latitude, popup = ~paste(sep = "<br/>", paste(tags$b("City:"), City), paste(tags$b("Population:"), Population), paste(tags$b("Score:"), Score)))) 
        
        
        
        else(
          
          
          leaflet(data = output1()) %>% setView(lng = 80, lat = 20, zoom = 5) %>% addTiles() %>% addMarkers(~Longitude, ~Latitude, popup = ~paste(sep = "<br/>", paste(tags$b("City:"), City), paste(tags$b("Population:"), Population), paste(tags$b("Score:"), Score))))
      )))
  
  
  output$table <- renderDataTable(
    
    if(input$shortlisted1 == "all" & input$states1 != "All")(datatable(output5()[c("Rank", "State.UT", "City", "Score", "Population")], options = list(autoWidth = TRUE , columnDefs = list(list(className = 'dt-center', targets = c(1:5))), pageLength = 15, lengthMenu = c(10, 15))))
    
    else(
      
      if(input$shortlisted1 == "all" & input$states1 == "All")
        
        (datatable(smartcities[c("Rank", "State.UT", "City", "Score", "Population")], options = list(autoWidth = TRUE , columnDefs = list(list(className = 'dt-center', targets = c(1:5))), pageLength = 15, lengthMenu = c(10, 15))))
      
      else(
        
        if(input$shortlisted1 != "all" & input$states1 == "All")
          
          (datatable(output6()[c("Rank", "State.UT", "City", "Score", "Population")], options = list(autoWidth = TRUE , columnDefs = list(list(className = 'dt-center', targets = c(1:5))), pageLength = 15, lengthMenu = c(10, 15))))
        
        
        else(
          
          (datatable(output4()[c("Rank", "State.UT", "City", "Score", "Population")], options = list(autoWidth = TRUE , columnDefs = list(list(className = 'dt-center', targets = c(1:5))), pageLength = 15, lengthMenu = c(10, 15))))
        ))))
  
  
  
  
  
  
  
  
  
  output$source <-  renderUI(
    
    
    
    tags$h6(tags$p("PS: Click on the markers on the map for more information about the Cities"),
            tags$br(),    
            
            tags$h2(tags$p("Data/Information Sources")),
            
            
            HTML('<a style="color:#FF9933; font-size:20px;" target="_blank" href="https://en.wikipedia.org/wiki/Smart_Cities_Mission">Wikipedia</a>'),
            br(),
            HTML('<a style="color:#FF9933; font-size:20px;" target="_blank" href="http://smartcities.gov.in/">smartcities.gov.in</a>'),
            br(),
            HTML('<a style="color:#FF9933; font-size:20px;" target="_blank" href="http://www.censusindia.gov.in/">www.censusindia.gov.in</a>'),
            br(),
            br(),
            tags$h2(tags$p("Created By")),
            HTML('<a style="color:#FF9933; font-size:20px;" target="_blank" href="http://www.avinash.social">Avinash Manure</a>'),
            br(),
            br()
            
    )
    
  )
  
  
  
  output$source1 <-  renderUI(
    
    
    
    tags$h2(tags$p("Data/Information Sources"),
            HTML('<a style="color:#FF9933; font-size:20px;" target="_blank" href="https://en.wikipedia.org/wiki/Smart_Cities_Mission">Wikipedia</a>'),
            br(),
            HTML('<a style="color:#FF9933; font-size:20px;" target="_blank" href="http://smartcities.gov.in/">smartcities.gov.in</a>'),
            br(),
            HTML('<a style="color:#FF9933; font-size:20px;" target="_blank" href="http://www.censusindia.gov.in/">www.censusindia.gov.in</a>'),
            br(),
            br(),
            tags$h2(tags$p("Created By")),
            HTML('<a style="color:#FF9933; font-size:20px;" target="_blank" href="http://www.avinash.social">Avinash Manure</a>'),
            br(),
            br(),
            br()
            
    )
  )
  
  
  
  
  
}

shinyApp(ui, server)