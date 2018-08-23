#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



library(shinydashboard)
library(shinycssloaders)
library(data.table)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinythemes)
library(maps)
library(xts)


dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)


#######################   The following lines are commented out because Shiny won't allow me to upload such a large dataset. ################
### Therefore I created a random sample of the data from the dataset and included years 2002 - 2017.
### I left of 2001 and 2018 because I don't think the data contained a full year.

# Reads crime dataset in from 2001 - present (file contains over 6 million observations)
# crimes.original <- read.csv("Crimes_-_2001_to_present.csv", stringsAsFactors = TRUE)

# Take a random 100000 sample of the crimes dataset because Shiny doesn't allow very large files.
# crimes.sample.100000 <- crimes.original[sample(nrow(crimes), 100000), ]

# Only save the variables I'll be using for this project to save memory on Shiny
# crimes.sample.100000.subset <- subset(crimes.sample.100000, Year > 2001 & Year <2018, 
#                                        select=c(ID, Date, IUCR, Primary.Type, Location.Description, Arrest, Domestic, 
#                                                 Beat, District, Ward, Community.Area, X.Coordinate, Y.Coordinate, Year, 
#                                                 Latitude, Longitude, Location))


# Write sample csv file to hard drive for later publishing to Shiny.
# Comment next line before I deploy to shiny
# write.csv(crimes.sample.100000.subset, "crimes_sample_100000_subset.csv")
###################    End of code Shiny won't allow me to do because of server size #########################################################


crimes.sample.100000.subset <- read.csv("crimes_sample_100000_subset.csv")

# Remove 2018 from the dataset because a full year of data didn't exist
crimes.sample.100000.subset.2002.2017 <- subset(crimes.sample.100000.subset, Year < 2018)

# Set dataframe to new dataframe
crimes <- crimes.sample.100000.subset.2002.2017

# Read in community dataset to link area_number and community
community.area.df <- read.csv("CommAreas.csv")
community.area.key <- dplyr::select(community.area.df, c(AREA_NUMBE, COMMUNITY))

# Validate that Primary.Type are unique
unique(crimes$Primary.Type)

# Clean Primary.Type values with the same meaning using gsub which replaces all matches with a string.
crimes$Primary.Type <- gsub("(.*)NARCOTIC(.*)","NARCOTICS",crimes$Primary.Type)

# **** Gets the Top 10 Crime types For plot 1 *****
bold.text <- element_text(face = "bold", color = "black")

# Code for Plot 1
crimes.top10 <- cbind(crimes,quantity=seq(1,1,nrow(crimes)))
agg.crimes <- aggregate(quantity~Year+Primary.Type,data=crimes.top10, FUN=sum)
agg.crimes.modified <- aggregate(quantity~Primary.Type, data=agg.crimes, FUN=mean)
agg.crimes.modified <- agg.crimes.modified[order(agg.crimes.modified$quantity, decreasing=TRUE),]
top10.crimes.summed <- agg.crimes.modified[1:10,]

#code For Plot 2
top10.plot2 <- agg.crimes.modified[1:10,]
subaggdata2 <- agg.crimes[which(agg.crimes$Primary.Type %in% top10.plot2$Primary.Type),]

# code For Plot 3
# Use dply to all crimes by year, grouped by primary.type and year, summed up.
crime.all.years <-
  crimes %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(Year = as.integer(as.character(Year))) %>% 
  group_by(Primary.Type, Year) %>% 
  summarize(reported_incidents = n()) %>% 
  group_by(Primary.Type) %>% 
  mutate(total_years = n()) %>% 
  filter(total_years > 15) %>%
  filter(Primary.Type == 'ARSON' | Primary.Type == 'ASSAULT' | Primary.Type =='BATTERY' | Primary.Type == 'BURGLARY' | Primary.Type == 'CRIMINAL DAMAGE' | 
           Primary.Type == 'CRIMINAL TRESPASS' | Primary.Type == 'KIDNAPPING' | Primary.Type == 'LIQUOR LAW VIOLATION' | Primary.Type == 'MOTOR VEHICLE THEFT' | 
           Primary.Type == 'NARCOTICS' | Primary.Type == 'OTHER OFFENSE' | Primary.Type == 'PROSTITUTION' | Primary.Type == 'ROBBERY' | 
           Primary.Type == 'SEX OFFENSE' | Primary.Type == 'STALKING' | Primary.Type == 'THEFT')



# code for plot 4
crimes.arrest <- crimes %>% group_by(Year, Arrest) %>% summarize(count = n()) %>% arrange(Year)
crimes.arrest.true <- subset(crimes.arrest, Arrest == "true")
crimes.arrest.true


#Code For Maps
state.data <- data.frame(state=tolower(rownames(state.x77)),state.x77)

states.map <- map_data("state")
#states.map <- maps::map("state", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% fortify()

county.map <- map_data("county")
#county.map <- ggplot(states.map)

illinois.counties <- subset(county.map,region=="illinois")
illinois.df <- subset(states.map, region == "illinois")

#set decimal points to 2 decimals
scaleFUN <- function(x) sprintf("%.2f", x)



# shiny dashboard Code

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

ui <- dashboardPage(skin = "red", 
  dashboardHeader(title = "Chicago Crime '02-'17"),
  dashboardSidebar( tags$style(HTML(".main-sidebar{width: 220px;}")),
                    sidebarMenu(id = 'sidebarmenu',
                                menuItem('Dashboard', tabName = '', icon = icon('refresh'),
                                         menuItem('Charts',
                                              tabName = '',
                                              icon = icon('line-chart'),
                                                  menuSubItem('Home',
                                                          tabName = 'dashboard',
                                                          icon = icon('refresh')),
                                                  menuSubItem('Top 10 Crime Types',
                                                          tabName = 'chart1',
                                                          icon = icon('bar-chart-o')),
                                                  menuSubItem('Crime Types Over Time',
                                                          tabName = 'chart2',
                                                          icon = icon('line-chart')),
                                                  menuSubItem('Crime Types Over Time 2',
                                                          tabName = 'chart3',
                                                          icon = icon('line-chart')),
                                                  menuSubItem('Arrests Over Time',
                                                          tabName = 'chart4',
                                                          icon = icon('line-chart'))
                                         ),
                                         menuItem('Maps', 
                                                  tabName = 'maps', 
                                                  icon = icon('map')
                                         ),
                                         menuItem('Dataset',
                                                  tabName = 'tables',
                                                  icon = icon('th')
                                         )
                                ))
  ),
  
  dashboardBody(
    
    tabItems(
      #Dashboard tab
      tabItem(tabName = "dashboard", 
              fluidRow(
                column(12, includeHTML("intro10.html"))
              )
      ),
      
      #Charts tab
      tabItem(tabName = "chart1",
              fluidRow(
                column(8, withSpinner(plotOutput(outputId="plot1")))
              )
      ),
      
    
      #chart 2 tab
      tabItem(tabName = "chart2",
              fluidRow(
                column(8, withSpinner(plotOutput(outputId="plot2"))),
                column(4, sliderInput("yearInput", "Year", 2001, 2017, c(2001, 2017)))
              )
      ),
      
      #Chart 3 tab
      tabItem(tabName = "chart3",
              fluidRow(
                  column(12, withSpinner(plotOutput(outputId="plot3")))
              )
      ),
      # #Chart 4 tab
      tabItem(tabName = "chart4",
              fluidRow(
                column(8, withSpinner(plotOutput(outputId="plot4"))),
                column(4, sliderInput("yearInput2", "Year", 2001, 2017, c(2001, 2017)))
              )
      ),
      #Map tab
      tabItem(tabName = "maps",
              fluidRow(
                # Commented out and using an HTML file to show the maps because the code does not work in Shiny
                # column(6, withSpinner(plotOutput(outputId="map1"))),
                # column(6, withSpinner(plotOutput(outputId="map2")))
                column(12, includeHTML("maps.html"))
              )
      ),
      #Table tab
      tabItem(tabName = "tables",
              fluidRow(
                column(12,
                       dataTableOutput('table')
                )
              )
      )
    )
  )
)


server <- function(input, output) {
  

  filtered.chart2 <- reactive({
    subaggdata2 %>%
      filter(Year >= input$yearInput[1],
             Year <= input$yearInput[2]
      )
  })
  
  filtered.chart4 <- reactive({
    crimes.arrest.true %>%
      filter(Year >= input$yearInput2[1],
             Year <= input$yearInput2[2]
      )
  })
  
  
  output$plot1 <- renderPlot({
  
    #Top 10 crimes
    ggplot(data=top10.crimes.summed, aes(x=reorder(Primary.Type,-quantity,sum),y=quantity,fill=Primary.Type)) +
      geom_bar(stat="identity") + 
      ggtitle("Top 10 Crime Types") +
      theme_linedraw() + 
      labs(x = "", 
           y = "No. of Incidents",
           subtitle = "Chicago crime type by number of incidents",
           caption = "Source: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2/data") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0)) +
      theme(legend.position="none") +
      theme(title = bold.text, axis.title = bold.text)
    
  })
  
  output$plot2 <- renderPlot({
    
    ggplot(data=filtered.chart2() ,aes(x=Year,y=quantity, color=Primary.Type)) +
      geom_line(size = 1.0) + 
      ggtitle("Top 10 Crime Types Over Time") + 
      theme_linedraw() +  
      labs(x = "Year",
           y = "Incidents",
           color = "",
           subtitle = "Crime from 2001 to 2017",
           caption = "Source: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2/data") +
      #theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      theme(title = bold.text, axis.title = bold.text) +
      scale_x_continuous(breaks = seq(2001, 2017, by = 2))
    
  })
  
  output$plot3 <- renderPlot({
    
    ggplot(data = crime.all.years, 
           aes(x = Year, y = reported_incidents)) + 
      geom_line(size = 1.0, color = "red") + 
      facet_wrap(~Primary.Type, scales = "free") + 
      ggtitle("Crime Type Over Time") + 
      theme_linedraw() +
      labs(x = "Year",
           y = "Incidents",
           color = "",
           subtitle = "Crime Types rom 2001 to 2017",
           caption = "Source: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2/data") +
      scale_y_continuous(name = "Incidents", labels = scales::comma) +
      scale_x_discrete(label = abbreviate) +
      theme(title = bold.text, axis.title = bold.text) + 
      scale_x_continuous(breaks = seq(2001, 2017, by = 5))
    
  })
  
  output$plot4 <- renderPlot({
    
    ggplot(data=filtered.chart4() ,aes(x=Year,y=count, color=Arrest)) +
      geom_line(size = 1.0) +
      ggtitle("Arrests Over Time") +
      theme_linedraw() + 
      labs(x = "Year",
           y = "Incidents",
           #color = "Arrest",
           subtitle = "Incidents resulting in arrests from 2001 - 2017",
           caption = "Source: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2/data") +
      theme(legend.position="none") +
      theme(title = bold.text, axis.title = bold.text) +
      scale_x_continuous(breaks = seq(2001, 2017, by = 2))
    
  })
  
  #Note: This code works locally but not on Shiny
  output$map1 <- renderPlot({
    crimes.murders <- subset(crimes, Primary.Type=="BATTERY")
    crimes.murders.2002 <- subset(crimes.murders, Year == 2002)
    
    
      ggplot(data = illinois.df, mapping = aes(x = long, y = lat)) +
        coord_fixed(1.3) +
        geom_polygon(aes(group=group),color = "black", fill = "gray")  +
        geom_polygon(data = illinois.counties, aes(group=group),fill = NA, color = "white") +
        geom_polygon(aes(group=group),color = "black", fill = NA) +
        geom_point(data=crimes.murders.2002, aes(x=Longitude, y=Latitude, color="red"), size=.5, alpha=1) +
        coord_map(xlim = c(-88, -87.5),ylim = c(41.5,42.2)) +
        ggtitle("Violent Crime in 2002") +
        theme_linedraw() +
        xlab("Longitude") +
        ylab("Latitude") +
        scale_x_continuous(labels = scaleFUN) +
        labs(subtitle = "Number of Battery Incidents",
           caption = "Source: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2/data") +
        theme(legend.position="none")
    
  })
  
  #Note: This code works locally but not on Shiny
  output$map2 <- renderPlot({
    crimes.murders <- subset(crimes, Primary.Type=="BATTERY")
    crimes.murders_17 <- subset(crimes.murders, Year == 2017)
    
    ggplot(data = illinois.df, mapping = aes(x = long, y = lat)) +
      coord_fixed(1.3) +
      geom_polygon(aes(group=group),color = "black", fill = "gray")  +
      geom_polygon(data = illinois.counties, aes(group=group),fill = NA, color = "white") +
      geom_polygon(aes(group=group),color = "black", fill = NA) +
      geom_point(data=crimes.murders_17, aes(x=Longitude, y=Latitude, color="red"), size=.5, alpha=1) +
      coord_map(xlim = c(-88, -87.5),ylim = c(41.5,42.2)) +
      ggtitle("Violent Crime in 2017") +
      theme_linedraw() +
      xlab("Longitude") +
      ylab("Latitude") +
      scale_x_continuous(labels = scaleFUN) +
      labs(subtitle = "Number of Battery Incidents",
           caption = "Source: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2/data") +
      theme(legend.position="none")
    
  })
  
  output$table <- renderDataTable(crimes, options = list(scrollX = TRUE))
}

shinyApp(ui, server)
