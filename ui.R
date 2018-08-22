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
community_area_df <- read.csv("CommAreas.csv")
community_area_key <- dplyr::select(community_area_df, c(AREA_NUMBE, COMMUNITY))
community_area_key

# Validate that Primary.Type are unique
unique(crimes$Primary.Type)

# Clean Primary.Type values with the same meaning using gsub which replaces all matches with a string.
crimes$Primary.Type <- gsub("(.*)NARCOTIC(.*)","NARCOTICS",crimes$Primary.Type)

# **** Gets the Top 10 Crime types For plot 1 *****
bold.text <- element_text(face = "bold", color = "black")

crimes.top10 <- cbind(crimes,quantity=seq(1,1,nrow(crimes)))
agg.crimes <- aggregate(quantity~Year+Primary.Type,data=crimes.top10, FUN=sum)
agg.crimes.modified <- aggregate(quantity~Primary.Type, data=agg.crimes, FUN=mean)
agg.crimes.modified <- agg.crimes.modified[order(agg.crimes.modified$quantity, decreasing=TRUE),]
top10.crimes.summed <- agg.crimes.modified[1:10,]
top10.crimes.summed

#For Plot 2
top10.plot2 <- agg.crimes.modified[1:10,]
subaggdata2 <- agg.crimes[which(agg.crimes$Primary.Type %in% top10.plot2$Primary.Type),]
subaggdata2

#For Plot 3
#Some records lack the information about "Community.Area". The ratio of such missing data is calculated:
areadata <- crimes.top10[which(crimes.top10$Community.Area != ""),]
areaMissRatio <- 1 - nrow(areadata)/nrow(crimes.top10)
areaMissRatio

#Data with missing "Community.Area" are removed. The ranking is based on the total occurrence of all crimes from 2001-present.
aggdata_area <- aggregate(quantity~Year+Community.Area,data=areadata,FUN=sum)
aggdata_area2 <- aggregate(quantity~Community.Area,data=aggdata_area,FUN=sum)
aggdata_area2 <- aggdata_area2[order(aggdata_area2$quantity,decreasing=TRUE),]
top10_area <- aggdata_area2[1:10,]
top10_area

# For plot 4
top3_area <- aggdata_area2[1:3,]
subaggdata_area2 <- aggdata_area[which(aggdata_area$Community.Area %in% top3_area$Community.Area),]

# for plot 5
#possible plot 5 - charts all crime types not just top 10.
# Mike change variables up and also look and feel 
crime_17years <-
  crimes %>% 
  #dplyr::filter(Crime.Type == "Violent") %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(Year = as.integer(as.character(Year))) %>% 
  group_by(Primary.Type, Year) %>% 
  summarize(reported_incidents = n()) %>% 
  group_by(Primary.Type) %>% 
  mutate(total_years = n()) %>% 
  filter(total_years > 15)


#munge data
if (!exists("crime_df")){
  crime_munged <-
    crimes %>%
    data.table() %>%
    filter(Year < 2018) %>%
    left_join(community_area_key, by = c("Community.Area" = "AREA_NUMBE")) %>%
    mutate(
      Year = as.ordered(Year),
      Crime.Type = as.factor(
        ifelse(Primary.Type %in% c("ROBBERY", "BURGLARY",
                                   "CRIM SEXUAL ASSAULT", "HOMICIDE", "HUMAN TRAFFICKING",
                                   "BATTERY", "SEX OFFENSE", "KIDNAPPING",
                                   "DOMESTIC VIOLENCE", "ASSAULT", "ARSON", "INTIMIDATION"),
               "Violent", "Nonviolent"))
    ) %>%
    dplyr::select(
      c(Crime = Primary.Type, Crime.Type, Arrest, Domestic,
        Year, Community.Area))
}

total_crime_17years <-
  crime_munged %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Year = as.integer(as.character(Year))) %>%
  group_by(Crime.Type, Year) %>%
  summarize(reported_incidents = n())

unique(top10.crimes.summed$Primary.Type)

#For Map 6
crimes.murders <- subset(crimes, Primary.Type == "HOMICIDE")

state.data <- data.frame(state=tolower(rownames(state.x77)),state.x77)

#states.map <- map_data("state")
states.map <- maps::map("state", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% fortify()

#county.map <- map_data("county")
county.map <- ggplot(states.map)
county.map$data$region
#illinois.counties <- subset(county.map,region=="illinois")
illinois.counties <- subset(county.map, county.map$data$region =="illinois")
#illinois_df <- subset(states.map, region == "illinois")
illinois_df <- subset(county.map, county.map$data$region == "illinois")

bin_crimes <- crimes %>% group_by(Primary.Type, Longitude, Latitude) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>% ungroup()
bin_crimes

bins_crimes.subset <- subset(crimes, Primary.Type=="HOMICIDE" | Primary.Type=="BATTERY" | Primary.Type=="KIDNAPPING" | Primary.Type=="BURGLARY")
bin_crimes2 <- bins_crimes.subset %>% group_by(Primary.Type, Longitude, Latitude) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>% ungroup()


#set decimal points to 2 decimals
scaleFUN <- function(x) sprintf("%.2f", x)




# shiny dashboard 

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

ui <- dashboardPage(
  dashboardHeader(title = "Chicago Crime '02-'17"),
  dashboardSidebar( tags$style(HTML(".main-sidebar{width: 220px;}")),
                    sidebarMenu(id = 'sidebarmenu',
                                menuItem('Dashboard', tabName = '', icon = icon('th'),
                                         menuItem('Charts',
                                              tabName = '',
                                              icon = icon('line-chart'),
                                                  menuSubItem('Home',
                                                          tabName = 'dashboard',
                                                          icon = icon('th')),
                                                  menuSubItem('Top 10 Crime Types',
                                                          tabName = 'chart1',
                                                          icon = icon('bar-chart-o')),
                                                  menuSubItem('Crime Types Over Time',
                                                          tabName = 'chart2',
                                                          icon = icon('line-chart')),
                                                  menuSubItem('Crime Types Over Time 2',
                                                          tabName = 'chart3',
                                                          icon = icon('line-chart')),
                                                  menuSubItem('Community Crime 1',
                                                          tabName = 'chart4',
                                                          icon = icon('line-chart')),
                                                  menuSubItem('Community Crime 2',
                                                          tabName = 'chart5',
                                                          icon = icon('line-chart'))
                                                  ),
                                         
                                         menuItem('Maps', tabName = '', icon = icon('map'),
                                                  menuSubItem('Home',
                                                              tabName = 'dashboard',
                                                              icon = icon('th')),
                                                  menuSubItem('Density Map',
                                                                  tabName = 'map1',
                                                                  icon = icon('map')),
                                                  menuSubItem('Violent Crime Map',
                                                              tabName = 'map2',
                                                              icon = icon('map'))
                                                  ),
                                         menuItem('Tables',
                                                  tabName = 'tables',
                                                  icon = icon('th'))
                                ))
  ),
  
  dashboardBody(
    
    tabItems(
      #Dashboard tab
      tabItem(tabName = "dashboard", 
              fluidRow(
                column(12, includeHTML("intro6.html"))
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
                  column(8, withSpinner(plotOutput(outputId="plot3")))
              )
      ),
      #Chart 4 tab
      tabItem(tabName = "chart4",
              fluidRow(
                column(8, withSpinner(plotOutput(outputId="plot4"))),
                column(4, sliderInput("yearInput2", "Year", 2001, 2017, c(2001, 2017)))
              )
      ),
      #Chart 5 tab
      tabItem(tabName = "chart5",
              fluidRow(
                column(8, withSpinner(plotOutput(outputId="plot5")))
              )
      ),
      #Map 1 tab
      tabItem(tabName = "map1",
              fluidRow(
                column(8, withSpinner(plotOutput(outputId="map1")))
              )
      ),
      #Map 2 tab
      tabItem(tabName = "map2",
              fluidRow(
                column(8, withSpinner(plotOutput(outputId="map2")))
              )
      ),
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
    subaggdata_area2 %>%
      filter(Year >= input$yearInput2[1],
             Year <= input$yearInput2[2]
      )
  })
  
  
  output$plot1 <- renderPlot({
  
    #Top 10 crimes
    ggplot(data=top10.crimes.summed, aes(x=reorder(Primary.Type,-quantity,sum),y=quantity,fill=Primary.Type)) +
      geom_bar(stat="identity") + 
      ggtitle("Top 10 Crime Types") +
      theme_dark() + 
      labs(x = "", 
           y = "No. of Incidents",
           subtitle = "Chicago crime type by number of incidents",
           caption = "Source: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2/data") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      theme(title = bold.text, axis.title = bold.text) +
      theme(legend.position="none")
    
  })
  
  output$plot2 <- renderPlot({
    
    ggplot(data=filtered.chart2() ,aes(x=Year,y=quantity, color=Primary.Type)) +
      geom_line() + 
      ggtitle("Top 10 Crime Types Over Time") + 
      theme_dark() + 
      labs(x = "Year",
           y = "Incidents",
           color = "",
           subtitle = "Crime from 2001 to 2017",
           caption = "Source: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2/data") +
      #theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      theme(title = bold.text, axis.title = bold.text) 
  })
  
  output$plot3 <- renderPlot({
    
    ggplot(data = crime_17years, 
           aes(x = Year, y = reported_incidents)) + 
      geom_line(size = 1.5, color = "red") + 
      facet_wrap(~Primary.Type, scales = "free") + 
      ggtitle("Crime Over Time by Type") + 
      theme_dark() + 
      scale_y_continuous(name = "Incidents", labels = scales::comma) +
      scale_x_discrete(label = abbreviate) 
    
  })
  
  output$plot4 <- renderPlot({
    
    ggplot(data=filtered.chart4(), aes(x=Year,y=quantity)) + 
      facet_grid(.~Community.Area) + 
      geom_point(aes(size=2,col=quantity)) + 
      geom_line(aes(group=1, col=quantity)) + 
      ggtitle("Most Dangerous Communities Over Time") + 
      theme_dark() + 
      labs(x = "Year",
           y = "Incidents",
           subtitle = "Top 3 crime communities from 2001 to 2017",
           caption = "Source: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2/data") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      #theme(title = bold.text, axis.title = bold.text) +
      theme(legend.position="none")
  })
  
  output$plot5 <- renderPlot({
    
    ggplot(data=top10_area, aes(x=reorder(Community.Area,-quantity,sum),y=quantity, fill=Community.Area)) +
      geom_bar(stat="identity") + 
      ggtitle("Top 10 Crime Communities") +
      theme_dark() + 
      labs(x = "Community", 
           y = "Incidents",
           subtitle = "Chicago's most dangerous communities",
           caption = "Source: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2/data") +
      theme(title = bold.text, axis.title = bold.text) +
      theme(legend.position="none")
    
    
  })
  
  output$map1 <- renderPlot({
    
    #map 1 - Density Map
    ggplot(data = illinois_df, mapping = aes(x = long, y = lat)) +
      coord_fixed(1.3) +
      geom_polygon(aes(group=group),color = "black", fill = "gray")  +
      geom_polygon(data = illinois.counties, aes(group=group),fill = NA, color = "white") +
      geom_polygon(aes(group=group),color = "black", fill = NA) +
      stat_density2d(data = bin_crimes, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 1, bins = 50, geom = 'polygon') +
      coord_map(xlim = c(-88, -87.5),ylim = c(41.5,42.2)) +
      theme_dark() +
      scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') +
      scale_alpha(range = c(.2, .3), guide = FALSE) +
      guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) +
      ggtitle("Chicago Crime") +
      xlab("Longitude") +
      ylab("Latitude") +
      labs(subtitle = "Violent crime in Chicago",
           caption = "Source: Merrimack College")
    
  })
  
  output$map2 <- renderPlot({
    
    #map 2 - Maps violent crime in Chicago
    ggplot(data = illinois_df, mapping = aes(x = long, y = lat)) +
      coord_fixed(1.3) +
      geom_polygon(aes(group=group),color = "black", fill = "gray")  +
      geom_polygon(data = illinois.counties, aes(group=group),fill = NA, color = "white") +
      geom_polygon(aes(group=group),color = "black", fill = NA) +
      geom_point(aes(x = Longitude, y = Latitude, colour = Primary.Type),
                                data = bin_crimes2, alpha=0.5) +
      theme_dark() +
      labs(title="Mapping Crimes in Chicago",
           subtitle="Violent crime in Chicago",
           x = "Longitude",
           y = "Latitude",
           color = "") +
      coord_map(xlim = c(-88, -87.5),ylim = c(41.5,42.2))

    
  })
  
  output$table <- renderDataTable(crimes, options = list(scrollX = TRUE))
}
#options(shiny.fulstacktrace = TRUE)
shinyApp(ui, server)
