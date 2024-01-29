library(dplyr)
library(ggplot2)
library(shiny)
library(tidyverse)
library(gtsummary)
library (DT)
# library(bslib)
library(shinythemes)
library(grid)
library(plotly)
library(gridExtra) 
# library(reshape)
# library(reshape2)
library(globe4r )

linebreaks <- function(n){HTML(strrep(br(), n))}



# CHANGE FOLDER TO PATH ON YOUR LOCAL MACHINE IF READING FROM DISK
# CHANGE FOLDER TO PATH ON YOUR LOCAL MACHINE IF READING FROM DISK
# CHANGE FOLDER TO PATH ON YOUR LOCAL MACHINE IF READING FROM DISK
# setwd("~/Desktop/HBV_Shiny_App")
# hbvdata = read.csv("DataFilterExport.csv")

# hbvdata = read.csv("https://raw.githubusercontent.com/nishanmudalige/hbv6/main/DataFilterExport.csv")

# hbvdata = read.csv('DataFilterExport.csv')

# Replace additional follow up time with NA
# hbvdata =  hbvdata %>% 
#   mutate(Additional.Follow.up.Time = replace(Additional.Follow.up.Time, Additional.Follow.up.Time<0, NA))
# hbvdata$Additional.Follow.up.Time = ifelse(hbvdata$Additional.Follow.up.Time<0, NA, hbvdata$Additional.Follow.up.Time)
# 
# Zonelist <- unique(hbvdata$Geography)
# Sexlist = unique(hbvdata$Sex)
# Immlist = unique(hbvdata$Immunization.Status)
# Yearlist = unique(hbvdata$Year)
# 
# draw_plot <- function(
#     zone_to_filter_by,
#     zone_title,
#     rd,
#     sex_to_filter_by,
#     imm_to_filter_by) {
#   filtered_wx <- hbvdata %>%
#     # filter(Year == !!year_to_filter_by) %>%
#     filter(Geography == !!zone_to_filter_by) %>%
#     filter(Sex == !!sex_to_filter_by) %>%
#     filter(Immunization.Status == !!imm_to_filter_by)
#   
#   
#   if(rd == "yes"){
#     
#     ggplot(filtered_wx, 
#            aes(x=factor(Year),
#                y=Immunization.Percent,
#                fill = Year)) +
#       coord_cartesian(ylim = c(0, 90)) +
#       stat_summary(fun = "mean", geom = "bar") + 
#       geom_errorbar(aes(ymin=Immunization.Percent-Standard.Error, 
#                         ymax=Immunization.Percent+Standard.Error),
#                     width=.2,
#                     position=position_dodge(.9)) +
#       theme(legend.position="none") +
#       xlab("Year") +
#       ylab("Immunization Percent") +
#       ggtitle(zone_title) +
#       theme(plot.title = element_text(hjust = 0.5))
#     
#   } else {
#     
#     ggplot(filtered_wx, 
#            aes(x=factor(Year),
#                y=Immunization.Percent,
#                fill = Year)) +
#       coord_cartesian(ylim = c(0, 90)) +
#       stat_summary(fun = "mean", geom = "bar") + 
#       theme(legend.position="none") +
#       xlab("Year") +
#       ylab("Immunization Percent") +
#       theme(plot.title = element_text(hjust = 0.5)) +
#       ggtitle(zone_title) +
#       theme(plot.title = element_text(hjust = 0.5))
#     
#   }
#   
# }
# draw_plot2 <- function(city_to_filter_by, 
#                       sex_to_filter_by,
#                       imm_to_filter_by) {
#   
#   filtered_wx <- hbvdata %>%
#     filter(Year == !!city_to_filter_by) %>%
#     filter(Sex == !!sex_to_filter_by) %>%
#     filter(Immunization.Status == !!imm_to_filter_by)
#   
#   ggplot(filtered_wx, 
#          aes(x=factor(Geography), 
#              y=Alberta.Percent,
#              fill = Geography)) +
#     stat_summary(fun = "mean", geom = "bar")
# }


# df = data.frame(A=1:10, B=11:20)



# image_file <- "~/Desktop/App/SilverspottedSculpin/adult.jpg"
# txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")



ui <- fluidPage(
  
  # theme = bs_theme(version = 4, bootswatch = "minty"),
  theme = shinytheme("cerulean"),
  
  collapsible = TRUE,
  
  titlePanel(
    fluidRow(
      column(2, img(# height = 105, width = 300,
                    height = 110, width = 226,
                    src = "https://raw.githubusercontent.com/nishanmudalige/SilverspottedSculpin/main/Images/vancouver_aquarium_logo_white.png")),
      # column(7, align = "left", "The Silverspotted Sculpin"),
      # column(7, align = "left", HTML("<em>(Blepsias cirrhosus)</em>") )
      column(7, align = "left", HTML("The Silverspotted Sculpin 
                                     <br>
                                     <em>(Blepsias cirrhosus)</em>"), 
                                    # Padding to vertically align text
                                    style="padding:20px;" )
      )
    ),
  
  # titlePanel(title=div(img(src="https://raw.githubusercontent.com/nishanmudalige/SilverspottedSculpin/main/Images/vancouver_aquarium_logo_white.png",
  # 
  #                          ),
  #                      )),
  
  # titlePanel(title=div(img(src="https://raw.githubusercontent.com/nishanmudalige/SilverspottedSculpin/main/Images/vancouver_aquarium_logo_white.png"),
  #                      "My Title")),
  
  # titlePanel(
  #   title=
  #   p( 
  #     h1("first sentence", align = "left"),
  #     linebreaks(1),
  #     h3("second sentence", align = "left")
  #   )
  # ),

  # titlePanel(title=div(img(height = 110,
  #                       src="https://raw.githubusercontent.com/nishanmudalige/SilverspottedSculpin/main/Images/vancouver_aquarium_logo_white.png"),
  #                       "\n\n\n\n\n The Silverspotted Sculpin\n(Blepsias cirrhosus)"
  #                      )),
  
  # titlePanel("The Silverspotted Sculpin"),
  # titlePanel(title=div(img(src="https://i.imgur.com/90rbas0.jpeg"), 
  #                      "The Silverspotted Sculpin")),
  
  # uiOutput("img_yoy"),
  # 
  # HTML('<center><img src="https://raw.githubusercontent.com/nishanmudalige/SilverspottedSculpin/main/Images/yoy.jpg" width="400"></center>'),
  # 
  br(),
  h4("Prepared by Nishan Mudalige"),
  h5("Exercise as part of the Application for 
      Interpreter with the Vancouver Aquarium"),
  
  mainPanel("",
            # "main panel",
            # img(src='https://i.imgur.com/90rbas0.jpeg', align = "right"),
            tags$hr(style="border-color: black;"),
            p(" "),
            
            

            h2("Description and Facts"),
            p(" "),

            HTML('<center><img src="https://raw.githubusercontent.com/nishanmudalige/SilverspottedSculpin/main/Images/blepsias_cirrhosus_ssp_rev.jpg"
                 width="800">
                 </center>
                 <center>
                 Copyright, Scott Stevenson Photography (2018)
                 </center>
                 '),

            h3("Appearance"),
            
            tags$div(
              tags$ul(
                tags$li(h4("text"))
              )
            ),
            
            hr(),
            
            h3("Habitat"),
            h4("text goes here"),
            
            hr(),
            
            h3("Location"),
            h4("text goes here"),
            
            hr(),
            
            h3("Diet"),
            h4("text goes here"),


            tags$hr(style="border-color: black;"),

            #life stages
            h2("Life Stages"),
            p(" "),
            
            h3("Egg"),
            
            p(" "),
            h4("text goes here"),
            
            hr(),

            h3("Fry"),
            HTML('<center><img src="https://raw.githubusercontent.com/nishanmudalige/SilverspottedSculpin/main/Images/yoy.jpg"
                 width="200">
                 </center>
                 <center>
                 Copyright, National Oceanic and <br> Atmospheric Administration (2015)
                 </center>
                 '),

            p(" "),
            h4("text goes here"),
            
            hr(),

            h3("Young Juvenille"),
            HTML('<center><img src="https://raw.githubusercontent.com/nishanmudalige/SilverspottedSculpin/main/Images/young_juvenile.jpg"
                 width="350">
                 </center>
                 <center>
                 Copyright, National Oceanic and <br> Atmospheric Administration (2015)
                 </center>
                 '),

            p(" "),
            h4("text goes here"),
            
            hr(),

            h3("Mature Juvenille"),
            HTML('<center><img src="https://raw.githubusercontent.com/nishanmudalige/SilverspottedSculpin/main/Images/mature_juvenile.jpg"
                 width="500">
                 </center>
                 <center>
                 Copyright, National Oceanic and <br> Atmospheric Administration (2015)
                 </center>
                 '),

            p(" "),
            h4("text goes here"),
            
            hr(),

            h3("Adult"),
            HTML('<center><img src="https://raw.githubusercontent.com/nishanmudalige/SilverspottedSculpin/main/Images/adult.jpg"
                 width="650">
                 </center>
                 <center>
                 Copyright, National Oceanic and <br> Atmospheric Administration (2015)
                 </center>
                 '),
            
            p(" "),
            h4("text goes here"),


            tags$hr(style="border-color: black;"),

            
            # Interactive plot
            h2("Interactive plot"),
            h4("Click on the bubbles to find out more"),
            
            div(
              
            plot_ly() %>%
              
              add_trace(
                
                x = c(1, 3.5, 1.475, 1.6, 1.75, 1.9, 2.25, 2.25, 2.70),
                y = c(1, 5.5, 2.750, 4.2, 1.00, 2.3, 4.50, 2.25, 3.50),
                
                text = c("", 
                         "", 
                         "The silverspotted sculpin has a moderate head with eyes located near the top of its head.\nThis is similar with other sculpins which also have eyes placed high, however other\nsculpins have a larger head.",
                         "a",
                         "b",
                         "c",
                         "d",
                         "e",
                         "f"),
                
                hoverinfo = 'text',
                
                marker = list(size = 75,
                              color = "white",
                              # opacity = c(0, 0.5, 0.5, 0.5)
                              opacity = c(0, 0, 
                                          0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
                              
                ),
                showlegend = F,
                
                type = 'scatter',
                mode = 'markers'
                
              ) %>%
              
              config(displayModeBar = F) %>% 
              
              layout(
                
                autosize=TRUE,
                width=1600,
                height=800,
                
                xaxis = list(# autorange = TRUE,
                             range=c(1, 3),
                             fixedrange=TRUE,
                             xaxis_autorange=FALSE,
                             # xaxis_autorange=TRUE,
                             showgrid = FALSE,
                             showticklabels = FALSE,
                             zeroline = FALSE
                ),
                
                yaxis = list(# autorange = TRUE,
                             range=c(0,5),
                             fixedrange=TRUE,
                             # # yaxis_autorange=FALSE
                             # yaxis_autorange=TRUE,
                             showgrid = FALSE,
                             showticklabels = FALSE,
                             zeroline = FALSE
                ),
                
                images = list(
                  list(
                    source = "https://raw.githubusercontent.com/nishanmudalige/SilverspottedSculpin/main/Images/natgeo_sculpin.jpg",
                    xref = "x",
                    yref = "y",
                    x = 1,
                    y = 5,
                    sizex = 2,
                    sizey = 5,
                    sizing = "stretch",
                    opacity = 1,
                    layer = "below"
                  )
                  
                )
                
              ) %>% layout(height = 650, width = 1000)
            
            ),
            
            # p(HTML('<center>
            #       Copyright National Geographics
            #       </center>')),
            # Empty space
            linebreaks(14),
            
            HTML("<center>Copyright, National Geographics (2020)</center>"),

            tags$hr(style="border-color: black;"),
            
            
            
            
            h2("Skeleton"),
            # h4("Click on the bubbles to find out more"),
            
            p(" "),
            
            HTML('<center><img src="https://raw.githubusercontent.com/nishanmudalige/SilverspottedSculpin/main/Images/skeleton.jpeg"
                 width="800">
                 </center>
                 <center>
                 Copyright, Leo Smith University of Kansas,<br>Biodiversity Institute (2014)
                 </center>
                 '),
            
            
            
            tags$hr(style="border-color: black;"),
            
            
            

            # Videos
            h2("Videos"),
            h4("Click on a video to find out more"),

            tabPanel("",

                     div(
                       style =
                         "height: 470;
                          background-color: rgba(50,126,172, 0.75);
                          width: 100%;
                          position: relative; right:0;",

                        tags$iframe(width="425",
                                    height="225",
                                    src="https://www.youtube.com/embed/xZeKe0ulRFE?si=7FVIfhy8ByFKYN2q",
                                    frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope;
                                                picture-in-picture", allowfullscreen=NA),

                          # Some horizontal space between videos
                          HTML("&nbsp &nbsp &nbsp &nbsp &nbsp"),

                          tags$iframe(width="425",
                                     height="225",
                                     src="https://www.youtube.com/embed/-Po1VWE3Uvk?si=J5FGiAnS_YDPYi8a",
                                     frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope;
                                                picture-in-picture", allowfullscreen=NA),

                          br(),
                          br(),

                          tags$iframe(width="425",
                                     height="225",
                                     src="https://www.youtube.com/embed/SpoVFJfLxNM?si=q7vy6blkikepaK_z",
                                     frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope;
                                                  picture-in-picture", allowfullscreen=NA),

                          # Some horizontal space between videos
                          HTML("&nbsp &nbsp &nbsp &nbsp &nbsp"),

                          tags$iframe(width="425",
                                   height="225",
                                   src="https://www.youtube.com/embed/M_GexCkEcHw?si=TiUVAMDgqO_w95dk",
                                   frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope;
                                                  picture-in-picture", allowfullscreen=NA)

                     ),
                     style="text-align:center"),

            h4("text goes here"),

            hr(),
            # br(),
            # div(
            #   style = "width: 70%; margin: auto;",
            #   h4(HTML("Lorem Ipsum is simply dummy text of the printing and typesetting industry.
            #                           Lorem Ipsum has been the industry's standard dummy text ever since the 1500s"),
            #      style="text-align:justify")),
            
            h4("Text goes here"),
            
            
      tags$hr(style="border-color: black;"),
      globeOutput("globe")
            
  )
)



server <- function(input, output) {
  
  # globe
  output$globe <- render_globe({
    create_globe() %>% 
      globe_img_url(image_url("blue")) %>%
      globe_bars(coords(lat, long, color = mag), data = quakes) %>%
      scale_bars_color() %>%
      globe_pov(-21, 179)
  })
}

shinyApp(ui = ui, server = server)
