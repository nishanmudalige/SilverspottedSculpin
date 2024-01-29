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
library(reshape)
library(reshape2)


linebreaks <- function(n){HTML(strrep(br(), n))}



# CHANGE FOLDER TO PATH ON YOUR LOCAL MACHINE IF READING FROM DISK
# CHANGE FOLDER TO PATH ON YOUR LOCAL MACHINE IF READING FROM DISK
# CHANGE FOLDER TO PATH ON YOUR LOCAL MACHINE IF READING FROM DISK
# setwd("~/Desktop/HBV_Shiny_App")
# hbvdata = read.csv("DataFilterExport.csv")

hbvdata = read.csv("https://raw.githubusercontent.com/nishanmudalige/hbv6/main/DataFilterExport.csv")

# hbvdata = read.csv('DataFilterExport.csv')

# Replace additional follow up time with NA
# hbvdata =  hbvdata %>% 
#   mutate(Additional.Follow.up.Time = replace(Additional.Follow.up.Time, Additional.Follow.up.Time<0, NA))
hbvdata$Additional.Follow.up.Time = ifelse(hbvdata$Additional.Follow.up.Time<0, NA, hbvdata$Additional.Follow.up.Time)

Zonelist <- unique(hbvdata$Geography)
Sexlist = unique(hbvdata$Sex)
Immlist = unique(hbvdata$Immunization.Status)
Yearlist = unique(hbvdata$Year)

draw_plot <- function(
    zone_to_filter_by,
    zone_title,
    rd,
    sex_to_filter_by,
    imm_to_filter_by) {
  filtered_wx <- hbvdata %>%
    # filter(Year == !!year_to_filter_by) %>%
    filter(Geography == !!zone_to_filter_by) %>%
    filter(Sex == !!sex_to_filter_by) %>%
    filter(Immunization.Status == !!imm_to_filter_by)
  
  
  if(rd == "yes"){
    
    ggplot(filtered_wx, 
           aes(x=factor(Year),
               y=Immunization.Percent,
               fill = Year)) +
      coord_cartesian(ylim = c(0, 90)) +
      stat_summary(fun = "mean", geom = "bar") + 
      geom_errorbar(aes(ymin=Immunization.Percent-Standard.Error, 
                        ymax=Immunization.Percent+Standard.Error),
                    width=.2,
                    position=position_dodge(.9)) +
      theme(legend.position="none") +
      xlab("Year") +
      ylab("Immunization Percent") +
      ggtitle(zone_title) +
      theme(plot.title = element_text(hjust = 0.5))
    
  } else {
    
    ggplot(filtered_wx, 
           aes(x=factor(Year),
               y=Immunization.Percent,
               fill = Year)) +
      coord_cartesian(ylim = c(0, 90)) +
      stat_summary(fun = "mean", geom = "bar") + 
      theme(legend.position="none") +
      xlab("Year") +
      ylab("Immunization Percent") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle(zone_title) +
      theme(plot.title = element_text(hjust = 0.5))
    
  }
  
}
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



            tags$hr(style="border-color: black;"),

            #life stages
            h2("Life Stages"),
            p(" "),

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
            h3("Interactive plot"),
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
            

            # Videos
            h3("Videos"),
            h4("Click on a video to find out more"),

            # HTML('<iframe width="560" height="315"
            #   <div class="box">
            #   <iframe src="https://embed.spotify.com/?uri=spotify:user:1290230929:playlist:6nTIVNGZfnZ4urUiwHIgpT" frameborder="0" scrolling="no" width="100%" height="512" align="left"> </iframe>
            #   </div>
            #
            #   <div class="box">
            #   <iframe src="https://embed.spotify.com/?uri=spotify:user:1285279066:playlist:56KI83cMiMTOocIdXq2R5j" frameborder="0" scrolling="no" width="100%" height="512" align="right"></iframe>
            #   </div>
            # </iframe>'),

            # HTML('<iframe width="560" height="315"
            #      src="https://www.youtube.com/embed/xZeKe0ulRFE?si=7FVIfhy8ByFKYN2q"
            #      frameborder="0" allow="accelerometer;
            #      autoplay;
            #      encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
            #

            # background-color: #02BE7F;
            tabPanel("",

                     div(
                       style =
                         "height: 470;
                          background-color: rgba(50,126,172, 0.75);
                          width: 100%;
                          position: relative; right:0;",

                        tags$iframe(width="450",
                                    height="225",
                                    src="https://www.youtube.com/embed/xZeKe0ulRFE?si=7FVIfhy8ByFKYN2q",
                                    frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope;
                                                picture-in-picture", allowfullscreen=NA),

                          # Some horizontal space between videos
                          HTML("&nbsp &nbsp &nbsp &nbsp &nbsp"),

                          tags$iframe(width="450",
                                     height="225",
                                     src="https://www.youtube.com/embed/-Po1VWE3Uvk?si=J5FGiAnS_YDPYi8a",
                                     frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope;
                                                picture-in-picture", allowfullscreen=NA),

                          br(),
                          br(),

                          tags$iframe(width="450",
                                     height="225",
                                     src="https://www.youtube.com/embed/SpoVFJfLxNM?si=q7vy6blkikepaK_z",
                                     frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope;
                                                  picture-in-picture", allowfullscreen=NA),

                          # Some horizontal space between videos
                          HTML("&nbsp &nbsp &nbsp &nbsp &nbsp"),

                          tags$iframe(width="450",
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
            
            h4("Text goes here")


            
            
            # p(" "),
            # h5("Please use the drop down menus below to filter by gender and immunizaton status."),
            
            # inputPanel(
            #   selectInput(
            #     "PlotSex",
            #     label = "Select Sex",
            #     choices = Sexlist
            #   ),
            #   selectInput(
            #     "PlotImm",
            #     label = "Select Immunization Status",
            #     choices = Immlist
            #   ),
            #   
            #   radioButtons("rd", "Error bars",
            #                c("With error bars" = "yes",
            #                  "No error bars" = "no"))
            # ),
            
            # fluidRow(
            #   splitLayout(cellWidths = c("33%", "33%", "33%"), 
            #               plotOutput("minplot"), 
            #               plotOutput("minplot1"),
            #               plotOutput("minplot2"))
            # ),
            # 
            # fluidRow(h1("\n")),
            # 
            # fluidRow(
            #   splitLayout(cellWidths = c("33%", "33%", "33%"), 
            #               plotOutput("minplot3"), 
            #               plotOutput("minplot4"),
            #               plotOutput("minplot5")),
            #   
            #   p(""),
            #   strong("Source: http://www.ahw.gov.ab.ca/IHDA_Retrieval")
            # ),
            
            
            # tags$hr(style="border-color: black;"),
          
            # h5("Please use the drop down menus to filter by year, geographic zone and immunizaton status."),
            
            # fluidRow(
            #   DT::dataTableOutput("vax_table"),
            #   
            #   p("Note the following:"),
            #   p("AB = Province of Alberts"),
            #   p("Z1 = South"),
            #   p("Z2 = Calgary"),
            #   p("Z3 = Central"),
            #   p("Z4 = Edmonton"),
            #   p("Z5 = North"),
            #   
            #   strong("Source: http://www.ahw.gov.ab.ca/IHDA_Retrieval"),
            #   
            #   tags$hr(style="border-color: black;"),
            #   
            #   h4("Summary (99 words)"),
            #   h5("We observe that vaccination rates for completely vaccinated grade 6 students were high in 2018-2019, 
            #         2021-2022 and 2022-2023 with approximately 70% - 80% of students being vaccinated across Alberta in 
            #         these 3 years. However, vaccination rates were low in 2019-2020 and 2020-2021 with under 40% vaccinated 
            #         across Alberta. However, many students were still partially vaccinated during this time."),
            #   h5(""),
            #   h5("The highest vaccination rates have historically been in Zone 2 (Calgary). 
            #         The lowest proportion of unvaccinated students was observed in Zone 1 (South) in 2020-2021."),
            #   h5(""),
            #   h5("The data also suggest that approximately the same proportion of males and 
            #     females are completely vaccinated."),
            #   
            #   tags$hr(style="border-color: black;"),
            #   
            #   
            #   h4("Limitations (95 words)"),
            #   h5("The additional follow up time is not available for 2022-2023 (indicated in the data set by -99). 
            #        This is the time period during which clients are monitored after vaccination (intervention). "),
            #   h5(""),
            #   h5("Additional follow up time could be used to monitor vaccine efficacy, observe side-effects or 
            #         detect safety concerns, obtain information related to the duration of immunity, 
            #         assist with the management, delivery and preparation of vaccination schedules, 
            #         and also provide information related to real-world effectiveness outside of a clinical trial."),
            #   h5(""),
            #   h5("In order to keep the comparisons between years consistent, this variable was not included in the analysis."),
            #   
            #   
            #   tags$hr(style="border-color: black;"),
            #   
            #   plotOutput("table_plot")
            # )
            
  )
)



server <- function(input, output) {

  output$img_yoy <- renderUI({
    # tags$img(src = "https://www.r-project.org/logo/Rlogo.png")
    tags$img(src = "https://raw.githubusercontent.com/nishanmudalige/SilverspottedSculpin/main/Images/yoy.jpg",
             align = "center", 
             style = paste0("width: 25%; height: 25%;")
             )
    
  })
  
  output$minplot <- 
    renderPlot(
      draw_plot( zone_to_filter_by="AB",
                 zone_title = "Alberta",
                 input$rd,
                 input$PlotSex, 
                 input$PlotImm))
  output$minplot1 <- 
    renderPlot(draw_plot( zone_to_filter_by="Z1",
                          zone_title = "Zone 1 (South)",
                          input$rd,
                          input$PlotSex, 
                          input$PlotImm))
  output$minplot2 <- 
    renderPlot(draw_plot( zone_to_filter_by="Z2",
                          zone_title = "Zone 2 (Calgary)",
                          input$rd,
                          input$PlotSex, 
                          input$PlotImm))
  
  
  output$minplot3 <- 
    renderPlot(draw_plot( zone_to_filter_by="Z3",
                          zone_title = "Zone 3 (Central)",
                          input$rd,
                          input$PlotSex, 
                          input$PlotImm))
  output$minplot4 <- 
    renderPlot(draw_plot( zone_to_filter_by="Z4",
                          zone_title = "Zone 4 (Edmonton)",
                          input$rd,
                          input$PlotSex, 
                          input$PlotImm))
  output$minplot5 <- 
    renderPlot(draw_plot( zone_to_filter_by="Z5",
                          zone_title = "Zone 5 (North)",
                          input$rd,
                          input$PlotSex, 
                          input$PlotImm))
  
  # out_tab = datatable(cast(hbvdata, Year + Geography + Immunization.Status ~ Sex)) %>% 
  #   formatRound(c(4:6), 2)
  
  # out_tab = dcast(hbvdata, Year + Geography + Immunization.Status ~ Sex)
  
  out_tab =  dcast(hbvdata, Year + Geography + Immunization.Status ~ Sex, value.var="Immunization.Percent")
  
  out_tab = data.frame(out_tab)
  
  out_tab$Year = factor(out_tab$Year)
  out_tab$Geography	= factor(out_tab$Geography)
  out_tab$Immunization.Status = factor(out_tab$Immunization.Status)
  
  out_tab$Both = round(out_tab$Both, digits=2)
  out_tab$Male = round(out_tab$Male, digits=2)
  out_tab$Female = round(out_tab$Female, digits=2)
  
  output$vax_table <- renderDT(data.frame(out_tab),
                               filter = "top",
                               options = list(
                                 autoWidth = TRUE,
                                 columnDefs = list(list(width = '100px', targets = 1)),
                                 pageLength = 20
                               )
                               
  )
  
  
}

shinyApp(ui = ui, server = server)
