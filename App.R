#Import packages
remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
Yinstall.packages('shiny')
install.packages("shinythemes")
install.packages('magrittr')
install.packages('plotly')
install.packages('shinyWidgets')
install.packages('shinydashboard')
install.packages('RColorBrewer')
install.packages('leaflet.extras')
install.packages('htmltools')
install.packages("geojsonio")
install.packages("tidyverse")
install.packages('maps')
install.packages('rgdal')
install.packages('mapview')
install.packages('rsconnect')
install.packages('svglite')
install.packages('plotly')
install.packages('gganimate')
install.packages('mgcv')
install.packages('cluster')
install.packages('rattle')
install.packages('mclust')
install.packages('zoo')
install.packages('visreg')
install.packages('plyr')

library(shiny)
library(plyr)
library(visreg)
library(mclust)
library(cluster)
library(rattle)
library(shinythemes)
library(ggplot2)
library(magrittr)
library(dplyr)
library(leaflet)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(RColorBrewer)
library(leaflet.extras)
library(htmltools)
library(sf)
library(geojsonio)
library(tidyverse)
library(maps)
library(rgdal)
library(mapview)
library(svglite)
library(plotly)
library(gganimate)
library(data.table)
library(mgcv)
library(zoo)


shinyWidgetsGallery()



#------------------------------------App----------------------------------

ui <- navbarPage(title='', collapsible=TRUE,theme=shinytheme('paper'),
                


#MAP TAB
#-----
              tabPanel('Map',
                          div(class="outer",
                              tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0, }"),
                              leafletOutput("map", width = "100%", height = "100%"),
 #CONTROL PANEL
#----
                              absolutePanel(
                                id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 110, left = 60,
                                right = "auto", bottom = "auto",
                                width = 0, height = 0,
                                
                                dropdownButton(
                                  label = "gear",
                                  status="custom",
                                  icon = icon("gear"),
                                  circle = TRUE,
                                  width = 250,
                                  
                                  awesomeCheckboxGroup(
                                    inputId = "plot_age",
                                    selected="25-34 years",
                                    label = "Select Age Group(s)",
                                    choices = c("5-14 years","15-24 years","25-34 years","35-54 years","55-74 years","75+ years"),
                                    status="primary",
                                  ),
                                  
                                  checkboxGroupButtons(
                                    inputId = "gender",
                                    selected="male",
                                    label = "Select Gender",
                                    choices = c("male","female"),
                                    status="secondary"
                                  ),
                                  
                                  sliderInput("plot_year", "Select Years", value=c(2000,2013),min=min(df$year),max=max(df$year),sep = "",step=1))),
                              
#----                              
#DRAGABLE/GRAPH PANEL
#----
                              absolutePanel(      id = "graphs", top = 65, right = 40, width = 400, fixed=TRUE,
                                                  draggable = TRUE, height = "auto",style='padding:5px',
                                                  tabBox(width="100%", height="auto",
                                                         
                                                  tabPanel(title = tagList(shiny::icon("globe-europe")),
                                                  textOutput("world_trend_title"),
                                                  textOutput("world_trend_subtitle"),
                                                  plotlyOutput("world_trend",height="200px"),
                                                  textOutput("world_rank_title"),
                                                  textOutput("world_rank_subtitle"),
                                                  div(style='height:100px; overflow-y: scroll',plotlyOutput("world_rank",height="900px"))),
                                                  
                                                  tabPanel(title="Selected country",
                                                  span(tags$i(h6("Click on a country on the map to show the number of suicides per 100,000 through the years."))),
                                                  plotlyOutput("curve_plot", height="200px", width="100%"),
                                                  textOutput('selected_text')),
                                                  
                                                  tabPanel(title="By gender",div(style='height:200px; overflow-y: scroll',
                                                  plotOutput("gender_prop",width="100%",height="900px"))),
                                                  
                                                  tabPanel(title="By age",div(style='height:200px; overflow-y: scroll',
                                                  plotOutput("age_prop",width="100%",height="900px")))
                                                  
                                                  )
                                               
                                                  
                                                  
                              ), 
                              
                              
                              
                              tags$style(type = "text/css", "
html, body {background-color:#D4DADC;width:100%;height:100%}

#graphs{background-color:white;opacity:0.9}

#world_trend_title,#world_rank_title{color:#2455A4;
                                 font-size: 16px;
                                 font-weight: bold;
}
#world_trend_subtitle,#world_rank_subtitle{font-style:italic}

.btn-custom {background-color: #9251BD; color: #FFF;}
.btn:hover { background-color: #FFF; color: #9251BD;}
.btn:focus { background-color: #FFF; color: #9251BD;}

.navbar-default {background-color:#D4DADC; border-color:#D4DADC;box-shadow: none!important;}
   
"
                                     
                              ))),
#MODEL TAB
#-----
                 tabPanel('Model',
                          
                          
                          absolutePanel(
                            id = "controls_model",  fixed = TRUE,
                            draggable = TRUE, top = 110, left = 25,
                            right = "auto", bottom = "auto",
                            width = "20%", 
                            
                            pickerInput(
                              inputId = "model_countries",
                              label = "Pick countries:", 
                              choices = levels(df$countries),
                              selected='Russia',
                              options = list(size = 5)
                            ),
                            pickerInput(
                              inputId = "variable1",
                              label = "Select a variable", 
                              selected="gdp_for_year....",
                              choices = names(df)
                            ),
                            sliderTextInput(
                              inputId = "k1",
                              label = "Choose a number of basis functions for the selected variable", 
                              choices = 1:20,
                              selected=9,
                              grid = TRUE
                            ),
                             ),
                          
                          absolutePanel(
                            id = "model_panel",  fixed = TRUE,
                            draggable = TRUE, top = 110, right=80, left="auto", bottom = "auto",
                            width = "65%",
                            plotOutput('model_plot'),
                            textOutput('model_output')
                            ),
                          
tags$head(tags$style(
HTML('
#model_panel {background-color: white}')))

),
#-----

#CLUSTER TAB
#-----
                    tabPanel('Cluster',
                             
                             fluidRow(
                               
                               column(width=4,
                                           leafletOutput("clustermap",width='100%',height='200px')),
                               column(width=8)
                               
                             ),
                             
                             fluidRow(
                               column(width=4,
                               tabBox(width='100%',
                                      tabPanel(title ='1',plotOutput("gam1")),
                                      tabPanel(title ='2',plotOutput("gam2")),
                                      tabPanel(title ='3',plotOutput("gam3")),
                                      tabPanel(title ='4',plotOutput("gam4"))
                               )),
                               column(width=2,title='Controls'),
                               column(width=6)
                               )
                             
                             
                               ),
#-----
                    tabPanel('View Dataset')
                    
                          
                          )
                 


server <- function(input, output, session){


##-------------------------Reactive Functions MAP-------------------------  
  

  
  reactive_map_data=reactive({
      req(input$gender)
      req(input$plot_year)
      req(input$plot_age)
      df %>% 
      filter( sex %in% input$gender &
              age %in% input$plot_age &
              year <= input$plot_year[2] &
              year >= input$plot_year[1]) %>% 
      group_by(country)%>%
      summarise(suicides.100k.pop=round(mean(suicides.100k.pop),2))%>%
      arrange(country)
    })

  reactive_polygons = reactive({
   worldcountries[worldcountries$ADMIN %in% reactive_map_data()$country,]
    })
  
  selected_data=reactive({
    req(input$gender)
    req(input$plot_year)
    req(input$plot_age)
    req(input$map_shape_click)
    df%>%
      filter(country==input$map_shape_click$id & sex %in% input$gender &
                               age %in% input$plot_age)%>%
      group_by(year)%>%
      summarise(suicides.100k.pop=mean(suicides.100k.pop))
    })
  
  reactive_curve_plot=reactive({
    
    p=ggplot(selected_data(), aes(x=year, y=suicides.100k.pop)) +
      geom_line( color=cv_pal(mean(selected_data()$suicides.100k.pop)), size=1, alpha=0.9)+ 
      theme_classic()+
      theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.ticks.y=element_blank())
    ggplotly(p)%>% add_trace(type = 'scatter',
                             mode = 'lines',
                             text=input$map_shape_click$id,
                             x=~year,
                             y=~suicides.100k.pop,
                             hovertemplate = paste(
                               "<b>%{text}</b><br>",
                               "Year: %{x}<br>",
                               "Suicides rate (per 100,000): %{y}",
                               "<extra></extra>"))
    
    })
  
  reactive_data_gender_plot=reactive({
    req(input$plot_year)
    req(input$plot_age)
    df %>% 
      filter(   age %in% input$plot_age &
                year <= input$plot_year[2] &
                year >= input$plot_year[1])
    })
  
  reactive_gender_plot=reactive({
    df1 <- reactive_data_gender_plot() %>% 
      group_by(country) %>% 
      summarise(SUM = sum(suicides.100k.pop)) %>%
      full_join(reactive_data_gender_plot()) %>% 
      group_by(country,sex)%>%
      mutate(prop = suicides.100k.pop/SUM)%>%
      drop_na(prop)
    
    
    ggplot() + geom_bar(aes(y = prop, x = country, fill = sex), data = df1,
                        stat="identity")+theme(legend.position = "top")+coord_flip()+scale_fill_manual(values=c("#B1A3F6","#99D6B9"))+theme(axis.title.y=element_blank(), axis.ticks.y=element_blank())
    
    
    
  
  })
  
  reactive_data_age_plot=reactive({
    req(input$plot_year)
    req(input$plot_age)
    df %>% 
      filter(   sex %in% input$gender &
                  year <= input$plot_year[2] &
                  year >= input$plot_year[1])
  })
  
  reactive_age_plot=reactive({
    df2 <- reactive_data_age_plot() %>% 
      group_by(country) %>% 
      summarise(SUM = sum(suicides.100k.pop)) %>%
      full_join(reactive_data_age_plot()) %>% 
      group_by(country,age)%>%
      mutate(prop = suicides.100k.pop/SUM)%>%
      drop_na(prop)
    
    
    ggplot() + geom_bar(aes(y = prop, x = country, fill = age), data = df2,
                        stat="identity")+coord_flip()+scale_color_brewer(palette="GnBu")+theme(legend.position = "top")+theme(axis.title.y=element_blank(), axis.ticks.y=element_blank())
    
    
  })
  
  reactive_world_rank_data=reactive({
    req(input$plot_year[1])
    req(input$plot_year[2])
    df%>%filter(year>=input$plot_year[1]&year<=input$plot_year[2])%>%group_by(country)%>%summarise(suicides.100k.pop=mean(suicides.100k.pop))%>%mutate(country = fct_reorder(country, suicides.100k.pop))
  })

  reactive_world_rank_plot=reactive({
    rank=reactive_world_rank_data()%>%ggplot( aes(x=country, y=suicides.100k.pop)) + 
      geom_bar(stat="identity", alpha=.6, width=.4) +coord_flip()+
      theme(text = element_text(size=9),axis.title.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.ticks.y=element_blank())+
      scale_x_discrete(position='top')
    ggplotly(rank)%>% layout(hoverlabel=list(bgcolor="white"))
  })
  
  dataworld=df%>%group_by(year)%>%summarise(suicides.100k.pop=mean(suicides.100k.pop))
  
   reactive_worldplot=reactive({
    curve=ggplot(dataworld, aes(x=year, y=suicides.100k.pop)) +
    geom_line(colour="#2455A4",size=1, alpha=0.9)+
    scale_x_continuous(breaks = seq(from = 1985, to = 2016, by = 5))+
    theme_classic()+
    theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.ticks.y=element_blank())
    ggplotly(curve)%>%
      add_trace(type = 'scatter',
                mode = 'lines',
                x=~year,
                y=~suicides.100k.pop,
                hovertemplate = paste(
                  "Year: %{x}<br>",
                  "Suicides rate (per 100,000): %{y}",
                  "<extra></extra>"))
     })
  
##-------------------------RENDER MAP TAB-----------------------
###-----MAP----  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE),data=reactive_polygons()) %>% 
      addTiles() %>%
      addProviderTiles('CartoDB.Positron') %>%
      addPolygons(layerId = reactive_polygons()$ADMIN,weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "", fillOpacity = 0.8,fillColor=cv_pal(reactive_map_data()$suicides.100k.pop),
                  highlight = highlightOptions(weight = 2,
                                               color = "white",
                                               dashArray = "",
                                               fillOpacity = 1, bringToFront = TRUE),
                  label = sprintf("<strong>%s</strong><br/>Average suicide rate (per 100,000): %g",
                                  reactive_map_data()$country,reactive_map_data()$suicides.100k.pop) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",padding = "3px 8px"),textsize = "15px",direction = "auto"))%>%
      addLegend("bottomleft", pal = cv_pal, values = 0:200)
    })
  
###-----WORLD TAB----  
 

  
  output$world_trend_title<-renderText({print("Worldwide suicide rate (per 100,000)")})
  output$world_trend_subtitle<-renderText({print("Between years 1985 and 2016")})
  output$world_trend<-renderPlotly({ reactive_worldplot()})
  
  
  output$world_rank_title<-renderText({print("Countries ranked by suicide rate (per 100,000)")})
  output$world_rank_subtitle<-renderText({paste("Average rate between ",input$plot_year[1]," and ",input$plot_year[2])})
  output$world_rank<-renderPlotly({reactive_world_rank_plot()})
  

  
###-----SELECTED COUNTRY TAB----
  output$curve_plot <- renderPlotly({
  reactive_curve_plot() })

###-----GENDER TAB----  
  output$gender_prop<-renderPlot({ reactive_gender_plot() })

###-----AGE TAB----
  output$age_prop<-renderPlot({ reactive_age_plot() })



  
  
  
  
##-------------------------Reactive Functions MODEL-------------------------  
  reactive_model_data=reactive({ 
    req(input$model_countries)
    df%>%filter(country==input$model_countries,age=='15-24 years',sex=='male')})
  
  reactive_x=reactive({reactive_model_data%>%select(input$variable1)})
   
  reactive_model=reactive({
    req(input$variable1)
    req(input$k1)

    #gam(as.formula(paste("suicides.100k.pop~s(",input$variable1,",k=",input$k1,")")),data=reactive_model_data())
    gam(suicides.100k.pop~s(input$variable1-lag(input$variable1),k=input$k1),data=reactive_model_data())
  })
  
  reactive_plot=reactive({
    plot(reactive_model(),pages=1,all.terms=TRUE,shade=TRUE)
  })
  
  reactive_output=reactive({
    out=capture.output(gam.check(reactive_model()))
    out_table=out[12:length(out)]
    p_value <- strsplit(out_table, " ")[[2]]
    output_p<-as.numeric(p_value[length(p_value)])
    
  })

    
##-----Render MODEL-----
  output$model_plot<-renderPlot({reactive_plot()})
  output$model_output<-renderText({reactive_output()})

#CLUSTER TAB
#------
  bin = c(1,2,3,4)
  pal <- colorFactor(c("#2E86E0","#FFBD20","#009B77","#EF3340"), domain =data$classification , levels = bin)
  
  classmap=worldcountries[worldcountries$ADMIN %in% data$country,]
  datamap=data%>%select(country,classification)%>%distinct()
  
  output$clustermap<-renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE),data=classmap) %>% 
      addTiles() %>%
      addProviderTiles('CartoDB.Positron') %>%
      addPolygons(layerId = classmap$ADMIN,weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "", fillOpacity = 0.8,fillColor=pal(datamap$classification),
                  highlight = highlightOptions(weight = 2,
                                               color = "white",
                                               dashArray = "",
                                               fillOpacity = 1, bringToFront = TRUE))%>%
      addLegend("topright", pal = pal, values = c(1,2,3,4))
  })
  
  output$BIC<-renderPlot({plot(BIC)})
  output$gam1<-renderPlot({plot(g1,pages=1,shade=TRUE)})
  output$gam2<-renderPlot({plot(g2,pages=1,shade=TRUE)})
  output$gam3<-renderPlot({plot(g3,pages=1,shade=TRUE)})
  output$gam4<-renderPlot({plot(g4,pages=1,shade=TRUE)})
#------
}
shinyApp(ui, server)

