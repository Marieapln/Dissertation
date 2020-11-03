#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
library(shiny)
library(DT)
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
library(plotly)
library(gganimate)
library(data.table)
library(mgcv)
library(zoo)
library(cowplot)
#SUICIDE DATASET AND MAP SETTING
#-------------------------------
df<-read.csv("Suicide.csv")

#IMPORT WORLD MAP
worldcountries = geojson_read("50m.geojson", what = "sp")

#reorder age groups in chronological order
df$age <- factor(df$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))

#Print mismatches between countries in the geojson file and those in the dataframe
df$country <- as.character(df$country)
df$country[df$country== "Republic of Korea"] <- "South Korea" 
df$country[df$country== "Russian Federation"] <- "Russia" 
df$country[df$country== "Czech Republic"] <- "Czechia" 
df$country[df$country== "Saint Vincent and Grenadines"] <- "Saint Vincent and the Grenadines" 
df$country[df$country== "United States"] <- "United States of America" 
df$country[df$country== "Serbia"] <- "Republic of Serbia"
df$country[df$country== "Bahamas"] <- "The Bahamas"
df$country[df$country== "Macau"]<-"Macao S.A.R" 
df$country<- as.factor(df$country)

#Create the map
df<-df[order(df$country),]
worldcountries<-worldcountries[order(worldcountries$ADMIN),]
plot_map <- worldcountries[worldcountries$ADMIN  %in% df$country, ] 

#Create relevant bins for the incidence/100K data
summary(df$suicides.100k.pop) 
bins = c(0,10,20,30,40,50,Inf)
cv_pal <- colorBin(c("#F9F871","#FFC257","#FF8C60","#F65C78","#C44091","#773C9E"), domain =df$suicides.100k.pop , bins = bins)
#-------------------------------

summary(df)

data=df%>%filter(year>=1991)
#UNEMPLOYMENT DATA
#------------------
#importing unemployment rates from the worldbank database
unemployment<-read.csv('unemployment-worldbank.csv')
unemployment=rename(unemployment, c('country'=Country.Name,'year'=Time,'unemployment'=Unemployment..total....of.total.labor.force...modeled.ILO.estimate...SL.UEM.TOTL.ZS.))

unemployment=unemployment%>%select(country,year,unemployment)
#Correct the country name inconsistencies between the suicide database and the worldbank database
unemployment$country<- as.character(unemployment$country)
unemployment$country[unemployment$country== "Bahamas, The"] <- "The Bahamas" 
unemployment$country[unemployment$country== "Czech Republic"] <- "Czechia" 
unemployment$country[unemployment$country== "Kyrgyz Republic"] <- "Kyrgyzstan" 
unemployment$country[unemployment$country== "Macao SAR, China"] <- "Macao S.A.R"
unemployment$country[unemployment$country== "Korea, Rep."] <- "South Korea"
unemployment$country[unemployment$country== "Russian Federation"] <- "Russia"
unemployment$country[unemployment$country== "St. Vincent and the Grenadines"] <- "Saint Vincent and the Grenadines"
unemployment$country[unemployment$country== "Serbia"] <- "Republic of Serbia"
unemployment$country[unemployment$country== "Slovak Republic"] <- "Slovakia"
unemployment$country[unemployment$country== "United States"] <- "United States of America"
unemployment$country<- as.factor(unemployment$country)
unemployment%>%filter(country=='Albania')
data=left_join(data, unemployment, by = c("country" = "country", "year" = "year"))
data=data%>%filter(unemployment!='..')
#------------------

#FEMALE LABOUR PARTICIPATION DATA
#--------------------------------
flpr<-read.csv('FLPR.csv')
flpr$country<- as.character(flpr$country)
flpr$country[flpr$country== "Bahamas, The"] <- "The Bahamas" 
flpr$country[flpr$country== "Czech Republic"] <- "Czechia" 
flpr$country[flpr$country== "Kyrgyz Republic"] <- "Kyrgyzstan" 
flpr$country[flpr$country== "Macao SAR, China"] <- "Macao S.A.R"
flpr$country[flpr$country== "Korea, Rep."] <- "South Korea"
flpr$country[flpr$country== "Russian Federation"] <- "Russia"
flpr$country[flpr$country== "St. Vincent and the Grenadines"] <- "Saint Vincent and the Grenadines"
flpr$country[flpr$country== "Serbia"] <- "Republic of Serbia"
flpr$country[flpr$country== "Slovak Republic"] <- "Slovakia"
flpr$country[flpr$country== "United States"] <- "United States of America"
data=left_join(data, flpr, by = c("country" = "country", "year" = "year"))
#--------------------------------


#FERTILITY DATA
#--------------
fertility<-read.csv('fertility.csv')
fertility$country<- as.character(fertility$country)
fertility$country[fertility$country== "Bahamas, The"] <- "The Bahamas" 
fertility$country[fertility$country== "Czech Republic"] <- "Czechia" 
fertility$country[fertility$country== "Kyrgyz Republic"] <- "Kyrgyzstan" 
fertility$country[fertility$country== "Macao SAR, China"] <- "Macao S.A.R"
fertility$country[fertility$country== "Korea, Rep."] <- "South Korea"
fertility$country[fertility$country== "Russian Federation"] <- "Russia"
fertility$country[fertility$country== "St. Vincent and the Grenadines"] <- "Saint Vincent and the Grenadines"
fertility$country[fertility$country== "Serbia"] <- "Republic of Serbia"
fertility$country[fertility$country== "Slovak Republic"] <- "Slovakia"
fertility$country[fertility$country== "United States"] <- "United States of America"
data=left_join(data, fertility, by = c("country" = "country", "year" = "year"))
data=data%>%filter(fertility!='..')
#-----------

#ADD UN SUBREGIONS
#-----------------

unsd_regions=read.csv('UNSD — Methodology.csv') #import UN subregions
unsd_regions=unsd_regions%>%select(Sub.region.Name,Country.or.Area) #select only relevant data: country+subregion classification
unsd_regions=rename(unsd_regions, c('subregion'=Sub.region.Name,'country'=Country.or.Area))
unsd_regions$country<- as.character(unsd_regions$country)
unsd_regions$country[unsd_regions$country== "Bahamas"] <- "The Bahamas" #replace
unsd_regions$country[unsd_regions$country== "Russian Federation"] <- "Russia" 
unsd_regions$country[unsd_regions$country== "Republic of Korea"] <- "South Korea" 
unsd_regions$country[unsd_regions$country== "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom" 
unsd_regions$country[unsd_regions$country== "Serbia"] <- "Republic of Serbia" 
data=left_join(data, unsd_regions, by = c("country" = "country"))

#-----------------

#type transformation: from integer to numeric
data$unemployment<-as.numeric(as.character(data$unemployment))
data$flpr<-as.numeric(as.character(data$flpr))
data$fertility<-as.numeric(as.character(data$fertility))

#standardise the values in the numeric columns
data_nscale=data
data=data %>% mutate_at(c("fertility", "flpr","gdp_per_capita...."), ~(scale(.) %>% as.vector))
#CLUSTERING
#----------
#Create a dataset with averages for numeric columns to be introduced in the model
d=data%>%
    filter(!is.na(subregion))%>%
    group_by(subregion,country)%>%
    summarise(gdp_per_capita....=mean(gdp_per_capita....),
              unemployment=mean(unemployment),
              fertility=mean(fertility),
              flpr=mean(flpr))%>%
    select(subregion,country,gdp_per_capita....,unemployment,fertility,flpr)
#Transform types into numerical values
d$gdp_per_capita....<-as.numeric(as.character(d$gdp_per_capita....))
d$unemployment<-as.numeric(as.character(d$unemployment))
d$fertility<-as.numeric(as.character(d$fertility))
d$flpr<-as.numeric(as.character(d$flpr))

#Subset data into two parts: Values (X) and subregions (class)
X=d[,3:6]
class=d$subregion

#Compute, plot BIC clustering criterion then print the top models according to the BIC criterion 
BIC<-mclustBIC(X)

#Fit a clustering model using the previous computation
model1<-Mclust(X, x=BIC)


#Add cluster number to the larger dataset
##add a column with the classification to the 'means' dataset used for clustering
d['classification']<-model1$classification
##Join with the larger dataset
dmeans=d%>%ungroup()%>%select(country,classification)

data=left_join(data, dmeans, by = "country")
data_nscale=left_join(data_nscale, dmeans, by = "country")

#----------
#MODELING
#--------------------
#Change 0s into very small values to allow for log transformation
data$suicides.100k.pop[data$suicides.100k.pop==0] <- .00001
data1=data%>%filter(classification==1)
data2=data%>%filter(classification==2)
data3=data%>%filter(classification==3)
data4=data%>%filter(classification==4)


datashow=data%>%select(country,year,sex,age,suicides_no,population,suicides.100k.pop,HDI.for.year,gdp_for_year....,gdp_per_capita....,generation,unemployment,flpr,fertility)
    
datashow=rename(datashow,c('nb of Suicides'=suicides_no,'suicide rate (per 100k)'=suicides.100k.pop,
                           'HDI'=HDI.for.year,'GDP per capita'=gdp_per_capita....,
                           'GDP'=gdp_for_year....))




ui <- navbarPage(title='', collapsible=TRUE,theme=shinytheme('paper'),
                 
                 
                 #MAP TAB
                 #-----
                 tabPanel('Map',
                          div(class="outer",
                              tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0, }"),
                              leafletOutput("map", width = "100%", height = "100%"),
                              
                              #CONTROL PANEL
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
                              
                              
                              #DRAGABLE/GRAPH PANEL
                              absolutePanel(      id = "graphs", top = 65, right = 40, width = 300, fixed=TRUE,
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

#world_trend_title,#world_rank_title{color:#9251BD;
                                 font-size: 16px;
                                 font-weight: bold;
}
#world_trend_subtitle,#world_rank_subtitle{font-style:italic}
.leaflet-container { background:#D4DADC ;}
.btn-custom {background-color: #9251BD; color: #FFF;}
.btn:hover { background-color: #FFF; color: #9251BD;}
.btn:focus { background-color: #FFF; color: #9251BD;}

.navbar-default {background-color:#D4DADC; border-color:#D4DADC;box-shadow: none!important;}
   
"

                              ))),
                 #MODEL TAB
                 #-----
                 tabPanel('Clustering and fitting a GAM',
                          
                          
                          absolutePanel(id='controls_model',
                                        style='padding:5px', fixed = TRUE,
                                        draggable = TRUE, top = 80, left = 20,
                                        right = "auto", bottom = "auto",
                                        width = "30%",
                                        div(
                                        HTML('<h6><b>Select a cluster on which to fit the GAM</b></h6>'),
                                        radioGroupButtons(
                                            inputId = "clustermod",
                                            label=NULL,
                                            choices = c("1","2","3","4","All","Manual selection"),
                                           #status = "danger"
                                        ),
                                        tags$script("$(\"input:radio[name='clustermod'][value='1']\").parent().css('background-color', '#2E86E0');"),
                                        tags$script("$(\"input:radio[name='clustermod'][value='2']\").parent().css('background-color', '#FFBD20');"),
                                        tags$script("$(\"input:radio[name='clustermod'][value='3']\").parent().css('background-color', '#009B77');"),
                                        tags$script("$(\"input:radio[name='clustermod'][value='4']\").parent().css('background-color', '#EF3340');"),
                                        pickerInput(inputId="selected_countries",label=NULL,multiple=TRUE,choices=unique(data$country)),
                                        HTML('<h6><b>"Select a transformation for the dependent variable (optional)"</b></h6>'),
                                        radioGroupButtons(
                                            inputId = "transformation",
                                            label = NULL,
                                            choices = c("log","X2","X3","none")
                                        ),
                                        splitLayout(
                                        
                                        #GDP
                                        prettySwitch(
                                            inputId = "gdpswitch",
                                            label = "GDP per capita",value=TRUE),
                                        
                                        #Unemployment
                                        prettySwitch(
                                          inputId = "uswitch",
                                          label = "Unemployment",value=FALSE)
                                        
                                        ),
                                        splitLayout(
                                          box(
                                            splitLayout(cellWidths ='50%',
                                            HTML('<h6>B</h6>'),
                                            numericInput(inputId='gdp_b',label=NULL,value=1, min = 1, max =20),
                                            
                                            HTML('<h6>SP</h6>'),
                                            numericInput(inputId='gdp_sp',label=NULL,value=1, min = 1, max =20))),
                                      
                                          box(
                                            splitLayout(cellWidths ='50%',
                                            HTML('<h6>B</h6>'),
                                            numericInput(inputId='u_b',label=NULL,value=1, min = 1, max =20),
                                           
                                            HTML('<h6>SP</h6>'),
                                            numericInput(inputId='u_sp',label=NULL,value=1, min = 1, max =20)))
                                            
                                        ),
                                        
                                        splitLayout(
                                        #Fertility
                                        prettySwitch(
                                            inputId = "fswitch",
                                            label = "Fertility",value=FALSE),
                                        #FLPR
                                        prettySwitch(
                                          inputId = "flprswitch",
                                          label = "FLPR",value=FALSE)),
                                        
                                        splitLayout(
                                          
                                          box(
                                            splitLayout(cellWidths ='50%',
                                            HTML('<h6>B</h6>'),
                                            numericInput(inputId='f_b',label=NULL,value=1, min = 1, max =20),
                                            HTML('<h6>SP</h6>'),
                                            numericInput(inputId='f_sp',label=NULL,value=1, min = 1, max =20)
                                        )),
                                        
                                        box(
                                        splitLayout(cellWidths ='50%',
                                            HTML('<h6>B</h6>'),
                                            numericInput(inputId='flpr_b',label=NULL,value=1, min = 1, max =20),
                                            HTML('<h6>SP</h6>'),
                                            numericInput(inputId='flpr_sp',label=NULL,value=1, min = 1, max =20)
                                        ))
                                        ),
                                        
                                        splitLayout(
                                            prettySwitch(
                                                inputId = "cswitch",
                                                label = "Country"),
                                            prettySwitch(
                                                inputId = "gswitch",
                                                label = "Gender"),
                                            prettySwitch(
                                                inputId = "aswitch",
                                                label = "Age")
                                        ),
                                        actionButton("fitbutton", "Fit a GAM"))
                                        
                                        
                          ),
                          
                          absolutePanel(
                              id = "model_panel",  fixed = TRUE,
                              draggable = TRUE, top = 80, left='35%', right="auto", bottom = "auto",
                              width = "30%",
                              div(style='height:630px',
                              leafletOutput("clustermap",width='100%',height='200px'),
                              plotOutput('gamsplot')),
                              
                          ),
                          absolutePanel(
                              id = "model_panel",  fixed = TRUE,
                              draggable = TRUE, top = 80, right=20, left="auto", bottom = 20,
                              width = "30%",
                              div(style='height:630px;overflow-y: scroll;overflow-x: scroll',  
                                  HTML('<h6>AIC</h6>'),
                                  textOutput('AIC'),
                                  verbatimTextOutput('gamprint')
                          )
                          ),
                          
                          tags$head(tags$style(
                              'HTML','
#controls_model {background-color: white}
input[type=\"number\"] {
    width: 40px;height:30px;
  }
'))
                          
                 ),
                 
                 
                 #-----
                 tabPanel('View Data Table', 
                          fluidRow( column(12,
                              DT::dataTableOutput("mytable")))
                 )
                 
                 
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
            geom_line(colour="#9251BD",size=1, alpha=0.9)+
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
    
    select_countries_data=reactive({
      data%>%filter(country%in%input$selected_countries)})
    
    
    dcluster=reactive({
        req(input$clustermod)
        if (input$clustermod=="All") {paste("data")} 
        else {
        if (input$clustermod=="Manual selection") {
          paste("D")
        } 
        else
        paste("data",input$clustermod,sep="")}})
    
    Y=reactive({
        if (input$transformation=="log") {
            paste(sep="","log(",dcluster(),"$suicides.100k.pop)~",dcluster(),"$year")
        } else {
            if (input$transformation=="X2") {
                paste(sep="",dcluster(),"$suicides.100k.pop^2~",dcluster(),"$year")
            } else { 
                if (input$transformation=="X3") {
                    paste(sep="",dcluster(),"$suicides.100k.pop^3~",dcluster(),"$year")
                } else { 
                    paste(sep="",dcluster(),"$suicides.100k.pop~",dcluster(),"$year")
                }
            }
        }
        
    })  
    x1=reactive({
        if (input$gdpswitch==FALSE) {
        } else {
            paste("+s(",dcluster(),"$gdp_per_capita....,k=",input$gdp_b,",sp=",input$gdp_sp,")",sep="")
        }
    })
    #      paste("+s(",dcluster(),"$gdp_per_capita....,k=",input$gdp_b,",sp=",input$gdp_sp,")",sep="")
    
    x2=reactive({
        if (input$uswitch==FALSE) {
            
        } else {
            paste("+s(",dcluster(),"$unemployment,k=",input$u_b,",sp=",input$u_sp,")",sep="")
        }
    })
    x3=reactive({
        if (input$fswitch==FALSE) {
            
        } else {
            paste("+s(",dcluster(),"$fertility,k=",input$f_b,",sp=",input$f_sp,")",sep="")
        }
    })
    x4=reactive({
        if (input$flprswitch==FALSE) {
            
        } else {
            paste("+s(",dcluster(),"$flpr,k=",input$flpr_b,",sp=",input$flpr_sp,")",sep="")
        }
    })
    x5=reactive({
        if (input$cswitch==FALSE) {
            
        } else {
            paste("+",dcluster(),"$country")
        }
    })
    x6=reactive({
        if (input$gswitch==FALSE) {
            
        } else {
            paste("+",dcluster(),"$sex")
        }
    })
    x7=reactive({
        if (input$aswitch==FALSE) {
            
        } else {
            paste("+",dcluster(),"$age")
        }
    })
    
    #formula_gam=reactive({paste(Y(),x1(),x2(),x3(),x4(),x5(),x6(),x7())})
    #reactive_gam=reactive({gam(formula=as.formula(formula_gam()))})
    GAM=reactive({
        D=select_countries_data()
        gam_formula=paste(Y(),x1(),x2(),x3(),x4(),x5(),x6(),x7())
        gam(formula=as.formula(gam_formula))
    })
    
    #paste(Y(),x1(),x2(),x3(),x4(),x5(),x6(),x7())
    reactive_plot=reactive({
        plot(reactive_gam(),pages=1,shade=TRUE)
    })
    
    #output$gamsplot<-eventReactive(input$fitbutton, reactive_plot())
    #    output$try<- renderText({paste(formula.gam(g4))})
    
    observeEvent(input$fitbutton, {
        output$gamsplot <- renderPlot({
            gamplot=plot(GAM(),pages=1,shade=TRUE)
            gamplot
            })
        output$gamprint<-renderPrint({summary(GAM())})
        output$AIC<-renderText({print(AIC(GAM()))})
        
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
    
    #------
    output$mytable = renderDT(datashow, filter = "top")
}
# Run the application 
shinyApp(ui = ui, server = server)
