library("shiny")
library("shinydashboard")
library("dashboardthemes")
library("move")
library("ggplot2")
library("shinyBS") ## to display message when hovering over input element in UI
library("lubridate")
library("geosphere")
library("dismo")
library("rgeos")
library("stringr")
library("shinyWidgets")
library("ggsn")
library("shinycssloaders")
library("maptools")
library("circular")

## TODO
# - scale bar that is also zoomable
# - make un-/selection of corridors possible, or al least by number of segments
# - button to see only corridor segments with and without rest of track
# - check why all individuals get zoomed in when one is zooomed in
# - give corridor segments different color

## https://www.youtube.com/watch?v=gGEY82qA3BI  ##  modules
## module shinydashboard site:stackoverflow.com


shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    dashboardPage(
      dashboardHeader(title = "Corridors per individual",titleWidth=300),
      dashboardSidebar(uiOutput(ns("SidebarUI")),
                       tags$style( ## to make a vertical scroll bar on the sidebar so all tabs can be accessed while seeing the main panel
                         "#sidebarItemExpanded {
                      overflow: auto;
                      height: calc(100vh - 50px) !important;
                     }")),
      dashboardBody(uiOutput(ns("TabUI")),
                    shinyDashboardThemes( #https://github.com/nik01010/dashboardthemes
                      theme = "grey_dark"
                    ))
      # ,skin = 'green'
    )
  )
}

shinyModuleConfiguration <- function(id, input) {
  ns <- NS(id)
  
  configuration <- list()
  
  configuration
}

shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  current <- reactiveVal(data)
  
  namesCorresp <- data.frame(nameInd=namesIndiv(data) , tabIndv=str_replace_all(namesIndiv(data), "[^[:alnum:]]", ""))
  ntabs <- length(namesIndiv(data))
  tabnames <- str_replace_all(namesIndiv(data), "[^[:alnum:]]", "") #it does not allow punctuation or spaces
  speedPropnames <- paste0(tabnames, '_speedProp') 
  circPropnames <- paste0(tabnames, '_circProp') 
  timeThinnames <- paste0(tabnames, '_timeThin')
  clustDistnames <- paste0(tabnames, '_clustDist')
  updateButton <- paste0(tabnames,'_updateButton')
  plotnames <- paste0("plot_",tabnames) 
  
  
  output$SidebarUI <- renderUI({
    Menus <- vector("list", ntabs)
    for(i in 1:ntabs){
      Menus[[i]] <-   menuItem(tabnames[i], icon=icon("paw"), tabName = tabnames[i], selected = i==1) }
    do.call(function(...) sidebarMenu(id = ns('sidebarMenuUI'),...), Menus)
  })
  
  output$TabUI <- renderUI({
    Tabs <- vector("list", ntabs)
    for(i in 1:ntabs){
      Tabs[[i]] <- tabItem(tabName = tabnames[i],
                           fluidRow( ## make the same as in "population corridors"
                             column(3,sliderInput(inputId=ns(speedPropnames[i]),label="Speed", min=0,max=1, value=0.75, step=0.01),
                                    bsTooltip(id=ns(speedPropnames[i]), title="Proportion of speeds which are high enough to be a valid corridor point (default: speeds that are greater than 75 % of all speeds)", placement = "bottom", trigger = "hover", options = list(container = "body"))),
                             column(3,sliderInput(inputId=ns(circPropnames[i]),label="Parallelism", min=0,max=1, value=0.25, step=0.01),
                                    bsTooltip(id=ns(circPropnames[i]), title="Proportion of the circular variances that is low enough to be a valid corridor point. Low values indicate that the segments are (near) parallel (default: variances that are lower than 25 % of all variances)", placement = "bottom", trigger = "hover", options = list(container = "body"))
                                    ), ## maybe change wording to make it simpler: the lower the value, the more parallel are the segments
                             column(2, numericInput(ns(timeThinnames[i]),"Thin track to X mins",min=0,max=60, value=0, step=1),
                                    bsTooltip(id=ns(timeThinnames[i]), title="This is specially recommended for high resolution tracks to ease finding regions with parallel segments. Default (=0) no thinning", placement = "bottom", trigger = "hover", options = list(container = "body"))
                                    ),
                             column(3, numericInput(ns(clustDistnames[i]),"Radius of corridor cluster circles (mts)",value=300),
                                    bsTooltip(id=ns(clustDistnames[i]), title="All identified corridor segments that fall within a circle will be grouped as a corridor cluster", placement = "bottom", trigger = "hover", options = list(container = "body"))
                                    ),
                             column(1,actionBttn(ns(updateButton[i]), label="Update!", style="fill", color="success",icon=icon("redo"),size="md"))
                           ),
                           strong("BUG in APP: if you do not see the track, or only see it partially, double click in the plot area", style="color:orange"),## warning text as long as zoom is not working
                           withSpinner(plotOutput(ns(plotnames[i]),dblclick = ns(paste0(plotnames[i],"_dblclick")), brush = brushOpts(id = ns(paste0(plotnames[i],"_brush")),delayType="debounce",direction="xy",resetOnNew = TRUE), height = "80vh"), type=5, size=1.5,color= "#28b78d") ## color the same as update button
      )
    }
    do.call(tabItems, Tabs)
  })
  
  ## zoom into each plot with double click, back to full with double click                        
  ranges <- reactiveValues(x_range = NULL, y_range = NULL) ## for zoom of plot
  observeEvent({input$sidebarMenuUI; input[[paste0("plot_",input$sidebarMenuUI,"_dblclick")]]}, {
    brush <- input[[paste0("plot_",input$sidebarMenuUI, "_brush")]]
    if (!is.null(brush)) {
      ranges$x_range <- c(brush$xmin, brush$xmax)
      ranges$y_range <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x_range <- NULL
      ranges$y_range <- NULL
    }
  })
  
  
  ## get input values for each tab
  RVtab <- reactiveValues()
  
  observe({
    RVtab$indv <- namesCorresp$nameInd[namesCorresp$tabIndv==input$sidebarMenuUI]
  })
  ## get input values for each tab after hitting update button
  RVupdate <- reactiveValues()
  observeEvent({input[[paste0(input$sidebarMenuUI, '_updateButton')]]},{
    RVupdate$thintime <- input[[paste0(input$sidebarMenuUI, '_timeThin')]]
    RVupdate$speedProp <- input[[paste0(input$sidebarMenuUI, '_speedProp')]]
    RVupdate$circProp <- input[[paste0(input$sidebarMenuUI, '_circProp')]]
    RVupdate$clustDist <- input[[paste0(input$sidebarMenuUI, '_clustDist')]]
  })
  
  for(i in 1:ntabs){
    output[[plotnames[i]]] <- renderPlot({
      dataSubInd <-  data[[RVtab$indv]]
      if(!input[[paste0(input$sidebarMenuUI, '_updateButton')]]){ ## default plot
        
        dataSubIndTime <- dataSubInd ## no thining by time
        
        ## extracting some data for plots
        indDF <- data.frame(long=coordinates(dataSubIndTime)[,1],lat=coordinates(dataSubIndTime)[,2])
        indivName <- namesIndiv(dataSubIndTime)
        
        corridorCalc <- corridor(x=dataSubIndTime, speedProp=.75, circProp=.25, plot=FALSE)
        # corrDF <- data.frame(long=coordinates(corridorCalc)[,1],lat=coordinates(corridorCalc)[,2], corrid=c(as.character(corridorCalc@burstId),"no.corridor"))
        
        if(!any(burstId(corridorCalc)=="corridor") || length(burstId(corridorCalc)[burstId(corridorCalc)=="corridor"])==1){  ## if levels do not contain "corridor" OR if there is only 1 corridor point identified it cannot be clustered and gives an error
          cntr <- gCentroid(move2ade(dataSubIndTime))
          
          ggplot()+geom_path(data=indDF,aes(long,lat))+
            labs(title=indivName, x ="Longitude", y = "Latitude")+ 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))+
            geom_label(aes(x=cntr@coords[1],y=cntr@coords[2]), label="No corridors found", color="red", size=8)+
            coord_fixed(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)+
            # annotation_scale(plot_unit="m")+
            ggsn::scalebar(indDF,location="bottomleft", dist = 1,dist_unit="km", transform=T,st.size=3, height=0.01, model = 'WGS84')
          
        } else { ## there are corridors found
          # ## to give each end pt of corridor segment also corridor id
          # crptsPosition <- which(burstId(corridorCalc)=="corridor")
          # dataSubIndTime$corridor <- "no.corridor" 
          # dataSubIndTime$corridor[crptsPosition] <- "corridor"
          # dataSubIndTime$corridor[crptsPosition+1] <- "corridor"
          
          ## corridor segment midpoints
          midCrPts <- corridorCalc@data[burstId(corridorCalc)=="corridor",c("segMid_x","segMid_y")] 
          coordinates(midCrPts) <- ~segMid_x+segMid_y
          projection(midCrPts) <- projection(dataSubIndTime)
          ## distance matrix
          dist <- distm(midCrPts, fun=distGeo)
          ## clustering
          hc <- hclust(as.dist(dist), method="single") # complete single
          # define clusters based on a tree "height" cutoff distance (distance between corridor clusters) and add them to the SpDataFrame
          midCrPts$clust <- cutree(hc, h=300)
          # add nb of corridor segments per cluster to SpDataFrame
          tbCount <- data.frame(table(midCrPts$clust))
          midCrPts$nbCorrInClust <- NA
          for(j in tbCount$Var1){
            midCrPts$nbCorrInClust[midCrPts$clust==j] <- tbCount$Freq[tbCount$Var1==j]
          }
          #HERE##### make here clusters selecable, unselectable, remove those with lesss than X points, etc
          midCrPts <- midCrPts[midCrPts$nbCorrInClust>1,]
          
          # get the centroid coords for each cluster
          centClust <- matrix(ncol=2, nrow=max(midCrPts$clust))
          centClust <- data.frame(x=NA,y=NA,clusterID=unique(midCrPts$clust))
          for(i in unique(midCrPts$clust)){
            centClust[centClust$clusterID==i,c("x","y")] <- gCentroid(subset(midCrPts, clust == i))@coords
          }
          # compute circles around the cluster centroids with "clustDist" radius
          clusterCircles <- circles(centClust[,c("x","y")], d=300, lonlat=T,dissolve=F)
          
          
          cifort <- fortify(clusterCircles@polygons, region="ID")
          
          ggplot()+geom_path(data=indDF,aes(long,lat))+
            # geom_segment(data=corrDF,aes(long,lat,color=corrid))
            geom_point(data=as.data.frame(midCrPts), aes(segMid_x, segMid_y, color=as.factor(clust)), size=3)+
            geom_polygon(data=cifort, aes(long, lat, group=id,colour=id), fill=NA)+
            labs(title=indivName, x ="Longitude", y = "Latitude", color="corridor cluster")+ 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))+
            coord_fixed(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)+
            ggsn::scalebar(indDF,location="bottomleft", dist = 1,dist_unit="km", transform=T,st.size=3, height=0.01, model = 'WGS84')
        }
        
      } else { ## updated plot
        
        
        if(RVupdate$thintime==0){dataSubIndTime <- dataSubInd ## no thining by time
        } else {
          dataSubIndTime <- dataSubInd[!duplicated(round_date(timestamps(dataSubInd), paste0(RVupdate$thintime," mins"))),]
        }
        ## extracting some data for plots
        indDF <- data.frame(long=coordinates(dataSubIndTime)[,1],lat=coordinates(dataSubIndTime)[,2])
        indivName <- namesIndiv(dataSubIndTime)
        
        corridorCalc <- corridor(x=dataSubIndTime, speedProp=RVupdate$speedProp, circProp=RVupdate$circProp, plot=FALSE)
        
        if(!any(burstId(corridorCalc)=="corridor") || length(burstId(corridorCalc)[burstId(corridorCalc)=="corridor"])==1){  ## if levels do not contain "corridor" OR if there is only 1 corridor point identified it cannot be clustered and gives an error
          cntr <- gCentroid(move2ade(dataSubIndTime))
          
          ggplot()+geom_path(data=indDF,aes(long,lat))+
            labs(title=indivName, x ="Longitude", y = "Latitude")+ 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))+
            geom_label(aes(x=cntr@coords[1],y=cntr@coords[2]), label="No corridors found", color="red", size=8)+
            coord_fixed(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)+
            # annotation_scale(plot_unit="m")+
            ggsn::scalebar(indDF,location="bottomleft", dist = 1,dist_unit="km", transform=T,st.size=3, height=0.01, model = 'WGS84')
          
        } else {
          # ## to give each end pt of corridor segment also corridor id
          # crptsPosition <- which(burstId(corridorCalc)=="corridor")
          # dataSubIndTime$corridor <- "no.corridor" 
          # dataSubIndTime$corridor[crptsPosition] <- "corridor"
          # dataSubIndTime$corridor[crptsPosition+1] <- "corridor"
          
          ## corridor segment midpoints
          midCrPts <- corridorCalc@data[burstId(corridorCalc)=="corridor",c("segMid_x","segMid_y")] 
          coordinates(midCrPts) <- ~segMid_x+segMid_y
          projection(midCrPts) <- projection(dataSubIndTime)
          ## distance matrix
          dist <- distm(midCrPts, fun=distGeo)
          ## clustering
          hc <- hclust(as.dist(dist), method="single") # complete single
          # define clusters based on a tree "height" cutoff distance (distance between corridor clusters) and add them to the SpDataFrame
          midCrPts$clust <- cutree(hc, h=RVupdate$clustDist)
          # add nb of corridor segments per cluster to SpDataFrame
          tbCount <- data.frame(table(midCrPts$clust))
          midCrPts$nbCorrInClust <- NA
          for(j in tbCount$Var1){
            midCrPts$nbCorrInClust[midCrPts$clust==j] <- tbCount$Freq[tbCount$Var1==j]
          }
          #HERE##### make here clusters selecable, unselectable, remove those with lesss than X points, etc
          midCrPts <- midCrPts[midCrPts$nbCorrInClust>1,]
          
          # get the centroid coords for each cluster
          centClust <- matrix(ncol=2, nrow=max(midCrPts$clust))
          centClust <- data.frame(x=NA,y=NA,clusterID=unique(midCrPts$clust))
          for(i in unique(midCrPts$clust)){
            centClust[centClust$clusterID==i,c("x","y")] <- gCentroid(subset(midCrPts, clust == i))@coords
          }
          # compute circles around the cluster centroids with "clustDist" radius
          clusterCircles <- circles(centClust[,c("x","y")], d=RVupdate$clustDist, lonlat=T,dissolve=F)
          
          
          cifort <- fortify(clusterCircles@polygons, region="ID")
          
          ggplot()+geom_path(data=indDF,aes(long,lat))+
            geom_point(data=as.data.frame(midCrPts), aes(segMid_x, segMid_y, color=as.factor(clust)), size=3)+
            geom_polygon(data=cifort, aes(long, lat, group=id,colour=id), fill=NA)+
            labs(title=indivName, x ="Longitude", y = "Latitude", color="corridor cluster")+ 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))+
            coord_fixed(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)+
            ggsn::scalebar(indDF,location="bottomleft", dist = 1,dist_unit="km", transform=T,st.size=3, height=0.01, model = 'WGS84')
        }
        
      }
    })
  }
  
  
  return(reactive({ current() }))
}

