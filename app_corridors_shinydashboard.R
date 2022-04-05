library(shiny)
library(shinydashboard)
# library(shinydashboardPlus)
library("dashboardthemes")
library(move)
library(ggplot2)
library(shinyWidgets)
library(shinyBS) ## to display message when hovering over input element in UI
library(lubridate)
library(geosphere)
library(dismo)
library(rgeos)
library(stringr)
library(sf)

## input data 
# data(fishers)
# dataInp <- fishers

dataInp <- readRDS("/home/anne/MoveAppsGit/allesMoegliche/data/carn.rds")
####


ui <- dashboardPage(
  dashboardHeader(title = "Corridors"),
  dashboardSidebar(uiOutput("Sidebar"),
                   tags$style( ## make a vertical scroll bar on the sidebar so all tabs can be accessed while seeing the main panel
                     "#sidebarItemExpanded {
                      overflow: auto;
                      height: calc(100vh - 50px) !important;
                     }"),
                     tags$style(HTML(".main-sidebar .sidebar .sidebar-menu .treeview-menu li.active a {background-color: orange !important;}"))

                   ),
  dashboardBody(uiOutput("TabUI"),
                shinyDashboardThemes( #https://github.com/nik01010/dashboardthemes
                  theme = "grey_light"
                ))#,
  # skin = 'green'
)


server <- function(input, output) {

  namesCorresp <- data.frame(nameInd=namesIndiv(dataInp) , tabIndv=str_replace_all(namesIndiv(dataInp), "[^[:alnum:]]", ""))
  ntabs <- length(namesIndiv(dataInp))
  tabnames <- str_replace_all(namesIndiv(dataInp), "[^[:alnum:]]", "")
  speedPropnames <- paste0(tabnames, '_speedProp') 
  circPropnames <- paste0(tabnames, '_circProp') 
  timeThinnames <- paste0(tabnames, '_timeThin')
  clustDistnames <- paste0(tabnames, '_clustDist')
  plotnames <- paste0("plot_",tabnames) 
  plot_dblclick <- paste0("plot_dblclick_",tabnames)
  plot_brush <- paste0("plot_brush_", tabnames)
  
  
  output$Sidebar <- renderUI({
    Menus <- vector("list", ntabs)
    for(i in 1:ntabs){
      Menus[[i]] <-   menuItem(tabnames[i], icon=icon("paw"), tabName = tabnames[i], selected = i==1) } #icon(name="scribble",class="fa-thin") <i class="fa-solid fa-code-merge"></i> <i class="fa-solid fa-circle-nodes"></i>
    do.call(function(...) sidebarMenu(id = 'sidebarMenu',...), Menus)
    

  })
  
  output$TabUI <- renderUI({
    Tabs <- vector("list", ntabs)
    for(i in 1:ntabs){
      Tabs[[i]] <- tabItem(tabName = tabnames[i],
                           fluidRow(
                             column(3,sliderInput(inputId=speedPropnames[i],label="Speed", min=0,max=1, value=0.75, step=0.01),
                                    bsTooltip(id=speedPropnames[i], title="Proportion of speeds which are high enough to be a valid corridor point (default: speeds that are greater than 75 % of all speeds)", placement = "bottom", trigger = "hover", options = list(container = "body"))),
                             column(3,sliderInput(inputId=circPropnames[i],label="Paralellnes", min=0,max=1, value=0.25, step=0.01),
                                    bsTooltip(id=circPropnames[i], title="Proportion of the circular variances that is low enough to be a valid corridor point. Low values indicate that the segments are (near) parallel (default: variances that are lower than 25 % of all variances)", placement = "bottom", trigger = "hover", options = list(container = "body"))),
                             column(3, numericInput(timeThinnames[i],"Thin track to X mins",min=0,max=60, value=0, step=1),
                                    bsTooltip(id=timeThinnames[i], title="This is specially recomended for high resolution tracks to ease finding regions wtih paralell segments", placement = "bottom", trigger = "hover", options = list(container = "body"))),
                             column(3, numericInput(clustDistnames[i],"Distance between corridor clusters (mts)",value=300),
                                    bsTooltip(id=clustDistnames[i], title="The radius of the cicles displayed on the map correspond to this value. All identified 'corridor segments' that fall within each cicle will be identified as a corridor", placement = "bottom", trigger = "hover", options = list(container = "body")))
                           ),
                           # plotOutput(plotnames[i],dblclick = "plot_dblclick", brush = brushOpts(id = "plot_brush",resetOnNew = TRUE))
                           plotOutput(plotnames[i],dblclick = plot_dblclick[i], brush = brushOpts(id = plot_brush[i],resetOnNew = TRUE),height = "80vh")
      )
    }
    do.call(tabItems, Tabs)
  })
                           
  ranges <- reactiveValues(x_range = NULL, y_range = NULL) ## for zoom of plot
  observeEvent(input[[paste0("plot_dblclick_",input$sidebarMenu)]], {
    brush <- input[[paste0("plot_brush",input$sidebarMenu)]]
    if (!is.null(brush)) {
      ranges$x_range <- c(brush$xmin, brush$xmax)
      ranges$y_range <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x_range <- NULL
      ranges$y_range <- NULL
    }
  })
  
  
  RV <- reactiveValues()
  observe({
    RV$indv <- namesCorresp$nameInd[namesCorresp$tabIndv==input$sidebarMenu]
    RV$thintime <- input[[paste0(input$sidebarMenu, '_timeThin')]]
    RV$speedProp <- input[[paste0(input$sidebarMenu, '_speedProp')]]
    RV$circProp <- input[[paste0(input$sidebarMenu, '_circProp')]]
    RV$clustDist <- input[[paste0(input$sidebarMenu, '_clustDist')]]
  })


  for(i in 1:ntabs){
    output[[plotnames[i]]] <- renderPlot({
      dataSubInd <-  dataInp[[RV$indv]]
      
      if(RV$thintime==0){dataSubIndTime <- dataSubInd
      } else {
        # dataSubIndTime <- fishers[[1]]
      dataSubIndTime <- dataSubInd[!duplicated(round_date(timestamps(dataSubInd), paste0(RV$thintime," mins"))),]}
      
      # plot(dataSubIndTime, type="b",pch=20,main=namesIndiv(dataSubIndTime))
      
      # corridorCalc <- corridor(x=dataSubIndTime)
      corridorCalc <- corridor(x=dataSubIndTime, speedProp=RV$speedProp, circProp=RV$circProp, plot=FALSE)
      
      crpts <- corridorCalc@data[burstId(corridorCalc)=="corridor",c("segMid_x","segMid_y")] ## something needs to be included if there is only 1 corridor point identified, as 1 points cannot be clustered and gives an error
      coordinates(crpts) <- ~segMid_x+segMid_y
      projection(crpts) <- projection(dataSubIndTime)
      # points(crpts,col="red")
      ## distance matrix
      dist <- distm(crpts, fun=distGeo)
      ## clustering
      hc <- hclust(as.dist(dist), method="single") # complete single
      # plot(hc)
      # define clusters based on a tree "height" cutoff distance (distance between corridor clusters) and add them to the SpDataFrame
      crpts$clust <- cutree(hc, h=RV$clustDist)

      # add nb of corridor segments per cluster to SpDataFrame
      tbCount <- data.frame(table(crpts$clust))
      for(j in tbCount$Var1){
        crpts$nbCorrInClust[crpts$clust==j] <- tbCount$Freq[tbCount$Var1==j]
      }

      # get the centroid coords for each cluster
      cent <- matrix(ncol=2, nrow=max(crpts$clust))

      cent <- data.frame(x=NA,y=NA,clusterID=unique(crpts$clust))
      for(i in unique(crpts$clust)){
        # gCentroid from the rgeos package
        cent[cent$clusterID==i,c("x","y")] <- gCentroid(subset(crpts, clust == i))@coords
      }
      # compute circles around the centroid coords using "clustDist" radius
      ci <- circles(cent[,c("x","y")], d=RV$clustDist, lonlat=T,dissolve=F)

      if(!any(burstId(corridorCalc)=="corridor")){## if levels do not contain "corridor"
        plot(dataSubIndTime, type="b",pch=20,main=paste0("No corridors found - ",namesIndiv(dataSubIndTime))) # change this
      } else{
      # plot(dataSubIndTime, type="l")
      # plot(ci@polygons, add=T)
      # plot(crpts, col=rainbow(max(crpts$clust))[factor(crpts$clust)], add=T, pch=19)
      # text(cent[,1],cent[,2],pos=3, labels=paste0("Grp:",1:max(crpts$clust)))
      # 
      indDF <- data.frame(long=coordinates(dataSubIndTime)[,1],lat=coordinates(dataSubIndTime)[,2])
      cifort <- fortify(ci@polygons, region="ID")
      
      ggplot()+geom_path(data=indDF,aes(long,lat))+
        geom_point(data=as.data.frame(crpts), aes(segMid_x, segMid_y,color=as.factor(clust)))+
        geom_polygon(data=cifort, aes(long, lat, group=group),colour='red', fill=NA)+
        coord_fixed(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)
}
    })
    # observeEvent(input$plot_dblclick, {
    #   brush <- input$plot_brush
    #   if (!is.null(brush)) {
    #     ranges$x_range <- c(brush$xmin, brush$xmax)
    #     ranges$y_range <- c(brush$ymin, brush$ymax)
    #     
    #   } else {
    #     ranges$x_range <- NULL
    #     ranges$y_range <- NULL
    #   }
    # })
    
   
  }

}

shinyApp(ui, server)



