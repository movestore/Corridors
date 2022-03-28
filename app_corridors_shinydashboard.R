library(shiny)
library(shinydashboard)
# library(shinydashboardPlus)
library(move)
library(ggplot2)
library(shinyWidgets)
library(shinyBS) ## to display message when hovering over input element in UI
library(lubridate)
library(geosphere)
library(dismo)
library(rgeos)
library(stringr)

## input data 
data(fishers)
dataInp <- fishers

# dataInp <- readRDS("/home/anne/MoveAppsGit/allesMoegliche/data/carn.rds")
####


ui <- dashboardPage(
  dashboardHeader(title = "Corridors"),
  dashboardSidebar(uiOutput("Sidebar")),
  dashboardBody(uiOutput("TabUI")),
  skin = 'green'
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
  
  output$Sidebar <- renderUI({
    Menus <- vector("list", ntabs)
    for(i in 1:ntabs){
      Menus[[i]] <-   menuItem(tabnames[i], icon=icon("paw"), tabName = tabnames[i], selected = i==1) } #icon(name="scribble",class="fa-thin") <i class="fa-solid fa-code-merge"></i> <i class="fa-solid fa-circle-nodes"></i>
    do.call(function(...) sidebarMenu(id = 'sidebarMenu', ...), Menus)
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
                             column(3, numericInput(timeThinnames[i],"Thin track to X mins",value=15),
                                    bsTooltip(id=timeThinnames[i], title="This is specially recomended for high resolution tracks to ease finding reagions wtih paralell segments", placement = "bottom", trigger = "hover", options = list(container = "body"))),
                             column(3, numericInput(clustDistnames[i],"Distance between corridor clusters (mts)",value=300),
                                    bsTooltip(id=clustDistnames[i], title="The radius of the cicles displayed on the map correspond to this value. All identified 'corridor segments' that fall within each cicle will be identified as a corridor", placement = "bottom", trigger = "hover", options = list(container = "body")))
                           ),
                           plotOutput(plotnames[i])
      )
    }
    do.call(tabItems, Tabs)
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

      dataSubIndTime <- dataSubInd[!duplicated(round_date(timestamps(dataSubInd), paste0(RV$thintime," mins"))),]
      plot(dataSubIndTime, type="b",pch=20,main=namesIndiv(dataSubIndTime))
      
      corridorCalc <- corridor(x=dataSubIndTime, speedProp=RV$speedProp, circProp=RV$circProp, plot=FALSE)
      crpts <- corridorCalc@data[burstId(corridorCalc)=="corridor",c("segMid_x","segMid_y")]
      coordinates(crpts) <- ~segMid_x+segMid_y
      projection(crpts) <- projection(dataSubIndTime)
      points(crpts,col="red")

    })
  }

}

shinyApp(ui, server)

