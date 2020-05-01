#' import shiny
#' import jsonlite
#'
#' @name Chinook
#' @rdname Chinook
#' @aliases Chinook
#------------------------------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------------------------------------------------
.Chinook <- setClass("Chinook",
                       representation = representation(
                           quiet="logical",
                           name="character",
                           state="environment")
                           )
#------------------------------------------------------------------------------------------------------------------------
setGeneric('addTab',          signature='obj', function(obj, tab) standardGeneric("addTab"))
setGeneric('getTabs',         signature='obj', function(obj) standardGeneric("getTabs"))
setGeneric('createUI',        signature='obj', function(obj) standardGeneric("createUI"))
setGeneric('createServer',    signature='obj', function(obj, session, input, output)
                                standardGeneric("createServer"))
setGeneric('createApp',       signature='obj', function(obj, port=NA_integer_) standardGeneric("createApp"))
setGeneric('dispatchMessage', signature='obj', function(obj, source, destination, cmd, json.payload)
                                standardGeneric("dispatchMessage"))
#------------------------------------------------------------------------------------------------------------------------
#' Create an Chinook object
#'
#' @description
#' a shiny app
#'
#' @rdname Chinook
#'
#' @param name  A character string
#'
#' @return An object of the Chinook class
#'
#' @export
#'
Chinook <- function(name, homePage.htmlFile=NA, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   state$tabs <- list()
   if(!is.na(homePage.htmlFile))
      stopifnot(file.exists(homePage.htmlFile))

   state$homePage <- homePage.htmlFile
   .Chinook(name=name, state=state, quiet=quiet)

} # Chinook
#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "Chinook",

    function(object){
        cat(paste("a Chinook object", "\n"))
        cat(sprintf("  tabs: : %s\n", length(obj@state$tabs)))
        })

#------------------------------------------------------------------------------------------------------------------------
#' add a ChinookTab subclass to this Chinook instance
#'
#' @rdname addTab
#'
#' @param obj  A Chinook object
#' @param tab  A ChinookTab object
#'
#' @return nothing
#'
#' @export
#'

setMethod("addTab", "Chinook",

    function(obj, tab){
       obj@state$tabs <- c(obj@state$tabs, tab)
       })

#------------------------------------------------------------------------------------------------------------------------
#' get all the tabs associated with this Chinook instance
#'
#' @rdname getTabs
#'
#' @param obj  A Chinook objet
#'
#' @return A list of ChinookTab objects
#'
#' @export
#'

setMethod("getTabs", "Chinook",

    function(obj){
       return(obj@state$tabs)
       })

#------------------------------------------------------------------------------------------------------------------------
.createSidebar <- function(obj)
{
   menuItems <- list()
   i <- 1
   menuItems[[i]] <- menuItem("Home", tabName="mainTab")

   for(tab in obj@state$tabs){
      i <- i + 1
      menuItems[[i]] <- menuItem(getMenuItemName(tab), tabName=getName(tab))
      } # for tab

   printf("wish to add menuItems for %d tabs", length(obj@state$tabs))

   dashboardSidebar(
      sidebarMenu(id="sidebarMenu", menuItems)
      )

} # .createSidebar
#------------------------------------------------------------------------------------------------------------------------
.createBody <- function(obj)
{
  mainTabContent <-  "<h3> Chinook Home Page</h3>"

  if(!is.na(obj@state$homePage))
    mainTabContent <- includeHTML(obj@state$homePage)

  mainPageTabItem <- tabItem(tabName="mainTab", mainTabContent)
                              # includeHTML(obj@state$homePage))
  tabItemList <- list()
  i <- 1
  tabItemList[[i]] <- mainPageTabItem


  for(tab in obj@state$tabs){
     nextItem <- tabItem(tabName=getName(tab), createPage(tab))
     i <- i + 1
     tabItemList[[i]] <- nextItem
     }

   dashboardBody(
     do.call(tabItems, tabItemList)
     )

} # .createBody
#------------------------------------------------------------------------------------------------------------------------
#' Create the user interface
#'
#' @rdname createUI
#' @aliases createUI
#'
#' @param obj An object of class Chinook
#'
#' @export
#'
setMethod('createUI', 'Chinook',

   function(obj){

      ui <- dashboardPage(
         dashboardHeader(title="Chinook devel"),
         .createSidebar(obj),
         .createBody(obj)
         )

      return(ui)
     })

#------------------------------------------------------------------------------------------------------------------------
#' Create the shiny server
#'
#' @rdname createServer
#' @aliases createServer
#'
#' @param obj An object of class TrenaViz
#' @param session A shiny session
#' @param input A shiny input object
#' @param output A shiny output object
#'
#' @export
#'
setMethod('createServer', 'Chinook',

   function(obj, session, input, output){

      })

#------------------------------------------------------------------------------------------------------------------------
#' Create a runnable shiny app
#'
#' @rdname createApp
#' @aliases createApp
#'
#' @param obj An object of class TrenaViz
#' @param port An integer (e.g., 3838, 60041) NA_integer_ by default
#'
#' @export
#'
setMethod('createApp', 'Chinook',

  function(obj, port=NA_integer_){

    server <- function(session, input, output){
       x <- createServer(obj, session, input, output)
       for(tab in obj@state$tabs){
         printf("server adding event handlers for %s", getName(tab))
         addEventHandlers(tab, session, input, output)
         }
       } # server

    shinyOptions=list(launch.browser=FALSE, host='0.0.0.0', port=port)

    ui <- createUI(obj)
    app <- shinyApp(ui, server, options=shinyOptions)

    return(app)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod('dispatchMessage', signature='Chinook',

    function(obj, source, destination, cmd, json.payload){

      printf("--- Chinook::dispatchMessage")
      printf("  source: %s", source)
      printf("  destination: %s", destination)
      printf("  cmd: %s", cmd)
      printf("  payload: %s", json.payload)
      for(tab in obj@state$tabs){
        if(tolower(destination) == tolower(getName(tab))){
           printf("will send to %s", destination)
           handleMessage(tab, source, destination, cmd, json.payload)
           }
        }
      })

#------------------------------------------------------------------------------------------------------------------------
