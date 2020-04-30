#' import shiny
#'
#' @name Chinook
#' @rdname Chinook
#' @aliases Chinook
#------------------------------------------------------------------------------------------------------------------------
.Chinook <- setClass("Chinook",
                             representation = representation(
                                quiet="logical",
                                name="character",
                                state="environment")
                                )
#------------------------------------------------------------------------------------------------------------------------
setGeneric('getTabs',  signature='obj', function(obj) standardGeneric("getTabs"))
setGeneric('createUI',      signature='obj', function(obj) standardGeneric("createUI"))
setGeneric('createServer',  signature='obj', function(obj, session, input, output) standardGeneric("createServer"))
setGeneric('createApp',     signature='obj', function(obj, port=NA_integer_) standardGeneric("createApp"))
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
Chinook <- function(name, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   state$tabs <- list()
   .Chinook(name=name, state=state, quiet=quiet)

} # Chinook
#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "Chinook",

    function(object){
        cat(paste("a Chinook object", "\n"))
        cat(sprintf("  tabs: : %s\n", length(obj@state$tabs)))
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
.createSidebar <- function()
{
   dashboardSidebar(
   sidebarMenu(id="sidebarMenu",
       menuItem("Home",              tabName = "mainTab")
       )
    )

} # .createSidebar
#------------------------------------------------------------------------------------------------------------------------
.createBody <- function()
{
   dashboardBody(
      useShinyjs(),
      tabItems(
         .createMainTab()
         ))

} # .createBody
#------------------------------------------------------------------------------------------------------------------------
.createMainTab <- function()
{
  tabItem(tabName="mainTab",
     h3("Chinook: placeholder for introduction page")
     )

} # .createMainTab
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
         .createSidebar(),
         .createBody()
         ) # dashboardPage

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
       }

    shinyOptions=list(launch.browser=FALSE, host='0.0.0.0', port=port)

    app <- shinyApp(createUI(obj), server, options=shinyOptions)

    return(app)

    })

#------------------------------------------------------------------------------------------------------------------------
