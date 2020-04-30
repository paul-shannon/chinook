#' import shiny
#' import cyjShiny
#' import graph
#' @name ChinookTab
#' @rdname ChinookTab
#' @aliases ChinookTab
#------------------------------------------------------------------------------------------------------------------------
.DemoTabTwo <- setClass("DemoTabTwo", contains="ChinookTab")
#------------------------------------------------------------------------------------------------------------------------
#' Create a DemoTabTwo object
#'
#' @description
#'
#' @rdname DemoTabTwo
#'
#' @param namem  A character string
#'
#' @return An DemoTabTwo object, a subclass of Chinookab
#'
#' @export
#'
DemoTabTwo <- function(name, menuItemName, parentApp, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   state$tabs <- list()
   obj <- .DemoTabTwo(ChinookTab(name=name, menuItemName=menuItemName, parentApp=parentApp, quiet=quiet))

   obj

} # DemoTabTwo
#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "DemoTabTwo",

    function(object){
        cat(paste("a DemoTabTwo object", "\n"))
        })

#------------------------------------------------------------------------------------------------------------------------
setMethod("createPage", "DemoTabTwo",

   function(obj){

      fluidPage(id="uppercaseViewPageContent",
         fluidRow(
            wellPanel(
              h3("UpperCaseView"),
              selectInput("selectUpperCaseLetter", "Select", LETTERS, width="200px"),
              selectInput("selectUpperCaseMessageDestination", "Send to",
                          c(" ", "UpperCase"), width="200px"),
              style="background-color: beige"
              )
           )) # fluidPage
          }) # createPage

#------------------------------------------------------------------------------------------------------------------------
setMethod("displayPage", "DemoTabTwo",

   function(obj) {
      removeUI(selector="#uppercaseViewPageContent", immediate=TRUE)
      insertUI(selector="#uppercaseViewPage", where="beforeEnd", createPage(obj), immediate=TRUE)
      }) # displayPage

#------------------------------------------------------------------------------------------------------------------------
setMethod("addEventHandlers", "DemoTabTwo",

   function(obj, session, input, output) {
      printf("--- UpperCaseView::addEventHandlers")
      obj@state$session <- session
      obj@state$input <- input
      obj@state$output <- output

      output$uppercaseValue <- renderText({input$uppercaseCaption})

      }) # addEventHandlers

#------------------------------------------------------------------------------------------------------------------------
