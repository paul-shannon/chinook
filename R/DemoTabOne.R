#' import shiny
#' import cyjShiny
#' import graph
#' @name ChinookTab
#' @rdname ChinookTab
#' @aliases ChinookTab
#------------------------------------------------------------------------------------------------------------------------
.DemoTabOne <- setClass("DemoTabOne", contains="ChinookTab")
#------------------------------------------------------------------------------------------------------------------------
#' Create a DemoTabOne object
#'
#' @description
#'
#' @rdname DemoTabOne
#'
#' @param namem  A character string
#'
#' @return An DemoTabOne object, a subclass of Chinookab
#'
#' @export
#'
DemoTabOne <- function(name, menuItemName, parentApp, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   state$tabs <- list()
   obj <- .DemoTabOne(ChinookTab(name=name, menuItemName=menuItemName, parentApp=parentApp, quiet=quiet))

   obj

} # DemoTabOne
#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "DemoTabOne",

    function(object){
        cat(paste("a DemoTabOne object", "\n"))
        })

#------------------------------------------------------------------------------------------------------------------------
setMethod("createPage", "DemoTabOne",

   function(obj){

      fluidPage(id="lowercaseViewPageContent",
         fluidRow(
            wellPanel(
              h3("LowerCaseView"),
              selectInput("selectLowerCaseLetter", "Select", letters, width="200px"),
              selectInput("selectLowerCaseMessageDestination", "Send to",
                          c(" ", "UpperCase"), width="200px"),
              style="background-color: beige"
              )
           )) # fluidPage
          }) # createPage

#------------------------------------------------------------------------------------------------------------------------
setMethod("displayPage", "DemoTabOne",

   function(obj) {
      removeUI(selector="#lowercaseViewPageContent", immediate=TRUE)
      insertUI(selector="#lowercaseViewPage", where="beforeEnd", createPage(obj), immediate=TRUE)
      }) # displayPage

#------------------------------------------------------------------------------------------------------------------------
setMethod("addEventHandlers", "DemoTabOne",

   function(obj, session, input, output) {
      printf("--- LowerCaseView::addEventHandlers")
      obj@state$session <- session
      obj@state$input <- input
      obj@state$output <- output

      output$lowercaseValue <- renderText({input$lowercaseCaption})

      }) # addEventHandlers

#------------------------------------------------------------------------------------------------------------------------
