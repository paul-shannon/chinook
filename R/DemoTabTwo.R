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
              h3(getName(obj),
              selectInput("selectUpperCaseLetter", "Select", LETTERS, width="200px"),
              selectInput("selectUpperCaseMessageDestination", "Send to",
                          c("-", "DemoTabOne"), width="200px"),
              actionButton("demoTabTwoSendButton", "Send"),
              style="background-color: beige")
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

      observeEvent(input$demoTabTwoSendButton, ignoreInit=TRUE, {
         destination <- isolate(input$selectUpperCaseMessageDestination)
         payload <- isolate(input$selectUpperCaseLetter)
         if(destination != "-"){
           printf(" send to: %s", destination)
           dispatchMessage(obj@parentApp, obj@name, destination, "defaultOperation",
                           jsonlite::toJSON(payload, auto_unbox=TRUE))
            } # if destination
         }) # button event



      }) # addEventHandlers

#------------------------------------------------------------------------------------------------------------------------
setMethod("handleMessage", "DemoTabTwo",

     function(obj, source, destination, cmd, json.payload){
         printf("%s has message from %s, %s(%s)", getName(obj), source, cmd,
                jsonlite::fromJSON(json.payload))
        })

#------------------------------------------------------------------------------------------------------------------------
