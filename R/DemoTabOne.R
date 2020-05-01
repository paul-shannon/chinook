#' import shiny
#' import jsonlite
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
        cat(sprintf("a DemoTabOne object with name '%s'\n", getName(object)))
        })

#------------------------------------------------------------------------------------------------------------------------
setMethod("createPage", "DemoTabOne",

   function(obj){

      fluidPage(id="lowercaseViewPageContent",
         fluidRow(
            wellPanel(
              h3(getName(obj),
              selectInput("selectLowerCaseLetter", "Select", letters, width="200px"),
              selectInput("selectLowerCaseMessageDestination", "Send to",
                          c("-", "DemoTabTwo"), width="200px"),
              actionButton("demoTabOneSendButton", "Send"),
              style="background-color: beige")
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

      observeEvent(input$demoTabOneSendButton, ignoreInit=TRUE, {
         destination <- isolate(input$selectLowerCaseMessageDestination)
         payload <- isolate(input$selectLowerCaseLetter)
         if(destination != "-"){
           printf(" send to: %s", destination)
           dispatchMessage(obj@parentApp, obj@name, destination, "defaultOperation",
                           jsonlite::toJSON(payload, auto_unbox=TRUE))
            } # if destination
         }) # button event

      }) # addEventHandlers

#------------------------------------------------------------------------------------------------------------------------
setMethod("handleMessage", "DemoTabOne",

     function(obj, source, destination, cmd, json.payload){
         printf("%s has message from %s, %s(%s)", getName(obj), source, cmd,
                jsonlite::fromJSON(json.payload))
        })

#------------------------------------------------------------------------------------------------------------------------
