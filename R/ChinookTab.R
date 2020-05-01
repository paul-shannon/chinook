#' import shiny
#' import cyjShiny
#' import graph
#' @name ChinookTab
#' @rdname ChinookTab
#' @aliases ChinookTab
#------------------------------------------------------------------------------------------------------------------------
.ChinookTab <- setClass("ChinookTab",
                             representation = representation(
                                parentApp="Chinook",
                                quiet="logical",
                                name="character",
                                menuItemName="character",
                                state="environment")
                                )
#------------------------------------------------------------------------------------------------------------------------
setGeneric("getName",          signature="obj", function(obj) standardGeneric("getName"))
setGeneric("getMenuItemName",  signature="obj", function(obj) standardGeneric("getMenuItemName"))
setGeneric("createPage",       signature="obj", function(obj) standardGeneric("createPage"))
setGeneric("displayPage",      signature="obj", function(obj, tf) standardGeneric("displayPage"))
setGeneric("addEventHandlers", signature="obj", function(obj, session, input, output)
                               standardGeneric("addEventHandlers"))
setGeneric("handleMessage",    signature="obj", function(obj, source, destination, cmd, json.payload)
                               standardGeneric("handleMessage"))
#------------------------------------------------------------------------------------------------------------------------
#' Create an ChinookTab object
#'
#' @description
#' a shiny app
#'
#' @rdname ChinookTab
#'
#' @param namem  A character string
#'
#' @return An object of the ChinookTab class
#'
#' @export
#'
ChinookTab <- function(name, menuItemName, parentApp, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   .ChinookTab(name=name, menuItemName=menuItemName, parentApp=parentApp, quiet=quiet)

} # ChinookTab
#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "ChinookTab",

    function(object){
        cat(paste("a ChinookTab object", "\n"))
        })

#------------------------------------------------------------------------------------------------------------------------
#' get the name of this tab
#'
#' @rdname getName
#' @aliases getName
#'
#' @param obj An object of class TrenaViz
#'
#' @return character string
#'
#' @export
#'
setMethod("getName", "ChinookTab",

    function(obj){
        obj@name
        })

#------------------------------------------------------------------------------------------------------------------------
#' get the menu item name associated with this tab
#'
#' @rdname getMenuItemName
#' @aliases getMenuItemName
#'
#' @param obj An object of class TrenaViz
#'
#' @return character string
#'
#' @export
#'
setMethod("getMenuItemName", "ChinookTab",

    function(obj){
        obj@menuItemName
        })

#------------------------------------------------------------------------------------------------------------------------
