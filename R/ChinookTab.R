#' import shiny
#' import cyjShiny
#' import graph
#' @name ChinookTab
#' @rdname ChinookTab
#' @aliases ChinookTab
#------------------------------------------------------------------------------------------------------------------------
.ChinookTab <- setClass("ChinookTab",
                             representation = representation(
                                quiet="logical",
                                name="character",
                                state="environment")
                                )
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
ChinookTab <- function(name, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   state$tabs <- list()
   .ChinookTab(name=name, tabs=tabs, quiet=quiet)

} # ChinookTab
#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "ChinookTab",

    function(object){
        cat(paste("a ChinookTab object", "\n"))
        cat(sprintf("  tabs: : %s\n", length(obj@state$tabs)))
        })

#------------------------------------------------------------------------------------------------------------------------
