library(Chinook)
library(RUnit)
#----------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()

} # runTests
#----------------------------------------------------------------------------------------------------
test_constructor <- function()
{
    message(sprintf("--- test_constructor"))
    x <- Chinook("demo")
    checkEquals(length(getTabs(x)), 0)

} # test_constructor
#----------------------------------------------------------------------------------------------------
test_minimalAppNoTabs <- function()
{
    message(sprintf("--- test_minimalAppNoTabs"))
    x <- Chinook("demo")
    createApp(x, 10001)
    checkEquals(length(getTabs(x)), 0)

} # test_constructor
#----------------------------------------------------------------------------------------------------
if(!interactive())
    runTests()
