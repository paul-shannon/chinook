library(Chinook)
library(RUnit)
#----------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()
   test_DemoTabOne()
   test_DemoTabTwo()

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
test_DemoTabOne <- function()
{
   message(sprintf("--- test_DemoTabOne"))

   c <- Chinook("demo")
   t <- DemoTabOne(name="tabOne", menuItemName="One", parentApp=c)
   checkTrue(all(c("ChinookTab", "DemoTabOne") %in% is(t)))

} # test_DemoTabOne
#----------------------------------------------------------------------------------------------------
test_DemoTabTwo <- function()
{
   message(sprintf("--- test_DemoTabTwo"))

   c <- Chinook("demo")
   t <- DemoTabTwo(name="tabTwo", menuItemName="Two", parentApp=c)
   checkTrue(all(c("ChinookTab", "DemoTabTwo") %in% is(t)))

} # test_constructor
#----------------------------------------------------------------------------------------------------
test_twoTabDemo <- function()
{
   message(sprintf("--- test_twoTabDemo"))

   homePage <- system.file(package="Chinook", "extdata", "demoHomePage.html")
   checkTrue(file.exists(homePage))

   c <- Chinook("demo", homePage)
   t.1 <- DemoTabOne(name="tabOne", menuItemName="One", parentApp=c)
   t.2 <- DemoTabTwo(name="tabTwo", menuItemName="Two", parentApp=c)

   addTab(c, t.1)
   addTab(c, t.2)

   createApp(c, 10001)

   checkTrue(all(c("ChinookTab", "DemoTabOne") %in% is(t)))

} # test_constructor
#----------------------------------------------------------------------------------------------------
if(!interactive())
    runTests()
