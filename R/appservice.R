#' Generate an app instance
#'
#' @return a list
#' @export
App <- function(plt){
  app <- new.env()
  app$create <- function(htmlfile="temp/index.html"){
    app$file <- htmlfile
    create_testapp(plt, htmlfile)}
  app$setup <- function(app_port=4321, chromedrivePort=9515){
    start_appservice(app$file, chromedrivePort, app_port)
  }
  app$view <- refreshApp
  app$update <- function(plt){
    create_testapp(plt, app$file)
    app$view()
  }
  app$restart <- restartApp(app)
  app
}

#' Start to serve web app at temp/ and lauch chromedriver for inspection
#'
#' @return a job id
#' @export
start_appservice <- function(appPath="./temp/index.html", chromedrivePort=9515, app_port=4321){

  .GlobalEnv$app_port <- app_port
  appPath=normalizePath(appPath)
  system.file("server/appserving.R", package="apptest") -> jobscript

  # httpuv::stopDaemonizedServer()

  rstudioapi::jobRunScript(
    jobscript,
    workingDir=dirname(appPath), #normalizePath("./temp"),
    importEnv = T,
    exportEnv="R_GlobalEnv") -> jobId
  if(!exists("session", envir = .GlobalEnv)){
    .GlobalEnv$session=webdriverChromeSession(chromedrivePort)
    .GlobalEnv$session$start_session()

  }

  appUrl <- glue::glue("http://127.0.0.1:{app_port}/{basename(appPath)}")
  .GlobalEnv$session$go(appUrl)
  .GlobalEnv$session$appUrl <- appUrl
  .GlobalEnv$session$appRefresh = function(){
    .GlobalEnv$session$go(.GlobalEnv$session$appUrl)
  }
  invisible(jobId)
}
#' chromedriver webdrive session starter
#'
#' @return an environment with $start_session() to start a webdriver session connected to chromedriver; with $kill_chrome() to kill chromedriver process.
#' @export
#'
webdriverChromeSession <- function(chromedrivePort=9515) {
  sessionNew = new.env()
  # sessionNew$p <- processx::process$new("chromedriver", stdout="|")
  tryCatch({
    sessionNew$p <- processx::process$new("chromedriver", stdout = "|")
  },
    error=function(e){
      warning("Cannot locate chromedriver.")
      message("Make sure chromedriver is installed in one of Sys.getenv(\"PATH\") paths.")
    })

  sessionNew$kill_chrome = sessionNew$p$kill
  # sessionNew$p$read_output() -> stdout

  # browser()

  sessionNew$start_session = function(){
    webdriver::Session$new(
      port=chromedrivePort) -> sessionx
    allmethods = names(sessionx)
    purrr::walk(
      allmethods,
      ~assign(.x, sessionx[[.x]], envir = sessionNew
    ))
    #sessionx$showWidget = chromeDriverShowWidget(sessionx)

  }
  sessionNew}

get_port <- function(stdout) {
  stdout |>
    stringr::str_extract("(?<=(on\\sport\\s))[0-9]+") |> as.integer() -> port
  port
}
chromeDriverShowWidget <- function(sessionx){
  function(tag=.Last.value){
    if(!dir.exists("temp")) dir.create("temp")
    servr::daemon_stop()
    htmltools::save_html(
      htmltools::tagList(tag, dep_mobile()), file=file.path("temp","temp.html")
    )
    ss <- servr::httd("temp")
    # ss$port
    # rstudioapi::viewer(glue::glue("http://127.0.0.1:{ss$port}/temp.html"))
    sessionx$go(glue::glue("http://127.0.0.1:{ss$port}/temp.html"))
  }
}
#' Create a web app from shiny tag
#'
#' @param tag a shiny tag
#' @param folderpath folder path
#' @param htmlfile html file name.
#'
#' @return the full path to the app which is file.path(folderpath, htmlfile)
#' @export
#'
create_testapp = function(tag=.Last.value, htmlfile="temp/index.html"){
  folderpath = dirname(htmlfile)
  htmlfile=basename(htmlfile)
  if(!dir.exists(folderpath)) dir.create(folderpath)
  if(!any(stringr::str_detect(
    list.files(normalizePath(glue::glue("./{folderpath}"))),
    "\\.Rproj$"))){
    rstudioapi::initializeProject(normalizePath(glue::glue("./{folderpath}")))
  }

  htmltools::save_html(
    htmltools::tagList(tag, dep_mobile()), file=file.path(folderpath,htmlfile)
  )
  file.path(folderpath, htmlfile)
}
refreshApp <- function(){
  assertthat::assert_that(
    exists("session", envir=.GlobalEnv),
    msg="No session in the global environment."
  )
  session$appRefresh()
}
restartApp <- function(app){
  function(chromedrivePort=9515, app_port=4321){

    .GlobalEnv$app_port <- app_port
    appPath=normalizePath(app$file)
    system.file("server/appserving.R", package="apptest") -> jobscript

    # httpuv::stopDaemonizedServer()

    rstudioapi::jobRunScript(
      jobscript,
      workingDir=dirname(appPath), #normalizePath("./temp"),
      importEnv = T,
      exportEnv="R_GlobalEnv") -> jobId
    invisible(jobId)
  }
}

