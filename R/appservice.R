#' Generate an app instance
#'
#' @param plt a widget
#' @param name name of the app, default="app"
#' @param js js file name (no need for folder)
#' @param css css file name (no need for folder)
#' @return a list
#' @export
App <- function(plt, name="app", js=NA, css=NA){
  app <- new.env()
  app$create <- function(htmlfile="temp/index.html"){
    app$file <- htmlfile
    create_appMeta(app, plt=plt, name=name, js=js, css=css, newCreate=T)
    create_testapp(app$meta, htmlfile)}
  app$setup <- function(app_port=4321, chromedrivePort=9515){
    app$meta$app_port = app_port
    start_appservice(app$file, chromedrivePort, app_port) -> app$meta$jobId
  }
  app$view <- refreshApp
  app$update <- function(plt=NULL, name=NA, js=NA, css=NA){
    flag_mustUpdateApp = !is.null(plt)
    if(flag_mustUpdateApp) app$meta$appcontent=plt
    flag_jsAlreadyInMeta=!is.null(app$meta$js)
    flag_cssAlreadyInMeta=!is.null(app$meta$css)
    flag_nameChange = !is.na(name) && app$meta$name != name
    flag_jsChange =
      !is.na(js) &&
      (!flag_jsAlreadyInMeta
      || (flag_jsAlreadyInMeta && app$meta$js!=basename(js)))
    flag_cssChange =
      !is.na(css) &&
      (!flag_cssAlreadyInMeta
        || (flag_cssAlreadyInMeta && app$meta$css!=basename(css)))

    flag_dependencyChange = (isTRUE(flag_nameChange) || isTRUE(flag_jsChange) || isTRUE(flag_cssChange))

    flag_pltUpdate = (!is.null(plt) && !identical(app$meta$appcontent,plt)) || !flag_dependencyChange

    if(flag_dependencyChange) updateDependency(app, name=name, js=js, css=css)

    create_testapp(app$meta$appcontent, app$file)
    app$view()
  }
  app$resume <- resumeApp(app)
  app$suspend <- function(){
    rstudioapi::launcherControlJob(
      jobId = app$meta$jobId, operation="suspend"
    )
  }
  app$stop <- function(){
    rstudioapi::launcherControlJob(
      jobId = app$meta$jobId, operation="kill"
    )
  }
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
#' @param appmeta app$meta
#' @param folderpath folder path
#' @param htmlfile html file name.
#'
#' @return the full path to the app which is file.path(folderpath, htmlfile)
#' @export
#'
create_testapp = function(appmeta, htmlfile="temp/index.html"){
  appmeta=app$meta
  if(!is.function(appmeta$dependency) && is.na(appmeta$dependency)){
    tag=appmeta$appcontent
  } else {
    tag=htmltools::tagList(
      appmeta$appcontent,
      appmeta$dependency()
    )
  }
  # tag=app$meta$appcontent
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
resumeApp <- function(app){
  function(chromedrivePort=9515, app_port=app$meta$jobId){

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

