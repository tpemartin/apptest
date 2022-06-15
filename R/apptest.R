#' Start app test
#'
#' @param name app name
#' @param js js file for testing
#' @param css css file for test
#' @param port port to serve app
#'
#' @return an app environment
#' @export
apptest = function(name="app", js=NA, css=NA, port=8880, google=F){
    App(htmltools::tagList() ,name=name, js=js, css=css, google=google) -> app
  # browser()
  app$create()
  app$setup(app_port=port)
  app$view()
  app=pwa(app)
  return(app)
}

startAppTest = function(){
  paste0(sample(1:8,4, T), collapse = "") |> as.integer() -> port
  apptest(js="appScript.js", css="appStyle.css",
    port=port) ->> app
  appTestEnv = new.env()
  appTestEnv$app = app
  appTestEnv$session=session
  attach(appTestEnv)
  # rm(appTestEnv, envir=.GlobalEnv)
}
startGoogleAPItest = function(){
  googlePort = Sys.getenv("googlePort")
  assertthat::assert_that(
    googlePort!="",
    msg="Please set googlePort in your system environment first"
  )

  apptest(js="appScript.js", css="appStyle.css",
    port=googlePort, google=T) ->> gapp

  appTestEnv = new.env()
  appTestEnv$gapp = gapp
  appTestEnv$session=session
  attach(appTestEnv)
  # rm(appTestEnv, envir=.GlobalEnv)
}
