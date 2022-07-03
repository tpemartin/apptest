#' Start Ngrok local host tunneling service
#'
#' @return terminal with tunnel information
#' @export
start_ngrok = function(){
  assertthat::assert_that(
    exists("app"),
    msg='You need to start App test first.'
  )

  rstudioapi::terminalExecute(glue::glue("ngrok http {app$meta$app_port}")) -> tId

  rstudioapi::terminalActivate(tId, show = T)
}
