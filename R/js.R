test_js <- function() {
  assertthat::assert_that(
    exists("session", envir=.GlobalEnv),
    msg="There is no session object in the global environment. Consider create one via econWeb::webdriverChromeSession()."
  )
  rstudioapi::getSourceEditorContext() -> document

  selectLine <- document$selection[[1]]$range$start[[1]]
  document$contents |>
    stringr::str_which("^```") -> line3dots

  if(!length(line3dots)==0){
    jsCodelines <-
      line3dots[
        c(max(which(selectLine >= line3dots)),
          min(which(selectLine <= line3dots)))
      ] + c(1,-1)
    jsCodes <-
      paste0(document$contents[jsCodelines[[1]]:jsCodelines[[2]]], collapse = "\n")
  } else {
    jsCodes <-
      paste0(document$contents, collapse="\n")
  }

  .GlobalEnv$session$executeScript(jsCodes)

  rstudioapi::documentSave(id=document$id)

}
