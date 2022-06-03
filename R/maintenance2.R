#' Look for path where chromedriver is
#'
#' @return a path
#' @export
whereIsChromeDriver <- function() {
  Sys.getenv("PATH") |>
    stringr::str_split(":") -> path
  chromedriverCheck = function(.x){
    list.files(.x) -> files
    any(stringr::str_detect(files, "chromedriver"))
  }
  purrr::safely(chromedriverCheck) -> chromedriverCheckSafe
  purrr::map_lgl(
    path[[1]], chromedriverCheck
    ) -> results
  results |> which() #View()
  path[[1]][which(results)] |> unique()
}
launchRegularChrome <- function(){
  if(.Platform$OS.type=="unix"){
    system2(command="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome")
  } else {
    system2(command="start chrome")
  }
}
