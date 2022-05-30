#' As browseURL but it browse a shiny.tag or html class object in RStudio Viewer
#'
#' @param tag A shiny.tag or html class object
#'
#' @return app show in viewer and app file in temp/index.html
#' @export
#'
showWidget <- function(tag=.Last.value, htmlfile="index.html"){
  if(!dir.exists("temp")) dir.create("temp")
  servr::daemon_stop()
  htmltools::save_html(
    htmltools::tagList(tag, dep_mobile()), file=file.path("temp",htmlfile)
  )
  ss <- servr::httd("temp")
  # ss$port
  rstudioapi::viewer(glue::glue("http://127.0.0.1:{ss$port}/{htmlfile}"))
}
dep_mobile <- function(){
  htmltools::htmlDependency(
    name="temp",
    version="1.0.0",
    src=c(file="assets/"),
    meta=list(
      viewport="width=device-width, initial-scale=1.0"
    )
  ) -> dep_mobile
}
getDupnames <- function(argList) {
  argList |>
    purrr::map(names) -> listnames
  unlist(listnames) -> allnames
  table(allnames) -> tbnames

  duplicateNames = names(tbnames)[tbnames!=1]
  okaynames = c(names(tbnames)[tbnames==1],"")
  list(
    duplicateNames=duplicateNames,
    okaynames=okaynames)
}
get1stLevelLists2merge <- function(argList) {
  listNames= setdiff(getDupnames(argList),"")
  duplicateNames=listNames$duplicateNames
  okNames = listNames$okaynames
  argList |>
    purrr::map(names) -> listnames
  purrr::map(
    seq_along(argList),
    ~{pickOK = (names(argList[[.x]]) %in% okNames)
    if(sum(pickOK)==0){
      list()
    } else {
      argList[[.x]][pickOK]
    }
    }) -> listOK
  unlist(listOK, recursive=F) -> listOK

  dupList <- vector("list", length(duplicateNames))
  if(length(duplicateNames)==0){
    return(list())
  } else {
    for(.x in seq_along(duplicateNames)){
      dupnameX = duplicateNames[[.x]]
      listnames |> purrr::map_lgl(
        function(x) purrr::has_element(x, dupnameX)
      ) -> pickListWithDupnamex
      argList[pickListWithDupnamex] |>
        purrr::map(
          function(x) purrr::pluck(x, dupnameX)
        ) -> list2merge
      dupList[[.x]]=list2merge
    }
    names(dupList) = duplicateNames

  }

  c(dupList, listOK)
}
#' Merge two list while respecting original list structure
#'
#' @param ... list arguments
#'
#' @return a merged list
#' @export
#'
merge_list <- function(...){
  argList = list(...)
  purrr::map_lgl(
    argList,
    ~length(.x)!=0
  ) -> pick_noEmpty

  append(list(list()), argList[pick_noEmpty]) -> seqList
  seqList |>
    purrr::reduce(recreate_list)
}
recreate_list <- function(result=list(), .list) {
  unlist(.list) -> unlistArgs
  # list(a,b) |> unlist() -> unlistArgs
  # unlist(a) -> unlistArgs
  # unlistArgs
  unlistArgs |> names() |>
    sort() |>
    stringr::str_extract_all("[^.]+","") -> listNames
  listRetrieval <- vector("list", length(listNames[,1]))
  for(.x in seq_along(listNames[,1])){
    #.x=2
    typeof(unlistArgs[[1]])

    listNames[.x,] |> na.omit() -> listNamesX
    paste0(paste0("[['",listNamesX, "']]"), collapse = "") -> retrievalX
    typeof(unlistArgs[[.x]]) -> argType
    paste0("result", retrievalX,("<- .list"), retrievalX) -> listRetrieval[[.x]]

    #unlistArgs[[.x]]") ->listRetrieval[[.x]]
  }
  # result=list()
  callenv=rlang::current_env()
  purrr::walk(
    seq_along(listRetrieval),
    ~eval(parse(text=listRetrieval[[.x]]), envir = callenv)
  )
  result
}
