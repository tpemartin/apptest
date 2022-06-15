pwa= function(app){
  app$createPWA =createPWA
  return(app)
}

createPWA=function(pageTag){
  exprPageTag=rlang::enexpr(pageTag)
  createShinyApp(exprPageTag)
  createPWAscript(exprPageTag)
}
createShinyApp = function(exprPageTag, pwaPath="."){
  # quoPageTag=rlang::enquo(pageTag)
  shinyTemplate = obtain_shinyTemplate(exprPageTag)
  #pwaPath = "inst/pwa"
  if(!dir.exists(pwaPath)) dir.create(pwaPath)
  xfun::write_utf8(shinyTemplate,con=file.path(pwaPath, "app.R"))
  charpente::set_pwa(pwaPath)
  message(file.path(pwaPath, "app.R"), " is created.")
}
createPWAscript = function(exprPageTag){
  rlang::expr_deparse(exprPageTag) -> pageTagString
  stringr::str_extract(pageTagString, "[^\\(]+(?=\\()") -> funname
  glue::glue("pwa_<<funname>>=function(){<<pageTagString>> |> add_pwa_deps() |> add_pwacompat_deps()}",
    .open="<<", .close=">>") -> pwaScript
  xfun::write_utf8(
    pwaScript, con="R/pwa.R"
  )
  message("R/pwa.R is created.")
  devtools::load_all(".")
  pwaPath = normalizePath("/www")
  glue::glue('pwa_<<funname>>() |>
    htmltools::save_html(
      "<<pwaPath>>/index.html"
    )',
    .open="<<", .close=">>") -> saveScript
  rlang::eval_bare({
    library(htmltools)
  })
  rlang::eval_bare(
    rlang::parse_expr(saveScript)
  )


}
movePWA2Doc = function(pwaPath = "."){
  #pwaPath = "inst/pwa"
  pwaWWW=file.path(pwaPath,"www")
  fromFiles = list.files(pwaWWW, full.names = T)
  fromDir = file.path(pwaWWW,"lib")
  file.copy(from=fromFiles, to="docs", overwrite = T)
  file.copy(from=fromDir, to="docs", overwrite=T, recursive = T)
}
movePWA2Temp = function(pwaPath = "."){
  #pwaPath = "inst/pwa"
  pwaWWW=file.path(pwaPath,"www")
  fromFiles = list.files(pwaWWW, full.names = T)
  fromDir = file.path(pwaWWW,"lib")
  file.copy(from=fromFiles, to="temp", overwrite = T)
  file.copy(from=fromDir, to="temp", overwrite=T, recursive = T)
}
obtain_shinyTemplate = function(exprPageTag){
glue::glue("ui={<<rlang::expr_deparse(exprPageTag)>> |> add_pwa_deps() |> add_pwacompat_deps()}

server=function(input, output,session){}

shiny::shinyApp(ui=ui, server=server)",
  .open="<<", .close=">>") }

