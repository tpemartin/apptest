create_appMeta <- function(app, plt, name="app", js=NA, css=NA, newCreate=T) {
  if(newCreate){
    app$meta=list(
      appcontent=plt,
      js=basename2(js),
      css=basename2(css),
      name=name,
      dependency=NA,
      dirpath=file.path("./inst", name)
    )
  }

  if(
    !newCreate &&
    isTRUE(js==app$meta$js) && isTRUE(css==app$meta$css)) return()
  # if(!is.na(js)){
  #   app$meta$js=file.path(app$meta$dirpath,basename2(js))
  #   file.edit2(app$meta$js)
  # }
  # if(!is.na(css)){
  #   app$meta$css=file.path(app$meta$dirpath,basename2(css))
  #   file.edit2(app$meta$css)
  # }
  # browser()
  if(!is.na(js) || !is.na(css)){
    # debug(create_appDepExpr)
    eval(
      create_appDepExpr(
        name=app$meta$name,
        js=basename2(js),
        css=basename2(css)),
      envir = rlang::current_env()
    )
    dep=appDependency()
    dirpath = dep$src$file

    if(length(dep$script)!=0) file.edit2(file.path(dirpath, dep$script))
    if(length(dep$stylesheet)!=0) file.edit2(file.path(dirpath, dep$stylesheet))

    # browser()
    # eval(app_depFunExpr, envir=rlang::current_env())
    app$meta$dependency <-
      appDependency
    app$meta$dependencyScript <- appDependency |> body()

    htmltools::attachDependencies(
      app$meta$appcontent,
      appDependency()) ->
      app$meta$appcontent
  }
}
updateDependency = function(app, name=NA, js=NA, css=NA){
  flag_noname = is.na(name)
  flag_nojs = is.na(js)
  flag_nocss = is.na(css)
  flag_just2update = flag_nocss && flag_nojs && flag_noname
  if(flag_just2update){
    js=app$meta$js
    css=app$meta$css
    name=app$meta$name
  } else{
    if(!is.na(name)){app$meta$name=name} else {name=app$meta$name}
    if(!is.na(js)){ app$meta$js=basename2(js)} else {js=app$meta$js}
    if(!is.na(css)) {app$meta$css=basename2(css)} else {
      css=app$meta$css
    }
  }

  # debug(create_appDepExpr)
  eval(
    create_appDepExpr(
      name=app$meta$name,
      js=basename2(js),
      css=basename2(css)),
    envir = rlang::current_env()
  )
  dep=appDependency()
  dirpath = dep$src$file

  if(length(dep$script)!=0 && !flag_nojs && !flag_just2update) file.edit2(file.path(dirpath, dep$script))
  if(length(dep$stylesheet)!=0 && !flag_nocss && !flag_just2update) file.edit2(file.path(dirpath, dep$stylesheet))
  # browser()
  # eval(app_depFunExpr, envir=rlang::current_env())
  app$meta$dependency <-
    appDependency
  app$meta$dependencyScript <- appDependency |> body()

  htmltools::attachDependencies(
    app$meta$appcontent,
    appDependency()) ->
    app$meta$appcontent
}
file.edit2 <- function(css){
  if(!dir.exists(dirname(css))) dir.create(dirname(css), recursive=T)
  file.edit(css)
}

create_appDepExpr = function(name,js, css){
  js= basename2(js, default=NULL)
  css=basename2(css, default=NULL)
  # if(!is.na(js)){
  #   file.path("js", basename(js))
  # } else {NULL}
  # css= if(!is.na(css)){
  #   file.path("css", basename(css))
  # } else {NULL}

  if(!is.null(css) || !is.null(js)){
    app_depFunExpr <-
      rlang::expr(
        appDependency <- function() {
          htmltools::htmlDependency(
            name = !!name,
            version = "1.0",
            src = c(file = !!normalizePath(file.path("./inst", name))),
            script = !!file.path("js",js),
            stylesheet = !!file.path("css",css)
          )
        }
      )
  } else {
    app_depFunExpr <- NA
  }
  app_depFunExpr
}
basename2 = function(file, default=NULL){
  if(!(is.null(file) || is.na(file))){
    x= basename(file)
  } else {
    x=default
  }
  return(x)
}
split_filepath = function(file){
  ffolder = dirname(file)
  ffile=basename(file)
  list(
    folder=ffolder,
    file=ffile
  )
}
