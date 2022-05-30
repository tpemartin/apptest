serverJob = function(){
  system.file("server/appserving.R", package="apptest") -> jobscript
  httpuv::stopAllServers()
  rstudioapi::jobRunScript(
    jobscript, workingDir=file.path(rprojroot::is_rstudio_project$make_fix_file()())
  )
}


