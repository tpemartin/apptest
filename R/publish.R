setup_appPublish <- function() {
  if(file.exists(".Rprofile")){
    linesRead=xfun::read_utf8(
      ".Rprofile"
    )
    setupChunkRange =
      stringr::str_which(
        linesRead,
        "^(# Setup app publishing folder|attach\\(appSetting\\))"
      )
    length(linesRead) -> totalLines

    if(length(setupChunkRange)!=0){
      resetYN = rstudioapi::showQuestion("App publishing setup","App publishing setup is already there. Do you want to reset/overwrite it?", ok="Yes", cancel="No")
      if(!resetYN) return()

      situation="replace"
    } else {
      situation="append"
    }
  } else {
    situation="create"
  }

  publishPath = select_publishPath()
  tayloredText = custom_text(publishPath)


  switch(situation,
    "replace"={
      seq_along(linesRead) |>
        cut(
          unique(c(0, setupChunkRange[[1]]-1, setupChunkRange[[2]], totalLines)), ordered_result=T) -> byBlocks
      split(linesRead, byBlocks) -> split_lines
      targetBlock = paste0("(",setupChunkRange[[1]]-1,",", setupChunkRange[[2]], "]")

      # replace targetBlock
      split_lines[[targetBlock]] = tayloredText
      newLines = unlist(split_lines)
      xfun::write_utf8(
        newLines, con=".Rprofile"
      )
    },
    "append"={
      c(linesRead, tayloredText) |>
        xfun::write_utf8(
          con=".Rprofile"
        )

    },
    "create"={
      tayloredText |>
        xfun::write_utf8(
          con=".Rprofile"
        )
    }
  )
  rstudioapi::showDialog(
    "App publishing setup","App publishing setup is specified in your .Rprofile now. You need to restart your session to make it work."
  )
}




custom_text <- function(publishPath) {
  glue::glue(
    '# Setup app publishing folder
  appSetting=new.env()
  appSetting$pusblishLocalPath = "{publishPath}"
  attach(appSetting)
  ')
}





select_publishPath <- function() {
  rstudioapi::showDialog(
    "App publishing setup",
    message= "Specify your publishing folder path")
  publishPath = rstudioapi::selectDirectory("App publishing setup")
  return(normalizePath(publishPath))
}
