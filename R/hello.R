test = function(){
  library(plotly)
  plt <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
  plt |> App(name="dash", js="script.js") -> app
  app$create()
  app$setup()
  app$view()
  session$go("https://www.ntpu.edu.tw")
  plt |>
    app$update(js="myscript2.js")
    apptest::create_testapp()
  app_port=8881
  start_appservice() -> job
}
