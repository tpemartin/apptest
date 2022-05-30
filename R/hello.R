test = function(){
  library(plotly)
  fig <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
  fig |>
    apptest::create_testapp()
  app_port=8881
  start_appservice() -> job
}
