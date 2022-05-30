if(!exists("app_port")) app_port=4321

if(pingr::is_up("http://127.0.0.1", port=app_port)){
  servr::daemon_stop()
}

servr::httd(port= app_port) -> .GlobalEnv$server

