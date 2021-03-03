func_fmt_data_d_m_Y <- function(data){
  format(as.Date(data), "%d/%m/%Y")
}

func_fmt_data_teste <- function(data_teste){
  data_teste <- Sys.time()
  typeof(data_teste)
  data_teste <- as.POSIXct(data_teste)
  format(as.Date(data_teste), "%d/%m/%Y")
}