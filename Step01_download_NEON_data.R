# download.file("https://github.com/cwida/duckdb/releases/download/master-builds/duckdb_r_src.tar.gz", destfile = "~/duckdb_r_src.tar.gz")
# install.packages("duckdb_r_src.tar.gz", repo = NULL)
# /groups/rqthomas_lab/neonstore
# remotes::install_github("cwida/duckdb/tools/rpkg", build = FALSE)
remotes::install_github("cboettig/neonstore")
# Sys.setenv("NEONSTORE_HOME" = "/groups/rqthomas_lab/neonstore3")

# Sys.setenv("NEONSTORE_HOME" = "/groups/rqthomas_lab/neonstore2")
# success <- lapply(neonstore::neon_index()$path, function(x)Sys.chmod(x, 677))
# prod <- neonstore::neon_index()$product[unlist(success)]
# unique(prod)
# dir.create("neonstore")
Sys.setenv("NEONSTORE_HOME" = "C:/Users/tadhgm/Desktop/neonstore")
Sys.setenv("NEONSTORE_DB" = "C:/Users/tadhgm/Desktop/neonstore")
success <- lapply(neonstore::neon_index()$path, Sys.chmod, "644")
Sys.setenv("NEON_TOKEN" = "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ0YWRoZ21AdnQuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzYwOTk3MDAxLCJpYXQiOjE2MDMzMTcwMDEsImVtYWlsIjoidGFkaGdtQHZ0LmVkdSJ9.Vzz0vAyZQYQfLbZkvy7jZc15bRwgOU6XoRBTbGgy6tf4uYssPnHhkOzVzJuBNALckonhXJsvla1nfLh_bjsvvw")

# DP1.20093.001 - Chemical properties of surface water
# DP1.20288.001 - Water quality
# DP1.00002.001 - Air temperature
# DP1.20219.001 - Zooplankton
# DP1.20033.001 - Nitrate in surface water
# DP1.20264.001 - Temperature at specific depths
# DP1.20048.001 - Stream discharge field collection
# DP1.20252.001 - Secchi Depth
# DP1.20046.001 - Air temperature of lakes on buoy
# DP4.00130.001 - Stream discharge - Streams
# DP1.20053.001 - Temperature (PRT) in surface water
# DP1.20264.001 - Temperature at specific depth in lakes
# DP1.20097.001 - Dissolved gases in surface water
# DP1.20261.001 - Photosynthetically active radiation below water surface
# DP1.20042.001 - Photosynthetically active radiation at water surface
# DP1.00006.001 - Precipitation


# library(tidyverse)

products <- c("DP1.20093.001",
              "DP1.20288.001", # Failed!
              "DP1.00002.001",
              "DP1.20219.001", "DP1.20033.001", "DP1.20264.001",
              "DP1.20048.001", "DP1.20252.001", "DP1.20046.001",
              "DP4.00130.001", "DP1.20053.001", "DP1.20264.001",
              "DP1.20097.001", "DP1.20261.001", "DP1.20042.001",
              "DP1.00006.001"
  )
sites <- c("BARC", "CRAM", "SUGG", "PRPO", "LIRO", "PRLA")

# product <-'DP1.20252.001' # "DP1.20033.001"

lapply(products, function(x){
  tryCatch({
    neonstore::neon_download(product = x, site = sites)
  })
})
