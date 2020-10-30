# download.file("https://github.com/cwida/duckdb/releases/download/master-builds/duckdb_r_src.tar.gz", destfile = "~/duckdb_r_src.tar.gz")
# install.packages("duckdb_r_src.tar.gz", repo = NULL)
# /groups/rqthomas_lab/neonstore
remotes::install_github("cwida/duckdb/tools/rpkg", build= FALSE)
remotes::install_github("cboettig/neonstore")
Sys.setenv("NEONSTORE_HOME" = "/groups/rqthomas_lab/neonstore")
site <- c("BARC")

print("Downloading: DP1.20288.001")
neonstore::neon_download(product = "DP1.20288.001", site = site, type = "basic")

neonstore::neon_store(product = "DP1.20288.001")

neonstore::neon_read(product= "DP1.20288.001", table = "dep_secchi-basic")

df <- neonstore::neon_table(site = sites, )


# -----------------------------------------------------------------------------------------------------------------

library(neonstore)

site <- "BARC"


neonstore::neon_dir()
Sys.setenv(NEONSTORE_HOME = "/groups/rqthomas_lab/neonstore")
neonstore::neon_dir()

# The current data products include: 
# Gauge height = DP1.20267.001
# PAR below surface = DP1.20261.001
# PAR above surface = DP1.20042.001
# Periphyton, seston, phyto = DP1.20163.001
# Secchi depth = DP1.20252.001
# Dissolved gases = DP1.20097.001
# Chemical properties of surface water = DP1.20093.001
# Surface water nitrate = DP1.20254.001
# Water Quality = DP1.20288.001
# Water temperature = DP1.20264.001
# Nitrate = DP1.20033.001

# Air temperature	DP1.20046.001
# Wind speed	DP1.20059.001
# Nitrate	DP1.20033.001
# Zooplankton	DP1.20219.001
# Secchi depth	DP1.20252.001
# Precipitation	DP1.00006.001
# Air temperature	DP1.00002.001
# Wind speed	DP1.00001.001
# Water quality	DP1.20288.001
# Chemical properties of surface water	DP1.20093.001


nidx <- neonstore::neon_index(site = site)
unique(nidx$table)

# Download met products
neonstore::neon_download(product = "DP1.20046.001", site = site)

# Air Temperature
neonstore::neon_store(table = "RH_30min-expanded")

humidity_lakes <- neonstore::neon_table(table = "RH_30min-expanded", site = lake_sites)

# Water quality from Sonde at surface (Conducatnce, DO, pSAT, pH, chla, turb, FDOM)
wq <- neonstore::neon_table(table = c("waq_instantaneous-basic"),
                            site = site)

water_quality <- neonstore::neon_read(
  table = c("waq_instantaneous-basic"),
  product = "DP1.20288.001",
  site = site,
  start_date = "2018-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  # dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE)
head(water_quality)
