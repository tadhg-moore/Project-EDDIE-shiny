# download.file("https://github.com/cwida/duckdb/releases/download/master-builds/duckdb_r_src.tar.gz", destfile = "~/duckdb_r_src.tar.gz")
# install.packages("duckdb_r_src.tar.gz", repo = NULL)
# /groups/rqthomas_lab/neonstore
remotes::install_github("cwida/duckdb/tools/rpkg", build = FALSE)
remotes::install_github("cboettig/neonstore")
# Sys.setenv("NEONSTORE_HOME" = "/groups/rqthomas_lab/neonstore3")

# Sys.setenv("NEONSTORE_HOME" = "/groups/rqthomas_lab/neonstore2")
# success <- lapply(neonstore::neon_index()$path, function(x)Sys.chmod(x, 677))
# prod <- neonstore::neon_index()$product[unlist(success)]
# unique(prod)
# dir.create("neonstore")
Sys.setenv("NEONSTORE_HOME" = "/groups/rqthomas_lab/neonstore4")
Sys.setenv("NEONSTORE_DB" = "/home/tadhgm/Project-EDDIE-shiny/neonstore/")
success <- lapply(neonstore::neon_index()$path, Sys.chmod, "644")
# neonstore::neon_dir()
neonstore::neon_store(table = "SAAT_30min-expanded")
# neonstore::neon_store()
# neonstore::neon_store(table="waq_instantaneous")

site <- c("BARC")

# DP1.20093.001 - Chemical properties of surface water
# DP1.20288.001 - Water quality
# DP1.00002.001 - Air temperature
# DP1.20219.001 - Zooplankton
# DP1.20033.001 - Nitrate in surface water
# DP1.20264.001 - Temperature at specific depths
# DP1.20048.001 - Stream discharge field collection


product <-'DP1.20264.001' # "DP1.20033.001"

neonstore::neon_download(product = product, site = site)

nidx <- neonstore::neon_index(site = site)
unique(nidx$table)
# unique(nidx$type)

air <- neonstore::neon_read(table = "SAAT_30min-expanded", site = site)
waq <- neonstore::neon_read(table = "waq_instantaneous", site = site)
nit <- neonstore::neon_read(site = site, table = "NSW_15_minute-expanded")
# zoo <- neonstore::neon_read(site = site, table = "zoo_taxonomyProcessed-basic")

df <- air[, c("endDateTime", "tempSingleMean")]
df2 <- waq[, c("endDateTime", "chlorophyll")]
df3 <- nit[, c("endDateTime", "surfWaterNitrateMean")]

df$year <- lubridate::year(df$endDateTime)
df2$year <- lubridate::year(df2$endDateTime)
df3$year <- lubridate::year(df3$endDateTime)

sub <- df[df$year == 2019, -ncol(df)]
sub2 <- df2[df2$year == 2019, -ncol(df)]
sub3 <- df3[df3$year== 2019, -ncol(df)]

write.csv(sub, "Project-EDDIE-shiny/module5/data/BARC_airtemp_2019.csv", row.names = F, quote = F)
write.csv(sub2, "Project-EDDIE-shiny/module5/data/BARC_chla_2019.csv", row.names = F, quote = F)
write.csv(sub3, "Project-EDDIE-shiny/module5/data/BARC_surfnitrate_2019.csv", row.names = F, quote = F)


# site <- c("BARC")



print("Downloading: DP1.20288.001")

# DP1.20093.001 - Chemical properties of surface water
# DP1.20288.001 - Water quality
# DP1.00002.001 - Air temperature
# DP1.20219.001 - Zooplankton
# DP1.20033.001 - Nitrate in surface water


product <-'DP1.00002.001' #"DP1.20033.001"

neonstore::neon_download(product = product, site = site)

success <- lapply(neonstore::neon_index()$path, Sys.chmod, "644")
sum(unlist(success))
prod <- neonstore::neon_index()$product[unlist(success)]
unique(prod)
length(success)

df <- neonstore::neon_read(table="waq_instantaneous")
df2 <- neonstore::neon_read(table="waq_instantaneous")


neonstore::neon_store()

nidx <- neonstore::neon_index(site = site)
unique(nidx$table)

neonstore::neon_read(table = "NSW_15_minute-expanded")


df <- neonstore::neon_table(site = site, table = "waq_instantaneous-basic")
vars <- neonstore::neon_read(site = site, table = "variables", )
units <- vars$units[vars$fieldName == "chlorophyll"]
units

# -----------------------------------------------------------------------------------------------------------------
zoo <- neonstore::neon_table(site = site, table = "zoo_taxonomyProcessed-basic")
nit <- neonstore::neon_table(site = site, table = "NSW_15_minute-expanded")
airt <- 

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
