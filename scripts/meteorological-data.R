# Download hourly meteorological data from NASA POWER
data <- nasapower::get_power(
  community = "AG",
  pars = c("T2M", "ALLSKY_SFC_SW_DWN", "RH2M", "PRECTOTCORR", "WS10M"),
  temporal_api = "hourly",
  lonlat = c(-47.623468, -22.703114),
  dates = c("2025-01-01", "2026-05-30")
)

# Create the output directory and save the original dataset in Parquet format
dir.create("data", showWarnings = FALSE)
arrow::write_parquet(data, "data/original.parquet")

