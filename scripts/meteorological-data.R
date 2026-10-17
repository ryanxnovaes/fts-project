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

# Inspect the dataset structure
dplyr::glimpse(data)

# Create the date variable and sort observations chronologically
data <- data |>
  dplyr::mutate(date = as.Date(sprintf("%04d-%02d-%02d", YEAR, MO, DY))) |>
  dplyr::arrange(date, HR)

# Inspect the processed dataset
dplyr::glimpse(data)

# Count missing observations by variable
sapply(data[7:12], function(x) sum(is.na(x)))

# Count missing observations by year and variable
sapply(data[7:12], function(x) {tapply(is.na(x), data$YEAR, sum)})

# Compute overall descriptive statistics
psych::describe(data[, c(7:11)], quant = c(0.25, 0.75)) |>
  (\(desc) {
    desc$cv <- desc$sd / desc$mean
    desc[, c(names(desc)[1:4], names(desc)[8], names(desc)[14], 
             names(desc)[5], names(desc)[15], names(desc)[9],
             names(desc)[11:12], names(desc)[16])]
  })() |>
  print(digits = 5)

# Export descriptive statistics to LaTeX
# knitr::kable(format = "latex", digits = 5, booktabs = TRUE)



# Compute monthly descriptive statistics by calendar month, pooling years
invisible(lapply(7:11, \(i) {
  
  cat("\n\n================", names(data)[i], "================\n\n")
  
  d_final <- do.call(rbind, lapply(split(data, data$MO), \(x) {
    
    d <- psych::describe(x[, i], quant = c(.25, .75))
    d$cv <- d$sd / d$mean
    
    d[, c(names(d)[c(3:4, 8, 14, 5, 15, 9, 11:12, 16)])]
    
  }))
  
  rownames(d_final) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  print(as.data.frame(d_final), digits = 5)
  # Export monthly descriptive statistics to LaTeX
  # cat(knitr::kable(as.data.frame(d_final), format = "latex",
  #     digits = 5, booktabs = TRUE))
  
}))


# Compute chronological monthly descriptive statistics
invisible(lapply(7:11, \(i) {
  
  cat("\n\n================", names(data)[i], "================\n\n")
  
  meses <- split(data, format(data$date, "%Y-%m"))
  
  d_final <- do.call(rbind, lapply(meses, \(x) {
    
    d <- psych::describe(x[, i], quant = c(.25, .75))
    d$cv <- d$sd / d$mean
    
    d[, c(names(d)[c(3:4, 8, 14, 5, 15, 9, 11:12, 16)])]
    
  }))
  
  rownames(d_final) <- names(meses)
  
  print(as.data.frame(d_final), digits = 5)
  
  # Export chronological monthly statistics to LaTeX
  # cat(knitr::kable(as.data.frame(d_final), format = "latex",
  #     digits = 5, booktabs = TRUE))
  
}))


# Plot the hourly temperature time series
ggplot2::ggplot(data, ggplot2::aes(x = date, y = T2M)) +
  ggplot2::geom_line(color = "#2166AC", linewidth = 0.4) +
  ggplot2::labs(title = "Hourly Air Temperature Time Series",
                x = "Date", y = "Air temperature (°C)") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = ggplot2::element_text(face = "bold"),
    axis.text = ggplot2::element_text(color = "black"),
    axis.line = ggplot2::element_line(linewidth = 0.4),
    axis.ticks = ggplot2::element_line(linewidth = 0.4),
    plot.margin = ggplot2::margin(10, 12, 10, 12)
  )


# Plot the temporal evolution of meteorological variables
data |>
  dplyr::mutate(
    Month = factor(MO, levels = 1:12,
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = Month, y = T2M)) +
  ggplot2::geom_boxplot(
    fill = "#D9E6F2", color = "#1F4E79",
    linewidth = 0.5, width = 0.65,
    outlier.shape = 21, outlier.size = 1.2,
    outlier.fill = "white", outlier.color = "#1F4E79",
    na.rm = TRUE
  ) +
  ggplot2::labs(
    title = "Monthly Air Temperature Distribution",
    x = "Month", y = "Air Temperature (°C)"
  ) +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold", hjust = 0.5, size = 14
    ),
    axis.title = ggplot2::element_text(face = "bold"),
    axis.text = ggplot2::element_text(color = "black", size = 10),
    axis.line = ggplot2::element_line(linewidth = 0.4),
    axis.ticks = ggplot2::element_line(linewidth = 0.4),
    plot.margin = ggplot2::margin(10, 12, 10, 12)
  )


# Visualize hourly temperature patterns using a heatmap
# Blue theme
ggplot2::ggplot(data, ggplot2::aes(x = HR, y = date, fill = T2M)) +
  ggplot2::geom_raster() +
  ggplot2::scale_x_continuous(breaks = seq(0, 23, 3), expand = c(0, 0)) +
  ggplot2::scale_y_date(date_breaks = "1 month", date_labels = "%Y-%m",
                        expand = c(0, 0)) +
  ggplot2::labs(title = "Hourly Temperature Heatmap", x = "Hour",
                y = "Date", fill = "Temperature (°C)") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = ggplot2::element_text(face = "bold"),
    axis.text = ggplot2::element_text(color = "black"),
    legend.title = ggplot2::element_text(face = "bold"),
    axis.line = ggplot2::element_line(linewidth = 0.4),
    plot.margin = ggplot2::margin(10, 12, 10, 12)
  )

# Blue to red theme
ggplot2::ggplot(data, ggplot2::aes(x = HR, y = date, fill = T2M)) +
  ggplot2::geom_raster() +
  ggplot2::scale_fill_distiller(palette = "RdYlBu", direction = -1,
                                name = "Temperature (°C)") +
  ggplot2::scale_x_continuous(breaks = seq(0, 23, 3), expand = c(0, 0)) +
  ggplot2::scale_y_date(date_breaks = "1 month", date_labels = "%m/%Y",
                        expand = c(0, 0)) +
  ggplot2::labs(title = "Hourly Air Temperature Heatmap",
                x = "Hour", y = "Date") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = ggplot2::element_text(face = "bold"),
    axis.text = ggplot2::element_text(color = "black"),
    legend.title = ggplot2::element_text(face = "bold"),
    axis.line = ggplot2::element_line(linewidth = 0.4),
    plot.margin = ggplot2::margin(10, 12, 10, 12)
  )


# Plot daily mean temperature with minimum and maximum values
data |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    media = mean(T2M, na.rm = TRUE),
    minima = min(T2M, na.rm = TRUE),
    maxima = max(T2M, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = date)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = minima, ymax = maxima),
                       fill = "gray70", alpha = 0.4) +
  ggplot2::geom_line(ggplot2::aes(y = media),
                     color = "#1F4E79", linewidth = 0.6) +
  ggplot2::labs(
    title = "Daily Temperature Evolution",
    subtitle = "Line: daily mean; shaded band: daily minimum and maximum",
    x = "Date", y = "Air Temperature (°C)"
  ) +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10, color = "gray35"),
    axis.title = ggplot2::element_text(face = "bold"),
    axis.text = ggplot2::element_text(color = "black"),
    axis.line = ggplot2::element_line(linewidth = 0.4),
    plot.margin = ggplot2::margin(10, 12, 10, 12)
  )


# Plot monthly mean values of meteorological variables
data |>
  dplyr::group_by(MO) |>
  dplyr::summarise(
    dplyr::across(c(RH2M, PRECTOTCORR, WS10M, ALLSKY_SFC_SW_DWN),
                  \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(cols = -MO, names_to = "Variable",
                      values_to = "Mean") |>
  dplyr::mutate(
    MO = factor(MO, levels = 1:12,
                labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
    Variable = factor(Variable,
                      levels = c("RH2M", "PRECTOTCORR", "WS10M", "ALLSKY_SFC_SW_DWN"),
                      labels = c("Relative Humidity (%)", "Precipitation (mm)",
                                 "Wind Speed (m/s)", "Global Solar Radiation (W/m²)"))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = MO, y = Mean, group = 1)) +
  ggplot2::geom_line(color = "#1F4E79", linewidth = 0.6) +
  ggplot2::geom_point(color = "#1F4E79", fill = "white",
                      shape = 21, size = 2.2, stroke = 0.6) +
  ggplot2::facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  ggplot2::labs(title = "Monthly Mean Meteorological Variables",
                x = "Month", y = NULL) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0.05, 0.08))) +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold", hjust = 0.5, size = 14),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold", size = 11),
    axis.title.x = ggplot2::element_text(face = "bold"),
    axis.text = ggplot2::element_text(color = "black", size = 9),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    axis.line = ggplot2::element_line(linewidth = 0.4),
    axis.ticks = ggplot2::element_line(linewidth = 0.4),
    panel.spacing = grid::unit(1.2, "lines"),
    plot.margin = ggplot2::margin(10, 12, 10, 12)
  )


# Compare monthly distributions of meteorological variables using boxplots
data |>
  tidyr::pivot_longer(cols = c(RH2M, PRECTOTCORR, WS10M, ALLSKY_SFC_SW_DWN),
    names_to = "Variable", values_to = "Value") |>
  dplyr::mutate(MO = factor(MO, levels = 1:12,
                labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
    Variable = factor(Variable, 
                      levels = c("RH2M", "PRECTOTCORR", "WS10M", "ALLSKY_SFC_SW_DWN"),
                      labels = c("Relative Humidity (%)", "Precipitation (mm)",
                                 "Wind Speed", "Global Solar Radiation"))) |>
  ggplot2::ggplot(ggplot2::aes(x = MO, y = Value)) +
  ggplot2::geom_boxplot(fill = "#D9E6F2", color = "#1F4E79", linewidth = 0.5, width = 0.65,
    outlier.shape = 21, outlier.size = 1.5, outlier.stroke = 0.4, outlier.color = "#C00000",
    outlier.fill = "white", outlier.alpha = 0.6) +
  ggplot2::facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  ggplot2::labs(title = "Monthly Distribution of Meteorological Variables",
    x = "Month", y = NULL) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.08))) +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold", size = 11),
    axis.title.x = ggplot2::element_text(face = "bold"),
    axis.text = ggplot2::element_text(color = "black", size = 9),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    axis.line = ggplot2::element_line(linewidth = 0.4),
    axis.ticks = ggplot2::element_line(linewidth = 0.4),
    panel.spacing = grid::unit(1.2, "lines"),
    plot.margin = ggplot2::margin(10, 12, 10, 12)
  )


# Compare temperature distributions across calendar months
data |>
  dplyr::mutate(
    Mes = factor(MO, levels = 1:12,
                 labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = Mes, y = T2M)) +
  ggplot2::geom_boxplot(fill = "#D9E6F2", color = "#1F4E79",
                        linewidth = 0.5, outlier.size = 1,
                        na.rm = TRUE) +
  ggplot2::labs(x = "Month", y = "Temperature (°C)",
                title = "Monthly temperature distributions") +
  ggplot2::theme_classic(base_size = 10)


# Examine pairwise relationships between meteorological variables
# Linear
GGally::ggpairs(
  data |> dplyr::select(T2M, RH2M, PRECTOTCORR, WS10M, ALLSKY_SFC_SW_DWN),
  upper = list(continuous = GGally::wrap(
    "cor", method = "spearman", size = 4, digits = 5,
    color = "#1F4E79"
  )),
  lower = list(continuous = GGally::wrap(
    "smooth", alpha = 0.3, size = 0.5,
    color = "#1F4E79"
  )),
  diag = list(continuous = GGally::wrap(
    "densityDiag", alpha = 0.4,
    fill = "#D9E6F2", color = "#1F4E79"
  ))
) +
  ggplot2::labs(title = "Relationships Among Meteorological Variables") +
  ggplot2::theme_classic(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5, face = "bold", size = 14
    ),
    axis.text = ggplot2::element_text(color = "black", size = 8),
    axis.title = ggplot2::element_text(face = "bold", size = 9),
    axis.line = ggplot2::element_line(
      color = "gray60", linewidth = 0.3
    ),
    plot.margin = ggplot2::margin(10, 12, 10, 12)
  )

# Linear and bspline
GGally::ggpairs(
  data |> dplyr::select(T2M, RH2M, PRECTOTCORR, WS10M, ALLSKY_SFC_SW_DWN),
  upper = list(continuous = GGally::wrap(
    "cor", method = "spearman", size = 4, digits = 5,
    color = "#1F4E79"
  )),
  lower = list(continuous = function(data, mapping, ...) {
    ggplot2::ggplot(data, mapping) +
      ggplot2::geom_point(color = "#1F4E79", alpha = 0.3, size = 0.7) +
      ggplot2::geom_smooth(
        method = "lm", formula = y ~ x,
        color = "black", linewidth = 0.5, se = FALSE
      ) +
      ggplot2::geom_smooth(
        method = "lm", formula = y ~ splines::bs(x, df = 4),
        color = "#C00000", linewidth = 0.7, se = FALSE
      )
  }),
  diag = list(continuous = GGally::wrap(
    "densityDiag", alpha = 0.4,
    fill = "#D9E6F2", color = "#1F4E79"
  ))
) +
  ggplot2::labs(title = "Relationships Among Meteorological Variables") +
  ggplot2::theme_classic(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = ggplot2::element_text(color = "black", size = 8),
    axis.title = ggplot2::element_text(face = "bold", size = 9),
    axis.line = ggplot2::element_line(color = "gray60", linewidth = 0.3),
    plot.margin = ggplot2::margin(10, 12, 10, 12)
  )
