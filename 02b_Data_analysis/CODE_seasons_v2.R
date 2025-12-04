###############################################
# Global peak timing maps for lakes
# - Compute per-lake DOY of annual maxima for chla/temp/level
# - Quantify uncertainty via circular SD across years
# - Map results in Robinson projection with a cyclic color scale
# - Place numeric size legend (SD in days) and circular DOY legend on right
###############################################

## 0) Packages ----
library(data.table)
library(lubridate)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(patchwork)     # layout and wrap_elements()
library(cowplot)       # get_legend()

## 1) Inputs (edit paths as needed) ----
path_dt   <- "E:/CompoundEvents/DATA_dailymean_chlatemplevel_v2.csv"
path_pts  <- "E:/HydroLAKES_points_v10_shp/HydroLAKES_points_v10_shp/HydroLAKES_points_v10.dbf"
# (You can also point to the .shp of HydroLAKES_points_v10; the .dbf works because it shares the basename.)

## 2) Load data ----
dt <- fread(path_dt)    # expects columns: Hylak_id, date, lat, lon, temp, chla, level
# Quick peek (optional)
# head(dt); summary(dt)

# HydroLAKES points (one feature per lake with pour-point attributes)
points <- st_read(path_pts, quiet = TRUE)

## 3) Build a pour-point (PP) table and harmonize per-lake coordinates ----
# Extract Hylak_id and pour-point lon/lat; if missing, fall back to geometry
pp <- as.data.table(st_drop_geometry(points))[
  , .(Hylak_id, pp_lon = Pour_long, pp_lat = Pour_lat)
]
coords <- st_coordinates(points)
pp[, `:=`(geom_lon = coords[, 1], geom_lat = coords[, 2])]
pp[, `:=`(
  pp_lon = fifelse(is.na(pp_lon), geom_lon, pp_lon),
  pp_lat = fifelse(is.na(pp_lat), geom_lat, pp_lat)
)]
pp[, c("geom_lon", "geom_lat") := NULL]
setkey(pp, Hylak_id)

# Compute per-lake median lat/lon from dt (robust to small jitter)
# If a lake has all-NA for lat or lon in dt, keep NA here.
lake_from_dt <- dt[
  , .(
    lat_dt = if (all(is.na(lat))) NA_real_ else median(lat, na.rm = TRUE),
    lon_dt = if (all(is.na(lon))) NA_real_ else median(lon, na.rm = TRUE)
  ),
  by = Hylak_id
]
setkey(lake_from_dt, Hylak_id)

# Prefer dt’s coordinates when available; otherwise use pour-point coords
lake_coords <- pp[lake_from_dt, on = "Hylak_id"][
  , .(
    Hylak_id,
    lat_final = fifelse(!is.na(lat_dt), lat_dt, pp_lat),
    lon_final = fifelse(!is.na(lon_dt), lon_dt, pp_lon)
  )
]
setkey(lake_coords, Hylak_id)

# Fill back into dt so every row for a lake has a single consistent (lat, lon)
setkey(dt, Hylak_id)
dt[lake_coords, `:=`(lat = i.lat_final, lon = i.lon_final)]

# Diagnostics (optional)
cat("Remaining NA lakes (lat): ", dt[is.na(lat), uniqueN(Hylak_id)], "\n")
cat("Remaining NA lakes (lon): ", dt[is.na(lon), uniqueN(Hylak_id)], "\n")
# missing_ids <- dt[is.na(lat) | is.na(lon), unique(Hylak_id)]  # if needed

## 4) Add day-of-year (DOY), year, and days-in-year (leap-aware) ----
# These support per-year peak detection and circular stats
dt[, `:=`(
  doy = yday(date),
  year = year(date),
  diy  = ifelse(leap_year(date), 366L, 365L)
)]

## 5) Circular statistics helper ----
# Given a vector of DOYs and corresponding days-in-year (365/366),
# compute: circular mean DOY (mapped to 365-day scale),
# circular SD (in days; dispersion of timing), and number of years.
circ_summary_days <- function(doy, diy) {
  ok <- !is.na(doy) & !is.na(diy)
  if (sum(ok) < 1L) {
    return(list(doy_circ = NA_real_, sd_circ_days = NA_real_, n_years = 0L))
  }
  doy <- as.numeric(doy[ok]); diy <- as.numeric(diy[ok])
  
  # Map DOY -> angle on [0, 2π)
  theta <- 2 * pi * (doy - 1) / diy
  
  C <- mean(cos(theta))
  S <- mean(sin(theta))
  Rbar <- sqrt(C^2 + S^2)
  
  # Circular mean angle -> map back to a canonical 365-day DOY
  mu <- atan2(S, C); if (mu < 0) mu <- mu + 2 * pi
  doy_circ <- 1 + 365 * mu / (2 * pi)
  
  # Circular SD in radians, then convert to days on 365-day circle
  sd_rad <- sqrt(pmax(0, -2 * log(pmax(Rbar, .Machine$double.eps))))
  sd_circ_days <- sd_rad * 365 / (2 * pi)
  
  list(doy_circ = as.numeric(doy_circ),
       sd_circ_days = as.numeric(sd_circ_days),
       n_years = length(theta))
}

## 6) Extract yearly peak DOY per variable, then summarize circularly per lake ----
# Ties (multiple equal maxima in a year) are resolved by taking the first (earliest).
peak_doy_circular <- function(DT, var) {
  yr_peaks <- DT[!is.na(get(var)),
                 .SD[which.max(get(var)), .(doy, diy)],
                 by = .(Hylak_id, year)]
  res <- yr_peaks[, as.list(circ_summary_days(doy, diy)), by = Hylak_id]
  setnames(res,
           c("doy_circ", "sd_circ_days", "n_years"),
           c(paste0("doy_peak_", var),
             paste0("sd_days_", var),
             paste0("n_years_", var)))
  res
}

res_chla  <- peak_doy_circular(dt, "chla")
res_temp  <- peak_doy_circular(dt, "temp")
res_level <- peak_doy_circular(dt, "level")

# Merge and attach one set of coords per lake
coords_once <- unique(dt[, .(Hylak_id, lat, lon)])
res <- Reduce(function(x, y) merge(x, y, by = "Hylak_id", all = TRUE),
              list(res_chla, res_temp, res_level))
res <- coords_once[res, on = "Hylak_id"]

# Quick sanity check (optional)
# res[, .(lakes=.N,
#         chla_years_med  = median(n_years_chla,  na.rm=TRUE),
#         temp_years_med  = median(n_years_temp,  na.rm=TRUE),
#         level_years_med = median(n_years_level, na.rm=TRUE))]

# Keep lakes with complete DOY estimates for all three variables
res_complete <- res[!is.na(doy_peak_chla) &
                      !is.na(doy_peak_temp) &
                      !is.na(doy_peak_level)]

## 7) Prepare mapping table (long format: one row per lake × variable) ----
long <- rbindlist(list(
  res_complete[, .(Hylak_id, lat, lon, var = "Chl-a",
                   doy = as.numeric(doy_peak_chla),
                   sd  = as.numeric(sd_days_chla))],
  res_complete[, .(Hylak_id, lat, lon, var = "Level",
                   doy = as.numeric(doy_peak_level),
                   sd  = as.numeric(sd_days_level))],
  res_complete[, .(Hylak_id, lat, lon, var = "Temperature",
                   doy = as.numeric(doy_peak_temp),
                   sd  = as.numeric(sd_days_temp))]
), use.names = TRUE)

# Drop rows with missing essentials (paranoia)
long <- long[!is.na(lat) & !is.na(lon) & !is.na(doy) & !is.na(sd)]

## 8) Convert points to sf, get land polygons, set Robinson CRS ----
pts_wgs   <- st_as_sf(long, coords = c("lon", "lat"), crs = 4326)
land      <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
crs_robin <- "+proj=robin +datum=WGS84 +no_defs"

## 9) Define a truly cyclic color palette for DOY (1 ≡ 366) ----
# Use base grDevices::hcl to generate a hue wheel; consistent lightness/chroma
make_cyclic_pal <- function(n = 366, c = 60, l = 70) {
  h <- seq(0, 360, length.out = n + 1)[1:n]  # wrap hue; drop duplicate endpoint
  grDevices::hcl(h = h, c = c, l = l)
}
pal_cyclic <- make_cyclic_pal(366, c = 60, l = 70)
doy_vals   <- 1:366

## 10) Build the three maps (faceted vertically), suppress internal legends ----
p_map <- ggplot() +
  geom_sf(data = land, fill = "grey90", color = NA) +
  geom_sf(data = pts_wgs, aes(color = doy, size = sd), alpha = 0.9, show.legend = FALSE) +
  coord_sf(crs = crs_robin) +
  scale_color_gradientn(
    colors = pal_cyclic,
    values = rescale(doy_vals, to = c(0, 1), from = c(1, 366)),
    limits = c(1, 366),
    guide  = "none"
  ) +
  # Map dot size directly to SD (days), reversed so larger SD => smaller dots (less certain)
  scale_size(range = c(0.2, 2.8), trans = "reverse", guide = "none") +
  facet_wrap(~ var, ncol = 1) +
  theme_void(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold")
  )

## 11) Build a numeric size legend for SD (days) ----
sd_range  <- range(long$sd, na.rm = TRUE)
sd_breaks <- pretty(sd_range, n = 4)

p_size_leg <- ggplot(long, aes(x = 1, y = 1, size = sd)) +
  geom_point() +
  scale_size(
    range  = c(0.9, 4.8),
    trans  = "reverse",           # larger SD shown as smaller points
    breaks = sd_breaks,
    name   = "SD of peak DOY (days)\n(larger = less certain)"
  ) +
  theme_void() +
  theme(legend.position = "right")

leg_size_panel <- patchwork::wrap_elements(cowplot::get_legend(p_size_leg))

## 12) Build a circular DOY legend (month labels around a color wheel) ----
first_day_doy <- c(Jan = 1, Feb = 32, Mar = 60, Apr = 91, May = 121, Jun = 152,
                   Jul = 182, Aug = 213, Sep = 244, Oct = 274, Nov = 305, Dec = 335)
month_ang <- 2 * pi * (first_day_doy - 1) / 365

legend_df <- data.table(
  ang = seq(0, 2 * pi, length.out = 366),
  r   = 1
)
legend_df[, doy := 1 + floor((ang / (2 * pi)) * 365)]
legend_df[doy == 366, doy := 365L]
legend_df[, col := pal_cyclic[doy]]

p_wheel <- ggplot(legend_df, aes(x = ang, y = r, fill = col)) +
  geom_tile(aes(width = (2 * pi) / 366, height = 0.18), color = NA) + # ring thickness
  scale_fill_identity() +
  coord_polar(theta = "x") +
  xlim(0, 2 * pi) +
  ylim(0.78, 1.22) +
  theme_void() +
  annotate("text", x = as.numeric(month_ang), y = 0.96,
           label = names(first_day_doy), size = 3) +
  labs(title = "DOY of max (cyclic)") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, margin = margin(b = 2)),
    plot.margin = margin(2, 2, 2, 2)
  )

p_wheel_panel <- p_wheel

## 13) Assemble final figure: maps (left) + legends (right) ----
right_col  <- leg_size_panel / p_wheel_panel + plot_layout(heights = c(1, 0.75))
final_plot <- p_map | right_col + plot_layout(widths = c(4.8, 1.2))

print(final_plot)

## 14) (Optional) Save to file ----
# ggsave("lake_peak_doy_global_robinson_cycliclegend_right.png",
#        final_plot, width = 12, height = 14, dpi = 300)


