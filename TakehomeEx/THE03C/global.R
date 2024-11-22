pacman::p_load(
  shiny, shinydashboard, shinyWidgets, shinyBS, sf, tidyverse, 
  viridis, spatstat, spdep, DT, plotly, tmap, tmaptools,
  leaflet, ggplot2, dplyr
)

# Data preprocessing function
preprocess_data <- function() {
  # Load raw data
  indo_schools <- read_csv("data/aspatial/complete_data.csv")
  adm2 <- st_read("data/geospatial", layer = "geoBoundaries-IDN-ADM2_simplified") %>%
    st_make_valid()
  batas <- st_read("data/geospatial", 
                   layer = "BATAS_DESA_DESEMBER_2019_DUKCAPIL_DKI_JAKARTA") %>%
    st_make_valid()
  
  # Clean and filter Jakarta schools
  jakarta_schools <- indo_schools %>% 
    filter(
      province_name == "DKI JAKARTA",
      city_name != "Kab. Kepulauan Seribu",
      !is.na(long) & !is.na(lat)
    ) %>%
    st_as_sf(
      coords = c("long", "lat"), 
      crs = 4326
    ) %>%
    st_make_valid() %>%
    mutate(
      school_level = case_when(
        stage %in% c("SD", "SDLB") ~ "Elementary",
        stage %in% c("SMP", "SMPLB") ~ "Junior High",
        stage %in% c("SMA", "SMK", "SMLB") ~ "Senior High",
        TRUE ~ "Special Education"
      ),
      school_type = if_else(status == "N", "Public", "Private")
    )
  
  # Filter and clean boundary data
  adm2_jakarta <- adm2 %>%
    filter(shapeName %in% c(
      "Kota Jakarta Barat", "Kota Jakarta Pusat",
      "Kota Jakarta Selatan", "Kota Jakarta Timur",
      "Kota Jakarta Utara"
    ))
  
  # Clean and standardize administrative boundaries
  batas_clean <- batas %>%
    select(
      OBJECT_ID, PROVINSI, KAB_KOTA, KECAMATAN, 
      DESA_KELUR, JUMLAH_PEN, JUMLAH_KK, 
      LUAS_WILAY, KEPADATAN, geometry
    ) %>%
    rename(
      province = PROVINSI,
      city = KAB_KOTA,
      district = KECAMATAN,
      subdistrict = DESA_KELUR,
      population = JUMLAH_PEN,
      total_families = JUMLAH_KK,
      area_size = LUAS_WILAY,
      popn_density = KEPADATAN
    ) %>%
    filter(!is.na(city) & city != "KEPULAUAN SERIBU") %>%
    mutate(
      city = case_when(
        city == "JAKARTA BARAT" ~ "Kota Jakarta Barat",
        city == "JAKARTA PUSAT" ~ "Kota Jakarta Pusat",
        city == "JAKARTA SELATAN" ~ "Kota Jakarta Selatan",
        city == "JAKARTA TIMUR" ~ "Kota Jakarta Timur",
        city == "JAKARTA UTARA" ~ "Kota Jakarta Utara",
        TRUE ~ city
      )
    )
  
  # Transform to UTM projection for analysis
  jakarta_schools_utm <- st_transform(jakarta_schools, 32748)
  adm2_jakarta_utm <- st_transform(adm2_jakarta, 32748)
  batas_clean_utm <- st_transform(batas_clean, 32748)
  
  jakarta_schools_utm <- st_join(
    jakarta_schools_utm,
    st_union(batas_clean_utm) %>% st_sf(),
    join = st_intersects,
    left = FALSE  # Only keep points that intersect
  )
  
  # Return preprocessed datasets
  return(list(
    schools = jakarta_schools_utm,
    boundaries = adm2_jakarta_utm,
    admin = batas_clean_utm
  ))
}

# Spatial analysis functions
# Updated calculate_kde function
calculate_kde <- function(points, boundary, bandwidth = NULL) {
  # Create window from boundary
  window <- as.owin(st_union(boundary))
  
  # Get coordinates 
  coords <- st_coordinates(points)
  
  # Remove duplicates first
  coords_df <- as.data.frame(coords)
  coords_unique <- coords_df[!duplicated(coords_df), ]
  
  # Filter points within window first
  inside <- inside.owin(
    x = coords_unique[,1],
    y = coords_unique[,2],
    w = window
  )
  valid_coords <- coords_unique[inside,]
  
  # Only proceed if we have enough points
  if(nrow(valid_coords) < 4) {
    return(NULL)
  }
  
  # Create point pattern
  ppp_points <- ppp(
    x = valid_coords[,1],
    y = valid_coords[,2],
    window = window
  )
  
  # Calculate bandwidth if not provided
  if (is.null(bandwidth)) {
    bandwidth <- bw.scott(ppp_points)
  } else {
    # Convert slider value (0.1-2) to actual bandwidth
    max_dim <- max(diff(window$xrange), diff(window$yrange))
    bandwidth <- bandwidth * (max_dim/20)
  }
  
  kde <- density.ppp(
    ppp_points,
    sigma = bandwidth,
    edge = TRUE,
    at = "pixels",
    dimyx = c(128, 128)
  )
  
  return(list(
    kde = kde,
    points = ppp_points,
    bandwidth = bandwidth
  ))
}

# Load GWR and LISA specific data
district_metrics <- readRDS("data/rds/district_metrics.rds")
elementary_results <- readRDS("data/rds/elementary_gwr_results.rds")
junior_results <- readRDS("data/rds/junior_gwr_results.rds")
senior_results <- readRDS("data/rds/senior_gwr_results.rds")

# Updated calculate_nn_stats function
calculate_nn_stats <- function(points, boundary) {
  window <- as.owin(st_union(boundary))
  coords <- st_coordinates(points)
  
  # Remove duplicates
  coords_df <- as.data.frame(coords)
  coords_unique <- coords_df[!duplicated(coords_df), ]
  
  # Filter points within window
  inside <- inside.owin(
    x = coords_unique[,1],
    y = coords_unique[,2],
    w = window
  )
  valid_coords <- coords_unique[inside,]
  
  if(nrow(valid_coords) < 4) {
    return(NULL)
  }
  
  ppp_points <- ppp(
    x = valid_coords[,1],
    y = valid_coords[,2],
    window = window
  )
  
  observed_mean_dist <- mean(nndist(ppp_points))
  n_points <- npoints(ppp_points)
  area <- area.owin(window)
  expected_mean_dist <- 1/(2*sqrt(n_points/area))
  r_stat <- observed_mean_dist/expected_mean_dist
  
  return(list(
    observed = observed_mean_dist,
    expected = expected_mean_dist,
    r_statistic = r_stat,
    n_points = n_points,
    area = area
  ))
}





