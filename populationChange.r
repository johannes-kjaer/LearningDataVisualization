# Mapping Population Change
# Johannes Kjær, 21.11.23
# Mostly following a tutorial by Milos Popovic, but deviating
# where it doesn't work as expected, and where I want to:
# https://www.youtube.com/watch?v=vQ_M-wv73aY&t=208s

country_code <- "NO"

# Defining the libraries we need
libs <- c("tidyverse", "terra", "giscoR", "here", "tidyterra")

# Checking which libraries are not already installed and installing them
new_packages <- libs[!(libs %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
    install.packages(new_packages)
}


# Load the libraries we use
invisible(lapply(
    libs, library,
    character.only = TRUE,
))

# - - - - - - - - - - - - - - #
# Checking if data is downloaded
# Loading the data - if it is downloaded, else nothing is loaded
file_names <- list.files(
    path = here("Data"),
    pattern = "\\.tif$",
    full.names = TRUE
)

# If not already downloaded, download and load the data
if (length(file_names) == 110) {
    # Downloading data from GHSL
    data_urls <- c(
        "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_S_GLOBE_R2023A/GHS_BUILT_S_E2020_GLOBE_R2023A_54009_100/V1-0/GHS_BUILT_S_E2020_GLOBE_R2023A_54009_100_V1_0.zip",
        "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_S_GLOBE_R2023A/GHS_BUILT_S_E1990_GLOBE_R2023A_54009_100/V1-0/GHS_BUILT_S_E1990_GLOBE_R2023A_54009_100_V1_0.zip"
    )

    options(timeout = 500) # Increasing time for timeout when downloading the data

    # downloading
    for (url in data_urls){
        download.file(
            url = url,
            path = here("Data"),
            destfile = basename(url)
    )
}

    # unzipping
    lapply(
        basename(data_urls),
        unzip
    )

    # Loading the data
    file_names <- list.files(
        path = here("Data"),
        pattern = "\\.tif$",
        full.names = TRUE
    )
}


# Loading the data as raster files
pop_rasters <- lapply(
    file_names,
    terra::rast
)

# - - - - - - - - - - - - - - #
# Country borders

get_country_border <- function(country_code) {
    country <- giscoR::gisco_get_countries(
        country = country_code,
        resolution = "1"
    )
    return(country)
}

country <- get_country_border(country_code)

# - - - - - - - - - - - - - - #
# Reprojecting crs data as the crs of the data and country border differs

crs_raster <- terra::crs(pop_rasters[[1]]) # Assuming rasters have the same CRS
crs_vector <- terra::crs(terra::vect(country))

if (crs_raster != crs_vector) {
    country <- terra::project(terra::vect(country), crs_raster)
}


# - - - - - - - - - - - - - - #
# Crop the raster files

country_pop_rasters <- lapply(
    pop_rasters,
    function(x) {
        terra::crop(
            x,
            country,
            snap = "in",
            mask = TRUE
        )
    }
)

# - - - - - - - - - - - - - - #
# Lambert projection to get a more accurate shape
crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=432100 +y_0=321000 +datum=WGS84 +units=m +no_defs"

# - - - - - - - - - - - - - - #
# Calculating the population change
population_change <- (
    country_pop_rasters[[2]] - country_pop_rasters[[1]]
) |>
    terra::project(crs_lambert)

# - - - - - - - - - - - - - - #
# Classify categories

get_categories <- function(x) {
    terra::ifel(
        population_change == 0, 0,
        terra::ifel(
            population_change > 0, 1,
            terra::ifel(
                population_change < 0, -1, population_change
            )
        )
    )
}

population_change_categories <- get_categories(population_change) |>
    as.factor()

# - - - - - - - - - - - - - - #
# Colour map
colours <- c(
    "#eb389f",
    "grey80",
    "#018f1d"
)

# - - - - - - - - - - - - - - #
# Defining the plot

p <- ggplot() +
    tidyterra::geom_spatraster(
        data = population_change_categories
    ) +
    geom_sf(
        data = country,
        fill = "transparent",
        color = "grey40",
        size = 0.5
    ) +
    scale_fill_manual(
        name = "growth or decline?",
        values = colours,
        labels = c(
            "Decline",
            "No change",
            "Increase"
        ),
        na.translate = FALSE
    ) +
    guides(
        fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(5, "mm"),
            keywidth = unit(40, "mm"),
            label.position = "bottom",
            label.hjust = .5,
            nrow = 1,
            byrow = TRUE,
            drop = TRUE
        )
    ) +
    coord_sf(crs = crs_lambert) +
    theme_void() +
    theme(
        legend.position = c(.5, .96),
        legend.title = element_text(
            size = 30, color = "grey20",
        ),
        legend.text = element_text(
            size = 25, color = "grey20",
        ),
        plot.caption = element_text(
            size = 20, color = "grey40",
            hjust = .5, vjust = 10
        ),
        plot.margin = unit(
            c(
                t = .5, b = -3,
                l = -3, r = -3
            ), "lines"
        )
    ) +
    labs(
        caption = "Global Human Settlement Layer at 100m"
    )


# - - - - - - - - - - - - - - #
# Getting height and width of the matrix of the pop change
w <- ncol(population_change_categories)
h <- nrow(population_change_categories)
coefficient <- 1

ggsave(
    country_code + "_pop_change.png",
    p,
    width = w * coefficient, height = h * coefficient,
    units = "px",
    bg = "white",
    dpi = 300,
    limitsize = FALSE
)