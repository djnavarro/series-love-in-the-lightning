
library(Rcpp)
library(dplyr)
library(cairobasic)

sys_id <- "01"
sys_name <- "love"
sourceCpp(here::here("source", paste0(sys_name, "_", sys_id, ".cpp")))

lover <- function(seed) {

  cat(seed, "\n")
  set.seed(seed)

  # fixed / default
  px <- 5000
  layers <- sample(10:20, 1)
  million <- 10^6
  iter <- 100 * million
  zoom <- .5
  alpha <- .2

  # file name
  prefix <- paste0(sys_name, "_", sys_id, "_")
  fname <- paste0(prefix, seed, ".jpg")
  dir <- here::here("image", paste0("sys_", sys_id))
  if(!dir.exists(dir)) dir.create(dir)
  fpath <- file.path(dir, fname)



  # palette specification ---------------------------------------------------

  ncl <- 1024

  sample_colorir <- function(n_colours, ...) {
    palette_name <- sample(unique(colorir::colores$palette_name), 1)
    palette_base <- colorir::colores$colour[colorir::colores$palette_name == palette_name]
    palette_fn <- colorRampPalette(palette_base)
    return(palette_fn(n_colours, ...))
  }

  bg <- "black"
  pal <- sample_colorir(ncl)

  # generate the data -------------------------------------------------------

  cat("generating...\n")

  # create data frame
  df <- raster_data(iter, layers, px, zoom, alpha)

  cat("transforming...\n")

  df <- rank(df)
  df <- df - min(df)
  df <- df / max(df)
  df <- as.integer(df * (ncl - 1)) + 1
  df <- pal[df]
  df <- matrix(df, px, px, byrow = TRUE)
  rs <- as.raster(df)

  cat("rendering...\n")

  jpeg(
    filename = fpath,
    width = px,
    height = px,
    bg = bg
  )
  op <- par(mar = c(0,0,0,0))
  plot(rs)
  dev.off()
  par(op)

}

# seed
seeds <- 216:250
for(s in seeds) lover(s)

