# Load Required Libraries
library(sf)
library(tidyverse)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)

# Load Kontur Data
data <- st_read("kontur_population_EG_20220630.gpkg")

# Set Bounding Box
bb <- st_bbox(data)

# Determine Width and Height
## Calculate width
bottom_left <- st_point(c(bb[['xmin']], bb[['ymin']])) |> 
  st_sfc(crs=st_crs(data))

bottom_right <- st_point(c(bb[['xmax']], bb[['ymin']])) |> 
  st_sfc(crs=st_crs(data))

width <- st_distance(bottom_left, bottom_right)

## Calculate height
top_left <- st_point(c(bb[['xmin']], bb[['ymax']])) |> 
  st_sfc(crs=st_crs(data))

height <- st_distance(top_left, bottom_left)

# Handle Aspect Ratio
if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
}

# Data Conversion: Spatial Data to Matrix
size = 5000
rast <- st_rasterize(data, nx=floor(size*w_ratio), 
                     ny=floor(size*h_ratio))
mat <- matrix(data=rast$population, nrow =floor(size*w_ratio), 
              ncol =floor(size*h_ratio))

# Generate Color Palette using MetBrewer
c1 <- met.brewer('Greek')
swatchplot(c1)
texture <- grDevices::colorRampPalette(c1, bias=2)(256)
swatchplot(texture)

# Create 3D Plot using Rayshader
# *If the plot_3d function causes a fatal error,install Rayshader
#  directly from Github using the remotes package.

rgl::close3d() # Close any open RGL windows
mat |> height_shade(texture = texture) |> 
  plot_3d(heightmap = mat, zscale = 200/5, 
          solid = F, shadowdepth = 0)

# Adjust Camera Angles
render_camera(theta = -20, phi = 45, zoom = 0.8)

# Render Plot and Save as PNG
# The approach below is to ensure that if the render fails but 
# a file is generated, it can be assumed that the errors weren't
# associated with incorrect files paths or permissions.

outfile = 'images\\final_plot.png'
{
  start_time <- Sys.time()
  cat(crayon::red(start_time), '\n')
  if(!file.exists(outfile)){  
    png::writePNG(matrix(1), target = outfile) 
  }
  
  render_highquality(
    filename = outfile,
    interactive = F,
    lightdirection = 280,
    lightaltitude = c(30, 80),
    lightcolor = c(c1[4], 'white'),
    lightintensity = c(600, 100),
    samples = 450,
    width = 6000,
    height = 6000
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::red(diff), "\n")
}
