source("infographiqR.R")
# remotes::install_github("noaa-onms/onmsR")

# update site ----
gsheets_to_csvs(gsheet) # downloads the Google Sheet tabs into separate data/*.csv files for quick reading
add_gimage_paths()      # adds image_path to data/gsheets/figures.csv

create_svg_links()      # create table (csv) that links svg icons to modal html links

create_modals()         # except where dynamic_modal == TRUE
render_modals()         # render static and dynamic modals

create_scenes()         # create scenes.Rmd (overview & habitats) with SVGs
render_site()           # render all *.Rmd files in root (not subdirs like modals/)

