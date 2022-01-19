source("infographiqR.R")
# remotes::install_github("noaa-onms/onmsR")

# update site ----
gsheets_to_csvs(gsheet) # downloads the Google Sheet tabs into separate data/*.csv files for quick reading
add_gimage_paths()      # adds image_path to data/gsheets/figures.csv
# Error : Unrecognized `type`:                                                                                                           
#   x JPG
# Error : Path exists and overwrite is FALSE
create_svg_links()      # create table (csv) that links svg icons to modal html links

# TODO: + arg for only_new = T, eg for rocky-shore MARINe versions being interactive & diff't
create_modals()         # 
render_modals()

create_scenes()
rmarkdown::render_site()

