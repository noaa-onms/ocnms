source("infographiqR.R")

# update site ----
gsheets_to_csvs(gsheet) # downloads the Google Sheet tabs into seperate CSVs for quick reading
add_gimage_paths()      # add image_path to data/gsheets/figures.csv
create_svg_links()      # 

# TODO: + arg for only_new = T, eg for rocky-shore MARINe versions being interactive & diff't
create_modals()         # 
render_modals()

create_scenes()
rmarkdown::render_site()

