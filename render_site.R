source("infographiqR.R")

# update site ----

gsheets_to_csvs(gsheet)
create_svg_links()
add_gimage_paths() # add image_path to data/gsheets/figures.csv

create_modals()
render_modals()

create_scenes()
rmarkdown::render_site()

