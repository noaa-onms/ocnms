# load libraries ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
shelf(
  dplyr, fs, glue, here, googledrive, googlesheets4, htmltools, knitr, purrr, readr, rmarkdown, stringr, tibble, tidyr)
here <- here::here

# set variables ----

gsheet <- "https://docs.google.com/spreadsheets/d/1C5YAp77WcnblHoIRwA_rloAagkLn0gDcJCda8E8Efu4/edit"
# expecting Google sheets: scenes, modals, glossary
skip_drive_auth <- F

# authenticate to GoogleDrive using Google Service Account's secret JSON
#   after Sharing with its email: shares@nms4gargle.iam.gserviceaccount.com
if (Sys.getenv("GITHUB_ACTIONS") == ""){
  message("GITHUB_ACTIONS environmental variable is empty")
  google_sa_json <- "/Users/bbest/My Drive (ben@ecoquants.com)/projects/nms-web/data/nms4gargle-774a9e9ec703.json"
  stopifnot(file.exists(google_sa_json))
  gsa_json_text <- readLines(google_sa_json) %>% paste(sep="\n")
} else {
  gsa_json_text <- Sys.getenv("GOOGLE_SA")
  message('nchar(Sys.getenv("GOOGLE_SA")): ', nchar(Sys.getenv("GOOGLE_SA")))
}
if (!skip_drive_auth){
  message("non-interactively authenticating to GoogleDrive with Google Service Account")
  googledrive::drive_auth(path = gsa_json_text)
  googlesheets4::gs4_auth(path = gsa_json_text)
}

# define functions ----

gsheets_to_csvs <- function(gsheet, dir_csvs=here::here("data/gsheets")){
  # dir_csvs=here::here("data/gsheets")
  
  if (!dir.exists(dir_csvs))
    dir.create(dir_csvs, showWarnings = F)
  
  gshts <- googlesheets4::sheet_names(gsheet)
  for (gsht in gshts){ # gsht = gshts[1]
    d <- googlesheets4::read_sheet(gsheet, sheet = gsht)
    csv <- glue::glue("{dir_csvs}/{gsht}.csv")
    readr::write_csv(d, csv)
  }
  
}

add_gimage_paths <- function(
  figures_csv = here::here("data/gsheets/figures.csv"),
  fld_glink   = "image_link",
  fld_path    = "image_path",
  dir_figures = "modals/figures",
  dir_modals  = "modals"){
  # add google image paths
  # figures_csv = here::here("data/gsheets/figures.csv"); fld_glink   = "image_link"; fld_path    = "image_path"; dir_figures = "modals/figures"; dir_modals  = "modals"
  
  stopifnot(file.exists(figures_csv))
  
  d_figures <- readr::read_csv(figures_csv, show_col_types = F)
  
  stopifnot(fld_glink %in% names(d_figures))
  
  if (!dir.exists(dir_figures))
    dir.create(dir_figures, showWarnings = F)

  get_image_path <- function(glink){
    if (is.na(glink))
      return(NA)
    
    #browser()
    # glink = "https://drive.google.com/file/d/1u_-SA9OSjFUOuxzKs5ESUZCmnv-i0xCg/view?usp=sharing"
    # glink = "https://drive.google.com/file/d/1bWyZOGzZQvcaESC5Y3vX9uDL9NRjVsyn/view?usp=sharing"
    
    gd <- try(googledrive::drive_get(glink), silent = T)
    if ("try-error" %in% class(gd))
      return(NA)
    fname <- gd %>% pull(name)
    fpath <- file.path(dir_figures, fname)
    
    # TODO: what if image changes but not file path?
    if (!file.exists(fpath)){
      drive_download(glink, fpath)
    }
    
    # strip dir_modal, so from modal figure is found
    dir_figures4modal <- str_replace(dir_figures, glue::glue("{dir_modals}/"), "")
    file.path(dir_figures4modal, fname)
  }
  
  d_figures <- d_figures %>% 
    mutate(
      across(all_of(fld_glink), ~ purrr::map_chr(.x, get_image_path), .names = fld_path))
  
  d_figures %>% 
    filter(!is.na(image_link) & is.na(image_path)) %>% 
    select(link, image_link) %>% 
    write_csv("data/gsheets/figures_needs-to-be-shared_shares@nms4gargle.iam.gserviceaccount.com.csv")
  
  readr::write_csv(d_figures, figures_csv)
}

gdrive2path <- function(gdrive_shareable_link, get_relative_path = T, relative_pfx = "../", redo = F, skip_spectrogram = F){
  
  # gdrive_shareable_link <- "https://drive.google.com/file/d/1_wWLplFmhEAEqmbsTA0D85yuAhmapc5a/view?usp=sharing"
  # gdrive_shareable_link <- tbl$gdrive_shareable_link; get_relative_path = T; redo = F
  # gdrive_shareable_link <- sound$sound_enhancement; get_relative_path = T; relative_pfx = "../", redo = F, skip_spectrogram = T; redo = F
  
  regex <- ifelse(
    str_detect(gdrive_shareable_link, "/file/"),
    "https://drive.google.com/file/d/(.*)/view.*",
    "https://drive.google.com/open\\?id=(.*)")
  gid <- str_replace(gdrive_shareable_link, regex, "\\1") %>%
    str_trim()
  
  fname <- try(drive_get(as_id(gid))$name)
  
  message(glue("gdrive_shareable_link: {gdrive_shareable_link}"))
  message(glue("  gid: {gid}"))
  
  
  if (class(fname) == "try-error")
    return(NA)
  
  fname_ok      <- fname %>% str_replace_all("/", "_")
  path          <- here(glue("draft/files/{fname_ok}"))
  path_relative <- glue("{relative_pfx}files/{fname_ok}")
  message(glue("  fname_ok: {fname}"))
  
  if (!file.exists(path) | redo)
    drive_download(as_id(gid), path)
  
  if (path_ext(path) %in% c("mp3","wav") & !skip_spectrogram){
    
    # path <- "/Users/bbest/github/sanctsound/files/SanctSound_CI02_01_HumpbackWhale_20181103T074755.wav"
    # path = "/Users/bbest/github/sanctsound/files/output.mp3"
    path_mp4 <- path_ext_set(path, "mp4")
    
    if (!file.exists(path_mp4) | redo)
      path_mp4 <- audio_to_spectrogram_video(path, path_mp4)
    
    path          <- path_mp4
    path_relative <- glue("{relative_pfx}files/{basename(path_mp4)}")
  }
  
  if (get_relative_path)
    return(path_relative)
  
  path
}


create_modal <- function(
  modal_html,
  d_modals,
  d_figures,
  dir_modals = here::here("modals"),
  fld_html = "link"){
  
  # modal_html = "deep-seafloor_benthic-invertebrates.html"; dir_modals = here::here("modals"); fld_html = "link"
  # modal_html = "deep-seafloor_groundfish-assemblage.html"; dir_modals = here::here("modals"); fld_html = "link"
  
  path_rmd <- file.path(
    dir_modals,
    fs::path_ext_set(basename(modal_html), ".Rmd"))
  
  d_m <- d_modals %>% 
    filter(.data[[fld_html]] == !!modal_html)
  d_f <- d_figures %>% 
    filter(
      .data[[fld_html]] == !!modal_html,
      !is.na(image_path))
    
  #* yaml ----
  write(glue::glue(
    "
    ---
    pagetitle: {d_m$title}
    ---
    
    "), path_rmd)
  
  #* info_* ----
  for (ikey in c("info_link", "info_photo_link",	"info_tagline")){ # ikey = "info_link"
    ival <- d_m[[ikey]]
    if (!is.na(ival)){
      write(glue::glue(
        "
        - [{ikey}]({ival})
        "), path_rmd, append = T)
    }
  }
  write("\n", path_rmd, append = T)
  
  #* figures ----
  if (nrow(d_f) >= 1){ 
    for (i in 1:nrow(d_f)){ # i = 1
      r_f <- d_f %>% slice(i)
      attach(r_f)
      
      if(!is.na(tab) & i==1)
        write(glue::glue(
          "
          
          # {.tabset}
          ", .open = "{{", .close = "}}"), 
          path_rmd, append = T)
      
      if(!is.na(tab))
        write(glue::glue(
          "
          
          ## {tab}
          "), path_rmd, append = T)
      
      write(glue::glue(
        "
        ![]({image_path})
        "), path_rmd, append = T)
      
      detach(r_f)
    }
  }
}

create_modals <- function(
  modals_csv  = here::here("data/gsheets/modals.csv"),
  figures_csv = here::here("data/gsheets/figures.csv")){
  # modals_csv  = here::here("data/gsheets/modals.csv"); figures_csv = here::here("data/gsheets/figures.csv")
  
  d_modals  <- readr::read_csv(modals_csv, show_col_types = F)
  d_figures <- readr::read_csv(figures_csv, show_col_types = F)
  
  for (mdl_html in d_modals$link){
    # mdl_html = "deep-seafloor_benthic-invertebrates.html"
    # mdl_html = "deep-seafloor_groundfish-assemblage.html" (n_figures = 4)

    message(glue("mdl_html: {mdl_html}"))
    create_modal(mdl_html, d_modals, d_figures)
  }
}

# update site ----
gsheets_to_csvs(gsheet)
add_gimage_paths()
create_modals()


