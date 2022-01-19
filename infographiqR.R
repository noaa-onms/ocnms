# load libraries ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
shelf(
  dplyr, dygraphs, fs, glue, here, googledrive, googlesheets4, htmltools, knitr, 
  noaa-onms/onmsR, purrr, readr, rmarkdown, stringr, tibble, tidyr)
here <- here::here

# set variables ----- 

gsheet <- "https://docs.google.com/spreadsheets/d/1C5YAp77WcnblHoIRwA_rloAagkLn0gDcJCda8E8Efu4/edit"
# expecting Google sheets: scenes, modals, glossary
skip_drive_auth <- F

# authenticate to GoogleDrive using Google Service Account's secret JSON
#   after Sharing with its email: shares@nms4gargle.iam.gserviceaccount.com
if (Sys.getenv("GITHUB_ACTIONS") == ""){
  message("GITHUB_ACTIONS environmental variable is empty")
  
  google_sa_json <- switch(
    Sys.info()[['user']],
    # download from: https://drive.google.com/drive/u/1/folders/1k7DZBGTm4f8w4ppXdURSw67tnvzW4DsK
    bbest       = "/Users/bbest/My Drive (ben@ecoquants.com)/projects/nms-web/data/nms4gargle-774a9e9ec703.json",
    jaira       = "C:/Users/jaira/OneDrive/Documents/ocnms_extra/nms4gargle-774a9e9ec703.json",
    jai         = "/Users/jai/Documents/ocnms-extra/nms4gargle-774a9e9ec703.json",
    PikesStuff  = "/Users/PikesStuff/Theseus/Private/nms4gargle-774a9e9ec703.json")
  
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

create_svg_links <- function(
  modals_csv    = here::here("data/gsheets/modals.csv"),
  dir_modals    = "modals",
  svg_links_csv = here::here("svg/links.csv")){
  
  d <- readr::read_csv(modals_csv)
  
  # check for columns required
  flds_missing <- setdiff(
    c("svg", "icon", "link", "title", "not_modal"),
    names(d))
  if (length(flds_missing) > 0) stop(glue("Missing columns in modals_csv: {paste(flds_missing, collapse = ', ')}."))
  
  # write file
  d %>% 
    select(svg, icon, link, title, not_modal) %>% 
    mutate(
      link      = ifelse(not_modal, link, glue("{dir_modals}/{link}")),
      not_modal = ifelse(not_modal, "T", "F")) %>% 
    write_csv(svg_links_csv)
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
    #glink = "link is to shared drive at HQ: \\\\aamb-s-clust01\\Shared_Data\\ONMS\\Socioeconomic\\Olympic Coast NMS\\Condition Report 2019\\Final Excel Files with Graphs for CR June 2021"
    # glink = "https://docs.google.com/document/d/1PqeKj8L94i8lpioG9BtW9Bz63ovf20BE3ceMwacBXNA/edit?usp=sharing"
    
    
    gd <- try(googledrive::drive_get(glink), silent = T)
    if ("try-error" %in% class(gd) || nrow(gd)==0)
      return(NA)
    
    message(glue("get_image_path(): {glink}"))
    fname <- gd %>% pull(name) %>% str_replace_all(fixed("*"), "")
    fpath <- file.path(dir_figures, fname)
    
    # TODO: what if image changes but not file path?
    if (!file.exists(fpath)){
      gd <- try(drive_download(glink, fpath))
      if ("try-error" %in% class(gd))
        return(NA)
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
  
  #message(glue("gdrive2path(): {gdrive_shareable_link}"))
  # if (gdrive_shareable_link == "link is to shared drive at HQ: \\\\aamb-s-clust01\\Shared_Data\\ONMS\\Socioeconomic\\Olympic Coast NMS\\Condition Report 2019\\Final Excel Files with Graphs for CR June 2021"){
  #   browser()
  # }
  
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
  path_rmd,
  d_modals,
  d_figures,
  rmd_template = here::here("_modal.Rmd"),
  fld_html = "link",
  debug = F){
  
  # path_rmd = here("modals/deep-seafloor_benthic-invertebrates.html"); fld_html = "link"
  # path_rmd = here("modals/deep-seafloor_groundfish-assemblage.html"); fld_html = "link"
  # path_rmd = here("modals/pelagic_seabirds.html"); fld_html = "link"
  # path_rmd = here("modals/kelp-forest_sea-otters.html")
  
  modal_html <- fs::path_ext_set(basename(path_rmd), ".html")
  
  d_m <- d_modals %>% 
    filter(
      .data[[fld_html]] == !!modal_html)
  d_f <- d_figures %>% 
    filter(
      .data[[fld_html]] == !!modal_html,
      !is.na(image_path)) %>% 
    arrange(tab_group, tab, image_path)
  
  #* template ----
  attach(d_m)
  knitr::knit_expand(rmd_template) %>% 
    writeLines(path_rmd)
  detach(d_m)
  
  glue_rmd <- function(x, ...){
    if (debug)
      message(glue("glue_rmd(): {x}"))
    write(glue::glue(x, ...), path_rmd, append = T)
  }
  
  #* figures ----
  if (nrow(d_f) >= 1){ 
    has_tabgroups <- any(!is.na(d_f$tab_group))
    has_tabs      <- any(!is.na(d_f$tab))
    tabgroup_prev <- "INIT"
    
    d_f <- d_f %>% 
      mutate(
        across(c(tab, tab_group), tidyr::replace_na, ""))
    
    for (i in 1:nrow(d_f)){ # i = 1
      
      r_f <- d_f %>% slice(i)
      attach(r_f) # detach(r_f)
      
      if(has_tabgroups & i==1)
        glue_rmd(
          "
          
          # {.tabset}
          ", .open = "{{", .close = "}}")
      
      if(has_tabgroups & tabgroup_prev != tab_group)
        glue_rmd(
          "
            
          ## {tab_group}
          ")
      
      if(has_tabs & tabgroup_prev != tab_group)
        glue_rmd(
          "
          
          ### {.tabset}
          ", .open = "{{", .close = "}}")
      
      if(has_tabs)
        glue_rmd(
          "
          
          #### {tab}
          ")
      
      glue_rmd(
        '
        
        `r insert_figure("{link}", "{image_path}")`
        ')
      
      tabgroup_prev <- tab_group
      detach(r_f)
    }
  }
}

insert_figure_meta <- function(
  link, image_path,
  figures_csv = here::here("data/gsheets/figures.csv")){
  # link = "deep-seafloor_groundfish-assemblage.html"
  # image_path = "figures/S.LR.13.7 - Groundfish CPUE 2004-2018.jpg"
  
  d_figs <- read_csv(figures_csv)
  stopifnot(all(
    c("link", "image_path", 
      "monitoring_title", "monitoring_link", 
      "data_title", "data_link") %in% names(d_figs)))
  
  # monitoring program, data ----
  d <- d_figs %>% 
    filter(
      link       == !!link,
      image_path == !!image_path)
  
  if (nrow(d) != 1)
    stop(glue("insert_figure(): need exactly 1 row in figures_csv to match supplied `link` and `image_path`"))
  
  html  <- NULL
  no_ws <- c("before","after","outside","after-begin","before-end")
  
  icons <- tribble(
    ~description_bkup   ,    ~css,            ~icon,         ~fld_url, ~fld_description,
    "Monitoring Program",  "left", "clipboard-list", "monitoring_link", "monitoring_title",
    "Data"              , "right", "database"      ,       "data_link", "data_title")
  
  # caption | monitoring_title | monitoring_link | data_title | data_link
  
  #if (is.na(d$)
  
  for (i in 1:nrow(icons)){  # i=1
    
    h           <- icons[i,]
    url         <- d[h$fld_url]
    description <- d[h$fld_description]
    
    if(!is.na(url) & substr(url,0,4) == "http"){
      if (is.na(description)){
        description <- h$description_bkup
      } else {
        #description <- substr(str_trim(description), 0, 45) 
        description <- str_trim(description)
      }   
      
      html <- tagList(
        html, 
        div(
          .noWS = no_ws,
          style = glue("text-align:{h$css}; display:table-cell;"),
          a(
            .noWS = no_ws,
            href = url, target = '_blank',
            shiny::icon(h$icon), description)))
    }
  }
  
  if (is.null(html))
    return("")
  
  tagList(
    div(
      .noWS = no_ws,
      style = "background:LightGrey; width:100%; display:table; font-size:120%; padding: 10px 10px 10px 10px; margin-bottom: 10px;",
      div(
        .noWS = no_ws,
        style = "display:table-row",
        html)))
}

insert_caption <- function(
  link, tab,
  figures_csv = here::here("data/gsheets/figures.csv")){
  # link = "rocky-shore_barnacles.html"; tab = "Trends - Acorn barnacles"

  d <- read_csv(figures_csv) %>% 
    filter(
      link == !!link,
      tab  == !!tab)
  
  if (nrow(d) != 1)
    stop(glue("insert_caption(): need exactly 1 row in figures_csv to match supplied `link` and `tab`"))
  
  tagList(
    htmltools::HTML(markdown::renderMarkdown(text = glue("{d$caption}"))))
}

insert_figure <- function(
  link, image_path,
  insert_meta = T,
  figures_csv = here::here("data/gsheets/figures.csv")){
  # link = "deep-seafloor_groundfish-assemblage.html"
  # image_path = "figures/S.LR.13.7 - Groundfish CPUE 2004-2018.jpg"
  
  h = NULL
  if (insert_meta)
    h = insert_figure_meta(link, image_path, figures_csv)
  
  d_figs <- read_csv(figures_csv)
  stopifnot(all(
    c("link", "image_path", 
      "caption") %in% names(d_figs)))
  
  d <- d_figs %>% 
    filter(
      link       == !!link,
      image_path == !!image_path)
  
  if (nrow(d) != 1)
    stop(glue("insert_figure() needs exactly 1 row (nrow={nrow(d)}) 
              in figures_csv to match supplied `link` and `image_path`.
              Please update:
                [Master_OCNMS_infographic_content](https://docs.google.com/spreadsheets/d/1C5YAp77WcnblHoIRwA_rloAagkLn0gDcJCda8E8Efu4/edit#gid=1464497539)
              and re-run render_site.R:
                source('infographiqR.R')
                gsheets_to_csvs(gsheet)
                add_gimage_paths() # adds image_path to data/gsheets/figures.csv"))
  
  tagList(
    h, 
    htmltools::HTML(markdown::renderMarkdown(text = glue("![]({d$image_path}) {d$caption}"))))
}


create_modals <- function(
  modals_csv  = here::here("data/gsheets/modals.csv"),
  figures_csv = here::here("data/gsheets/figures.csv"),
  dir_modals  = here::here("modals")){
  # modals_csv  = here::here("data/gsheets/modals.csv"); figures_csv = here::here("data/gsheets/figures.csv")
  
  d_modals_rmd  <- readr::read_csv(modals_csv, show_col_types = F) %>% 
    filter(
      !not_modal) %>% 
    mutate(
      path_rmd = file.path(
        dir_modals,
        fs::path_ext_set(basename(link), ".Rmd")),
      path_html = file.path(
        dir_modals,
        basename(link)))
  d_modals  <- d_modals_rmd %>% 
    filter(
      !dynamic_modal)
  d_figures <- readr::read_csv(figures_csv, show_col_types = F)
  
  if (!dir.exists(dir_modals))
    dir.create(dir_modals)
  
  for (path_rmd in d_modals$path_rmd){
    message(glue("creating modal: {basename(path_rmd)}"))
    create_modal(path_rmd, d_modals, d_figures)
  }
  
  # delete unused modal Rmd/html files
  rmd_del <- setdiff(list.files(dir_modals, "Rmd$"), basename(d_modals_rmd$path_rmd))
  unlink(file.path(dir_modals, rmd_del))
  html_del <- setdiff(list.files(dir_modals, "html$"), basename(d_modals_rmd$path_html))
  unlink(file.path(dir_modals, html_del))
}

render_modals <- function(
  dir_modals = here::here("modals")){
  
  # TODO: blacklist certain Rmds to be rendered manually
  d <- tibble(
    input = list.files(dir_modals, ".*\\.Rmd$", full.names = T))
  # View(d)
  # which(basename(d$input) == "rocky-shore_mussels.Rmd")
  # gsheets_to_csvs(gsheet); add_gimage_paths() 
  d %>%
    slice(61:nrow(d)) %>% 
    purrr::pwalk(rmarkdown::render)
}

create_scene <- function(
  title, svg, 
  rmd_out       = glue::glue(here::here("{svg}.Rmd")),
  svg_links_csv = here::here("svg/links.csv"),
  dir_svg       = here::here("svg"),
  rmd_template  = here::here("_scene.Rmd"),
  ...){
  
  # d <- readr::read_csv(scenes_csv) %>% 
  #   filter(svg == "rocky_shore")
  # attach(d)
  
  # rmd_template = here::here("_scene.Rmd")
  # dir_svg      = here::here("svg")
  
  # make paths relative to project
  svg_csv <- stringr::str_replace(svg_links_csv, glue("{here::here()}/"), "")
  dir_svg <- stringr::str_replace(dir_svg, glue("{here::here()}/"), "")
  
  knitr::knit_expand(rmd_template) %>% 
    writeLines(rmd_out)
}

create_scenes <- function(
  scenes_csv    = here::here("data/gsheets/scenes.csv"),
  dir_svg       = here::here("svg"),
  svg_links_csv = here::here("svg/links.csv")){
  
  readr::read_csv(scenes_csv) %>% 
    purrr::pwalk(create_scene)
}

insert_info <- function(
  rmd = knitr::current_input(),
  csv = here::here("data/gsheets/scenes.csv"),
  debug = F){
  # rmd = here::here("rocky_shore.Rmd")
  
  svg <- basename(fs::path_ext_remove(rmd))

  d_all <- readr::read_csv(csv)
  
  if (debug)
    message(glue("rmd: {rmd}"))
  
  flds_missing <- setdiff(c("svg", "info_link", "info_photo_link", "info_tagline") , names(d_all))
  if (length(flds_missing) > 0) 
    stop(glue("Missing expected columns in csv: {paste(flds_missing, collapse = ', ')}."))
  
  if ("link" %in% names(d_all)){
    # modal
    html <- basename(fs::path_ext_set(rmd, ".html"))
    d <- d_all %>%
      dplyr::filter(link == !!html)
  } else {
    # scene
    d <- d_all %>%
      dplyr::filter(svg == !!svg)
  }
  
  if (nrow(d) != 1)
    stop("Need rmd to match 1 possible value of svg column in scenes_csv.")
  
  icons_html = NULL
  if (!is.na(d$info_link)){
    icons_html =
      htmltools::a(shiny::icon("info-circle"), href=d$info_link, target='_blank')
  }
  if (!is.na(d$info_photo_link)){
    icons_html = htmltools::tagList(
      icons_html,
      htmltools::a(shiny::icon("camera"), href=d$info_photo_link, target='_blank'))
  }
  
  htmltools::div(
    htmltools::div(
      htmltools::tagList(
        icons_html), 
      style = "margin-top: 10px;margin-bottom: 10px; margin-right: 10px; flex: 1;"), 
    htmltools::div(
      ifelse(
        !is.na(d$info_tagline), 
        d$info_tagline, ""), 
      style = "margin: 10px; font-style: italic; flex: 20; "), 
    style="display: flex")
}

