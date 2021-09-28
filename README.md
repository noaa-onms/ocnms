# ocnms

Olympic Coast National Marine Sanctuary (OCNMS) - interactive infographic site

This website uses a simple interactive infographics implementation based on JavaScript only (ie not using the R-based [infographiq](https://github.com/marinebon/infographiq)).

## technical implementation

The illustration in scalable vector graphics (`.svg`) format has individual elements given an identefier (ie `id`) which are linked to popup (ie "modal") windows of content using a simple table in comma-seperated value (`.csv`) format using [d3](https://d3js.org).

### core files: `.svg`, `.csv`

These two files are at the core of the infographic construction:

1. **illustration**: [`mbnms_pelagic.svg`](https://github.com/marinebon/mbnms/blob/master/svg/mbnms_pelagic.svg) 
1. **table**: [`svg_links.csv`](https://github.com/marinebon/iea-ak-info/blob/master/svg/svg_links.csv) 

Each `link` in the table per element identified (`id`) is the page of content displayed in the modal popup window when the given element is clicked. The `title` determines the name on hover and title of the modal window.

### html and js/css dependencies

The illustration (`.svg`) and table (`.csv`) get rendered with the `link_svg()` function (defined in `infographiq.js`) with the following HTML:

```html
<!-- load dependencies on JS & CSS -->
<script src='https://d3js.org/d3.v5.min.js'></script>
<script src='infographiq.js'></script>

<!-- add placeholder in page for placement of svg -->
<div id='svg'></div>

<!-- run function to link the svg -->
<script>link_svg(svg='svg/mbnms_pelagic.svg', csv='svg/svg_links.csv');</script>
```

The modal popup windows are rendered by [Bootstrap modals](https://getbootstrap.com/docs/3.3/javascript/#modals). This dependency is included with the default Rmarkdown rendering, but if you need to seperately include it then add this HTML:

```html
<!-- load dependencies on JS & CSS -->
<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css">
<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css">
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"></script>
```

## build and view website in R

This website is constructed using [Rmarkdown website](https://bookdown.org/yihui/rmarkdown/rmarkdown-site.html) for enabling easy construction of site-wide navigation (see [`_site.yml`](https://github.com/marinebon/iea-ak-info/blob/master/_site.yml)) and embedding of [htmlwidgets](https://www.htmlwidgets.org), which provide interactive maps, time series plots, etc into the html pages to populate the modal window content in [`modals/`](https://github.com/marinebon/iea-ak-info/tree/master/modals). To build the website and view it, here are the commands to run in R:

## develop

### content editing workflow

1. edit .Rmd files in `./docs/modals/`
2. run `make_site.R`

NOTE: The `.html` files *can* be edited but by default `.html` files are overwritten by content knit from the `Rmd` files of the same name.
To use html directly set `redo_modals <- T`, but you will need to clear `.html` files manually with this setting.

### testing

Because of [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) restrictions, need local web server to debug:

```r
# build website
source("render_site.R")

# serve website
servr::httd()
```

or using Python:

```bash
cd ~/github/mbnms/docs; python -m SimpleHTTPServer
```

The [`render_site.R`](https://github.com/marinebon/iea-ak-info/blob/master/render_site.R) script renders the modal and website pages.

Note the actual html content served at [marinebon.github.io/mbnms](https://marinebon.github.io/mbnms) via [Github Pages](https://pages.github.com/) is all the html/jss/csss files copied into the `docs/` folder of this repository.

## Authenticate to Google Sheet

```r
if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  DT,
  googlesheets4,
  here)

gsheets_sa_json <- switch(
  Sys.info()[["effective_user"]],
  bbest      = "/Volumes/GoogleDrive/My Drive/projects/nms-web/data/nms4gargle-774a9e9ec703.json",
  jai        = "/Volumes/GoogleDrive/My Drive/service-tokens/nms4gargle-774a9e9ec703.json",
  PikesStuff = "/Users/PikesStuff/Theseus/Private/nms4gargle-774a9e9ec703.json")

gsheet <- "https://docs.google.com/spreadsheets/d/1C5YAp77WcnblHoIRwA_rloAagkLn0gDcJCda8E8Efu4/edit"
```

### Setup Service Account API Key

Reference:

- [googlesheets4 auth • googlesheets4](https://googlesheets4.tidyverse.org/articles/articles/auth.html)
- [Non-interactive auth](https://cran.r-project.org/web/packages/gargle/vignettes/non-interactive-auth.html): main reference for understanding this

Steps:

1. Save [nms4gargle-774a9e9ec703](https://drive.google.com/file/d/1LTvMM74gYB5MAkspiTqjA4BiTzVLTgNp/view?usp=sharing) locally, in this case to `r gsheets_sa_json`.

  1. Manage [shares – IAM & Admin – nms4gargle – Google Cloud Platform](https://console.cloud.google.com/iam-admin/serviceaccounts/details/111162214432618062602?authuser=2&project=nms4gargle). Added permissions to : jai.ranganathan@gmail.com, pike.spector@noaa.gov.
  
1. Share [Master_OCNMS_infographic_content - Google Sheets](`r gsheet`) to `shares@nms4gargle.iam.gserviceaccount.com`.


### Use

```{r}
# ensure secret JSON file exists
stopifnot(file.exists(gsheets_sa_json))

# authenticate to GoogleSheets using Google Service Account's secret JSON
gs4_auth(path = gsheets_sa_json)

gsa_json_text <- readLines(gsheets_sa_json) %>% paste(sep="\n")
# listviewer::jsonedit(jsonlite::fromJSON(gsa_json_text))
# cat(gsa_json_text)
gs4_auth(path = gsa_json_text)

# access to this Google Sheet was granted to the Google Service Account
#   by Sharing with its email: shares@nms4gargle.iam.gserviceaccount.com
sheet_names(gsheet)

# read in the tab called figures
tbl_modals <- read_sheet(gsheet, "figures")

datatable(tbl_modals)
```

