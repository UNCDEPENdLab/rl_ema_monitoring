# make_dashboard_site.R
#
# Updates and makes R Markdown RL EMA QA site
#should pull from a function with config info
setwd("~/Data_Analysis/Momentum/rl_ema_monitoring/dashboard/")


# Install and/or load packages
library(pacman)
p_load(tidyverse, data.table, checkmate, DT, RSQLite,
       ggplot2, zoo, sqldf, rjson)

source("dashboard_utils.R")
source("render_utils.R")
source("site_render_spec.R")

dashboard_dir <- getwd()
data_dir <- normalizePath(file.path(dashboard_dir, "..", "data"))

#update data cache (Shane: develop R hook)
#update_data_cache(dashboard_dir)

force_regen <- TRUE #whether to force reports to regenerate

#get info about current data
slist <- getSubjList(data_dir)

#render subject compliance
render_subject_compliance_reports(slist, dashboard_dir, force=force_regen)

#render physio reports
# render_subject_physio_reports(dashboard_dir)

#render video reports
# render_subject_video_reports(dashboard_dir)


#Example of rendering specific Rmd files

## rmarkdown::render(input = "talks/bootcamp-day-1-intro.Rmd", 
##                   output_format = "ioslides_presentation")

## rmarkdown::render(input = "talks/slow-r.Rmd", 
##                   output_format = c("html_document"))

## rmarkdown::render(input = "talks/r-eproducible-science.Rmd", 
##                   output_format = c("html_document"))

# Render site last so that updated versions get copied to rendered_site/
##Shane: NEED TO MAKE THIS NOT DELETE CUSTOM HTMLS -- may need a custom generator, or perhaps just stick to this file as a makefile
##Haven't figured out how to preserve the navbar and other general site features on report pages without a custom renderer -- so that may be the right direction
#rmarkdown::render_site()

rmarkdown::render(input = "index.Rmd", output_format = c("html_document"), output_file = "index.html", output_dir = "rendered_site")


#save rendering environment
insert_into_report_cache(dashboard_dir, arguments=list(session_info=sessionInfo()))

