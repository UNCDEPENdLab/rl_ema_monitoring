#######################################
# General Utils for rendering the site.
#######################################

# Method to append a yaml file to as a header
appendYaml <- function(yaml_file=NULL, append_file=NULL, add_delimiter=FALSE) {
  # read in the append file
  base_file_str <- read_file(append_file)
  # read in the yaml file
  yaml_file_str <- read_file(yaml_file)
  # default to add the --- 
  lines_3 <- ""
  # if an rmarkdown file, do not add them (rmarkdown will add automatically)
  if(add_delimiter) {
    lines_3 <- "---"
  }
  # create the string to write back to the append file
  new_file_str <- paste0(lines_3, yaml_file_str, lines_3, "\n\n", base_file_str)
  # write the appended file
  write_file(new_file_str, file=append_file, append=FALSE)
}

# Method to append a yaml files to as a headers, assuming same file name
appendYamlByName <- function(file_name=NULL, extension=NULL) {
  # get full yaml name
  full_yaml <- paste0(file_name, ".yaml")
  # get full file name
  full_base <- paste0(file_name, ".", extension)
  # append the yaml file
  appendYaml(yaml_file=full_yaml, append_file=full_base)
}

# Method to convert an archetype from .md files to 
addPage <- function(page_archetype=NULL, page_name=NULL, source_path=".") {
  # page_archetype: string, name of the archetype
  #   Should have a matching directory name in archetypes and under content
  #
  # page_name: string, name of the new page or grouping of pages
  #
  # source_path: path to site's root directory
  #
  # Note: some yaml and rmd files in the archetype begin as .md files
  # so that Hugo can use Go to dynamically set certain variables.
  # This is why lists of files to be converted are based on yaml_
  # and rmd_ prefixes to files in the archetype.
  
  # create the string to give to hugo
  add_cmd <- paste0("new ", page_archetype, "/", page_name, " -s ", source_path)
  # run the hugo/blogdown command to create the 
  blogdown::hugo_cmd(add_cmd)
  # get a path for the new files created
  page_path <- paste0(source_path, "/content/", page_archetype, "/", page_name)
  # get the to_yaml list
  to_yaml <- list.files(path=page_path, pattern="yaml_")
  # get the to_rmd list
  to_rmd <- list.files(path=page_path, pattern="rmd_")
  # convert mds to yamls
  # change extension to yaml
  yaml_ <- gsub(pattern=".md", replacement=".yaml", to_yaml)
  # remove leading yaml_
  yaml_removed <- gsub(pattern="yaml_", replacement="", yaml_, )
  # run the rename
  file.rename(paste0(page_path, "/", to_yaml), paste0(page_path, "/", yaml_removed))
  # convert mds to rmd
  # change extension to rmd
  rmd_ <- gsub(pattern=".md", replacement=".Rmd", to_rmd)
  # remove leading rmd_
  rmd_removed <- gsub(pattern="rmd_", replacement="", rmd_)
  # run the rename
  file.rename(paste0(page_path, "/", to_rmd), paste0(page_path, "/", rmd_removed))
  # get list of yaml files created
  yaml_files <- list.files(path=page_path, pattern=".yaml")
  # get list of yaml rmd files created
  rmd_files <- list.files(path=page_path, pattern=".Rmd")
  # get the yamls without their extension
  yaml_names <- gsub(pattern="*.yaml", replacement="", yaml_files)
  # get the rmds without their extension
  rmd_names <- gsub(pattern="*.Rmd", replacement="", rmd_files)
  # get rmds and yamls with matching names
  matched_names <- intersect(rmd_names, yaml_names)
  # append yaml files into rmds with the same file name
  for(matched_name in matched_names) {
    appendYamlByName(paste0(page_path, "/", matched_name), extension="Rmd")
  }
}

# Method to render a page from rmarkdown to a hugo-usable html page
render_rmd_hugo <- function(yaml_header=TRUE, rmd_file=NULL, html_name=NULL, dir_path=".") {
  # get the html file 
  html_file <- html_name
  # if html is null, then use the default render name
  if(is.null(html_name)) {
    html_file <- gsub(pattern=".Rmd", replacement=".html", rmd_file)
  }
  # Note: otherwise, a name for the file was given and assigned

  # render the page using rmarkdown
  rmarkdown::render(input=paste0(dir_path, "/", rmd_file), output_file=html_name)
  # if there should be a yaml header
  if(yaml_header == TRUE) {
    # append the yaml header to the output html
    appendYamlByName(paste0(dir_path, "/", gsub(pattern=".html", replacement="", html_file)), extension="html")
  }
}
