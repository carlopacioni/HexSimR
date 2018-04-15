#' Batch modification of scenarios
#' 
#' Modify several scenarios at once by replacing, deleting or adding nodes (e.g.
#' events) in scenario .xml files.
#' 
#' If several scenarios have been developed in HexSim and later the user 
#' realises that there are some modifications that need to be applied to several
#' or all of them, this function can help automate this task. It involves 
#' generating an scenario with the correct/new events and parameters. This 
#' scenario is used then as template and the nodes that needs to be added, 
#' replaced or deleted are identified using a .csv. Some setting up is required 
#' and it may not be worthwhile for only one or two scenarios. To use this 
#' function some understanding of the xml file structure in HexSim is necessary.
#' The easiest is to open the xml file and have a look at the structure. Using a
#' source code editor like Notepad++ (that colour-code different element of the 
#' text based on the language used) can help to understand the structure. 
#' Briefly, each of the settings and parameters in HexSim is set by a node in 
#' the xml file. Some nodes have a unique identifier, which can be an attribute 
#' (generally name="\emph{attribute_value}"), or a text element value (e.g. 
#' <name> \emph{node_name} </name>). To this end, identifiers are considered 
#' element that make the node unique. For example, accumulate events have all 
#' the same structure, but they can be identified by the value given to the node
#' <name>. On the contrary, there are nodes that are unique and can be simply 
#' identified by the node's name. One example of such nodes is 
#' <initializationSpatialData>, which is unique, so a search in the xml file for
#' the path: "/scenario/population/initializationSpatialData" will return one 
#' node only. From a practical point of view, the easier is to create or modify 
#' a scenario with the correct events/parameters. Save it and then comparing 
#' this with one of the one that needs to be modified using, for example, the 
#' plug in "compare" in Notepad++, or Winmerge (on Windows).
#' 
#' If \code{path.scenarios=NULL}, an interactive dialog box is used to select 
#' the path where the scenario xml files are located.
#' 
#' If \code{scenarios="all"} (default), all scenarios are processed, otherwise 
#' it is possible to select a subset of scenarios using a character vector, e.g.
#' scenarios=c("scen1", "scen2"). \bold{Note:} when a \code{xml.template} is 
#' passed, this is assumed to be in the \code{path.scenarios} directory and it 
#' is removed from the list of scenarios to be modified.
#' 
#' A back up of the folder \code{path.scenarios} is copied in the the folder 
#' "Scenarios_bkup", one level up from \code{path.scenarios}.
#' 
#' The .csv file essentially provides the information on how to locate the nodes
#' that need to be modified, and what type of actions need to be carried out.The
#' .csv must be located in the scenario folder and the name should be passed 
#' with \code{csv.in} as a character vector. The file must have the following 
#' headings: nodes, identifier, attribute, mode, ref, ref_identifier,	and 
#' ref_attribute. \code{scenarios.batch.modifier} will parse the file and use 
#' these columns as arguments.
#' 
#' The column with heading \bold{nodes} is the path to the node to be searched 
#' in the \code{xml.template} file, except when the \bold{mode} is "delete", in 
#' which case the nodes are searched in the scenario file and then deleted. The 
#' path starts at the root of the xml file ("scenario") and progresses until the
#' node's name or identifier. It must start with a "/", and must not have a "/" 
#' at the end. For example, for an accumulator, the path in the column "nodes" 
#' would be: "/scenario/population/accumulators/accumulator/name".
#' 
#' \bold{identifier} is used to indicate whether the node has an identifier. If 
#' it does (e.g. name="\emph{attribute_value}"), the name (or the value if an 
#' attribute) of the identifier needs to be in this column (e.g. 
#' \emph{attribute_value}), otherwise FALSE must be used.
#' 
#' \bold{attribute} indicates whether the identifier is an attribute (TRUE) or 
#' not (FALSE).
#' 
#' \bold{mode} indicates the type of action that need to be performed. Possible 
#' options are "add", "before", "after", "replace" or "delete". If "add" the 
#' node is added as last child of the parent node. If the node needs to be added
#' in a specific position (events generally do), "before" and "after" should be 
#' used to indicate the position respect to the \bold{ref} node (i.e. whether 
#' the new node goes before or after the node \bold{ref}. Note that the 
#' \bold{ref} node is searched in the scenario file, not the 
#' \code{xml.template}. If the option "delete" is used, the node is searched and
#' deleted from the scenario file. When "replace" is used, the \bold{ref} node 
#' must be provided, even when the node is the same. This is because there might
#' be situations where what it is being changed is the name of the node. In
#' these cases, \code{scenarios.batch.modifier} would not find the original node
#' in the scenario file.
#' 
#' \bold{ref} needs to be passed when the options "before", "after" or "replace"
#' are used in \bold{mode}. When not relevant, \code{NA} is used. When \bold{ref} is used, then
#' a search is performed in the scenario xml file and the fields 
#' \bold{ref_identifier} and	\bold{ref_attribute} must also be passed when 
#' relevant. \bold{ref_identifier} and	\bold{ref_attribute} have the same 
#' meaning as \bold{identifier} and \bold{attribute}, but they refer to the 
#' \bold{ref} node. Use FALSE when these are not relevant.
#' 
#' @param path.scenarios The path to the 'Scenarios' folder
#' @param xml.template The name of the xml file to use as template for the new 
#'   nodes
#' @param csv.in The name of the .csv file in the "Scenarios" folder
#' @inheritParams collate.census
#' @import xml2
#' @importFrom tcltk tk_choose.dir
#' @export
scenarios.batch.modifier <- function(
  path.scenarios=NULL,
  scenarios="all", 
  xml.template=NULL,
  csv.in) {
  
  #### Setting arguments ####
  txt <- "Please, select the 'Scenarios' folder within the workspace"
  if(is.null(path.scenarios)) path.scenarios <- tk_choose.dir(caption=txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.files(path=path.scenarios, pattern=".xml$", 
                            full.names=FALSE, recursive=FALSE))
  if(!is.null(xml.template)){
    scenarios <- scenarios[!scenarios %in% xml.template]
  }
  
  csv_file <- read.csv(file=file.path(path.scenarios, csv.in), stringsAsFactors=FALSE)
  node_paths <- csv_file[, "nodes"]
  modes <- csv_file[, "mode"]
  identifiers <- csv_file[, "identifier"]
  attribs <- csv_file[, "attribute"]
  refs <- csv_file[, "ref"]
  ref_identifiers <- csv_file[, "ref_identifier"]
  ref_attribs <- csv_file[, "ref_attribute"]
  
  #### Creating new nodes and ref Xpaths ####
  nodes <- vector("list", length(node_paths))
  Xpaths <- vector("character", length(node_paths))
  if(sum(is.na(refs))) ref_Xpaths <- vector("character", length(node_paths))
  if(!is.null(xml.template)) 
    xml_template <- read_xml(file.path(path.scenarios, xml.template), options="")
  
  for(i in seq_along(node_paths)) {
    
    if(!is.logical(identifiers[i])) {
      last_slash <- gregexpr("/", node_paths[i])
      indentifier_name <- substr(node_paths[i], start=max(last_slash[[1]]) + 1, 
                                 stop=nchar(node_paths[i]))
      Xpaths[i] <- paste0(sub(pattern=paste0("/", indentifier_name), 
                          replacement="", 
                          x=node_paths[i]),
                      paste0("[", if(attribs[i]) "@", indentifier_name, 
                             "='", identifiers[i], "']"))
    } else {
      Xpaths[i] <- node_paths[i]
    }
    
    if(modes[i] != "delete") {
      nodes[[i]] <- xml_find_all(xml_template, Xpaths[i])  
      if(grepl(pattern = "Event", x = xml_path(nodes[[i]]))) {
        nodes[[i]] <- xml_parent(nodes[[i]]) 
      }
    } 
    
    if(length(nodes[[i]]) > 1) {
      stop(paste(
        "More than one node identified with the node path:", 
        node_paths[[i]], "in the file", xml.template))
    }
    # ref Xpaths
    if(!is.na(refs[i])) {
      if(!is.logical(ref_identifiers[i])) {
        last_slash <- gregexpr("/", refs[i])
        indentifier_name <- substr(refs[i], start=max(last_slash[[1]]) + 1, 
                                   stop=nchar(refs[i]))
        ref_Xpaths[i] <- paste0(sub(pattern=paste0("/", indentifier_name), 
                            replacement="", 
                            x=refs[i]),
                        paste0("[", if(ref_attribs[i]) "@", indentifier_name, 
                               "='", ref_identifiers[i], "']"))
      } else {
        ref_Xpaths[i] <- refs[i]
      }
    }
  }
  
  #### Backing up the scenario files
  d <- paste(dirname(path.scenarios), "Scenarios_bkup", sep="/")
  suppressWarnings(dir.create(path = d))
  file.copy(from = path.scenarios, to = d, recursive = TRUE, copy.date = TRUE)
  
  #### Processing modifications ####
  for(scenario in scenarios) {
    xml_scenario <- read_xml(file.path(path.scenarios, scenario))
    if(sum(!is.na(refs))) ref_nodes <- vector("list", length(node_paths))

    for(i in seq_along(node_paths)) {
      if(modes[i] == "add") {
        p <- xml_find_all(xml_scenario, xml_path(xml_parent(nodes[[i]])))
        xml_add_child(p, nodes[[i]])
      } else {
        if(modes[i] %in% c("before", "after", "replace")) {
          ref_nodes[[i]] <- xml_find_all(xml_scenario, ref_Xpaths[i]) 
          if(grepl(pattern = "Event", x = xml_path( ref_nodes[[i]]))) {
            ref_nodes[[i]] <- xml_parent( ref_nodes[[i]]) 
          }
          if(length(ref_nodes[[i]]) > 1) {
            stop(paste(
              "More than one node identified with the node path:", 
              refs[[i]], "in the file", scenario))
          }
          if(modes[i] %in% c("before", "after")) {
            xml_add_sibling(ref_nodes[[i]], nodes[[i]], .where=modes[i])
          } else {
            xml_replace(ref_nodes[[i]], nodes[[i]])
          }
        } else {
          if(modes[i] == "delete") {
            nodes[[i]] <- xml_find_all(xml_scenario, Xpaths[i])
            xml_remove(nodes[[i]])
          } else {
            stop(paste("Don't know what to do with mode", modes[i]))
          }
        } 
      }
    }
    write_xml(x = xml_scenario, file=file.path(path.scenarios, scenario))
  }
}

#' Modify the path of the workspace and grid file in all scenario files
#' 
#' When a HexSim workspace is copied to a new location, the path to the root of 
#' the workspace and the grid file that is stored in the scenario files need to 
#' be updated. When the simulations are run manually via the GUI, HexSim picks 
#' this up and asks the user to re-save the scenario. However, when simulations 
#' are run via command-line, an error occurs unless the path and the grid file 
#' exist. This function updates all the xml scenario files with the new path
#' passed with: \code{new.grid.path}.
#' 
#' \bold{WARNING:} Currently, xml scenario files saved by \code{HexSimR} trigger 
#' a request by HexSim to re-save the files. This is due to very minor 
#' differences in the file formatting, which do not affect the simulations. 
#' These files run with no problems when run via the command-line (i.e. 
#' batchRunner.exe)
#' 
#' @param new.grid.path The fully qualified (i.e. including the full path) of 
#'   the grid file, including extension
#' @inheritParams collate.census
#' @inheritParams scenarios.batch.modifier
#' @import xml2
#' @importFrom tcltk tk_choose.dir
#' @export
workspace.path.modifier <- function(
  path.scenarios=NULL,
  scenarios="all", 
  new.grid.path) {
  txt <- "Please, select the 'Scenarios' folder within the workspace"
  if(is.null(path.scenarios)) path.scenarios <- tk_choose.dir(caption=txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.files(path=path.scenarios, pattern=".xml$", 
                            full.names=FALSE, recursive=FALSE))
  for(scenario in scenarios) {
    xml_scenario <- read_xml(file.path(path.scenarios, scenario))
    workspace <- xml_find_all(xml_scenario, "/scenario/workspace")
    grid <- xml_find_all(xml_scenario, "/scenario/hexagonGrid/filename")
    xml_text(workspace) <- dirname(new.grid.path)
    xml_text(grid) <- new.grid.path
    write_xml(x = xml_scenario, file=file.path(path.scenarios, scenario))
  }
}
  