#' Batch modification of scenarios
#' 
#' Modify several scenarios at once by replacing, deleting or adding nodes (e.g.
#' events) in scenario .xml files.
#' 
#' To use this function some understanding of the xml file structure in HexSim 
#' is necessary. Briefly, each of the settings and parameter in HexSim is set by
#' a node in the xml file. Some nodes have a unique identifier, which can be an 
#' attribute (generally name="\emph{node_name}"), or a text element value (e.g. 
#' <name> \emph{node_name} </name>). To this end, identifiers are considered 
#' element that make the node unique. For example, accumulate events have all 
#' the same structure, but they can be identified by the value given to the node
#' <name>. On the contrary, there are node that are unique and can be simply 
#' identified by the node's name. One example of such node is 
#' <initializationSpatialData>, which is unique, so a search in the xml file for
#' the path: /scenario/population/initializationSpatialData will return one node
#' only.  See the tutorial if you need help with this.
#' 
#' If \code{path.scenarios=NULL} an interactive dialog box is used to select the
#' path where the scenario xml files are located.
#' 
#' If \code{scenarios="all"} (default), all scenarios are processed, otherwise 
#' it is possible to select a subset of scenarios using a character vector, e.g.
#' scenarios=c("scen1", "scen2"). \bold{TODO remove xml.template from list of 
#' scenarios}
#' 
#' \code{nodes} is used to pass the new nodes, which can be constructed by 
#' searching for a given node-path in a xml template file, in which case these 
#' paths are store in a .csv and the name of the .csv is passed to \code{nodes},
#' or can be manually constructed and passed as a list of nodes, or nodesets 
#' created with \code{\link[xml2]{read_cml}}. If a .csv is used, it must be 
#' located in the scenario folder and the name should be passed as character 
#' vector. The file must have the following headings: nodes, mode, ref, 
#' ref_identifier,	ref_attribute, identifier, and attribute. 
#' \code{scenarios.batch.modifier} will parse the file and use these columns as 
#' relevant arguments, so these do not need to be passed in the function call. 
#' If \code{nodes} is passed as a list, then clearly mode, ref, identifier, and 
#' attribute need to be passed. When a .csv file is used, the column with 
#' heading "nodes" is the path to the node to be searched in the xml template 
#' file. If the node has an identifier, then this is not part of the path, but 
#' it must be included in the column "identifier". The found nodes are then 
#' retrived and added or replaced to the scenario file. When the \code{mode} is 
#' "delete", the nodes are clearly searched in the scenario file and then 
#' deleted.
#' 
#' \code{mode} indicates the type of action that need to be performed. It has to
#' be of same length as \code{nodes}. If shorter, the last item is recycled. If 
#' "add" the node is added as last child of the parent node. If the node needs 
#' to be added in a specific position (events generally do), "before" and 
#' "after" can be used to indicate the position respect to the \code{ref} node 
#' (i.e. whether before or after the node \code{ref}. Note that the \code{ref} 
#' node is searched in the scenario file, not the xml.template. If the option 
#' "delete" is used, the node is deleted from the scenario file. When "same" is 
#' used, the node is assumed to have the same identifier as the one passed with 
#' \code{nodes} and it is replaced in the scenario file. If the identifier (e.g.
#' the name) is being changed, then "replace" must be used and the original node
#' in the scenario file need to be indicated. When \code{nodes} is a .csv, 
#' \code{ref} values is taken from the homonimous column.
#' 
#' \code{ref} needs to be passed when the options "before", "after" or "replace"
#' are used. When not relevant, \code{NA} is used. It has to be of same length 
#' as \code{nodes}. If shorter, the last item is recycled. When \code{nodes} is 
#' a .csv, \code{ref} values are taken from the homonimous column. When a ,csv 
#' file is passed to \code{nodes}, then a search is performed in the scenario 
#' xml file and the fields ref_identifier and	ref_attribute mustalso be passed
#' as for \code{nodes} to allow correct construction of the node path.
#' 
#' \code{identifier} is used to indicate whether the element node has an 
#' identifier. If it does, the name of the identifier needs to be passed, 
#' otherwise FALSE must be used. It has to be of same length as \code{nodes}. If
#' shorter, the last item is recycled. When \code{nodes} is a .csv, \code{ref} 
#' values is taken from the homonimous column.
#' 
#' \code{attribute} indicates whether the identifier is an attribute (TRUE) or 
#' not (FALSE).It has to be of same length as \code{nodes}. If shorter, the last
#' item is recycled. When \code{nodes} is a .csv, \code{ref} values is taken from
#' the homonimous column.
#' 
#' @param path.scenarios The path to the 'Scenarios' folder
#' @param xml.template The name of the xml file to use as template for the new 
#'   nodes
#' @param nodes The (new) nodes to be replaced, deleted or inserted in the xml 
#'   file
#' @param mode The mode in which the modification should operate (i.e. whether 
#'   to replace, delete or add). See details
#' @param ref The reference node in the original (i.e. the one to be modified) 
#'   xml file. See details
#' @param identifier The element node or attibute that is the identifier or 
#'   FALSE
#' @param attribute Whether the identifier of the node is an attribute or not
#' @inheritParams collate.census
#' @export
scenarios.batch.modifier <- function(
  path.scenarios=NULL,
  scenarios="all", 
  xml.template=NULL,
  csv.in,
  mode=c("add", "before", "after", "delete", "replace", "same"),
  ref=NA,
  identifier=FALSE,
  attribute=FALSE) {
  
  #### Setting arguments ####
  txt <- "Please, select the 'Scenarios' folder within the workspace"
  if(is.null(path.scenarios)) path.scenarios <- choose.dir(caption=txt)
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
  if(sum(is.na(ref))) ref_Xpaths <- vector("character", length(node_paths))
  if(!is.null(xml.template)) 
    xml_template <- read_xml(file.path(path.scenarios, xml.template), options="")
  
  for(i in seq_along(node_paths)) {
    
    if(!is.na(identifiers[i])) {
      last_slash <- gregexpr("/", node_paths[i])
      indentifier_name <- substr(node_paths[i], start=max(last_slash[[1]]) + 1, 
                                 stop=nchar(node_paths[i]))
      Xpaths[i] <- paste0(sub(pattern=paste0("/", indentifier_name), 
                          replacement="", 
                          x=node_paths[i]),
                      paste0("[", if(!is.na(attribs[i])) "@", indentifier_name, 
                             "='", identifiers[i], "']"))
    } else {
      Xpaths[i] <- node_paths[i]
    }
    
    if(modes[i] == "delete") next
    nodes[[i]] <- xml_find_all(xml_template, Xpaths[i])  
    if(xml_length(nodes[[i]]) > 1) {
      stop(paste(
        "More than one node identified with the node path:", 
        node_paths[[i]], "in the file", xml.template))
    }
    # ref Xpaths
    if(!is.na(ref[i])) {
      if(!is.na(ref_identifier[i])) {
        last_slash <- gregexpr("/", ref[i])
        indentifier_name <- substr(ref[i], start=max(last_slash[[1]]) + 1, 
                                   stop=nchar(ref[i]))
        ref_Xpaths[i] <- paste0(sub(pattern=paste0("/", indentifier_name), 
                            replacement="", 
                            x=ref[i]),
                        paste0("[", if(!is.na(attribs[i])) "@", indentifier_name, 
                               "='", ref_identifier[i], "']"))
      } else {
        ref_Xpaths[i] <- ref[i]
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
    if(sum(is.na(ref))) ref_nodes <- vector("list", length(node_paths))

    for(i in seq_along(node_paths)) {
      if(modes[i] == "add") {
        p <- xml_find_all(xml_scenario, xml_path(xml_parent(nodes[[i]])))
        xml_add_child(p, nodes[[i]])
      } else {
        if(modes[i] == "before" | "after" | "replace") {
          ref_nodes[[i]] <- xml_find_all(xml_scenario, ref_Xpaths[i])  
          if(xml_length(ref_nodes[[i]]) > 1) {
            stop(paste(
              "More than one node identified with the node path:", 
              ref[[i]], "in the file", scenario))
          }
          if(modes[i] == "before" | "after") {
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
    write_xml(x = xml_scenario, file=scenario)
  }
}