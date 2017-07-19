#' Build a Xpath
#' 
#' This function builds a Xpath to search nodes in HexSim xml files. It is used 
#' internally by hexSimR. When used in the LHS.scenarios function, it constructs
#' the path "/.../name[text()='...']" when param_node is not NA, 
#' param_identifier is FALSE and is.LHS is TRUE. Addtional details on arguments
#' are in the scenarios.batch.modifier.
#' 
#' @param node_path The node_path
#' @param identifier The identifier of the node
#' @param attrib Whether the identifier is an attribute
#' @param param_node Whether the parameter is contained in an internal node
#' @param is.LHS Whether the Xpath being built is for a LHS
#' @return A character vector with the Xpath
#' @export
make.Xpath <- function(node_path, identifier, attrib, 
                    param_node=NA, param_identifier=FALSE, is.LHS=FALSE) {
  last_slash <- gregexpr("/", node_path)
  if(is.na(param_node) & is.LHS & !is.na(as.logical(param_identifier))) {
    Xpath <- paste0(x=node_path, "[text()='", identifier, "']")
  } else {
    identifier_name <- substr(node_path, start=max(last_slash[[1]]) + 1, 
                               stop=nchar(node_path))
    Xpath <- paste0(sub(pattern=paste0("/", identifier_name), 
                        replacement="", 
                        x=node_path),
                    paste0("[", if(attrib) "@", identifier_name, 
                           "='", identifier, "']"))
  }
  return(Xpath)
}
#----------------------------------------------------------------------------#

#' Check conditions and pass the right arguments to make.Xpath
#' 
#' See scenarios.batch.modifier for addtional details on arguments
#' 
#' @param node_paths A character vector of node_paths
#' @param Xpaths A empty list of length = length(node_paths)
#' @param identifiers A character vector of node identifiers
#' @param attribs A logical vector as to whether the identifiers are attributes
#' @param param_nodes A character vector if the parameters are contained in 
#'   internal nodes, NA otherwise
#' @param param_node_identifiers A character vector if the param_nodes have
#'   identifiers, FALSE otherwise
#' @param param_identifiers A character vector if the params have identifiers
#'   (it is assumed these are attributes), FALSE otherwise
#' @param is.LHS A logical vector whether the Xpaths being built are for a LHS
#' @return A list (Xpaths) of character vectors with the Xpaths
#' @export
make.Xpaths <- function(node_paths, Xpaths, identifiers, attribs, 
                        param_nodes=NA, param_node_identifiers=FALSE,
                        param_node_attributes=FALSE,
                        param_identifiers=FALSE, is.LHS=FALSE) {
  
  suppressWarnings(
    if(is.na(param_nodes) & length(param_nodes) == 1) 
                                      param_nodes <- rep(NA, length(node_paths))
  )
  suppressWarnings(
    if(is.na(param_identifiers) & length(param_identifiers) == 1) 
                                param_identifiers <- rep(FALSE, length(node_paths))
  )
  suppressWarnings(
    if(is.na(param_node_identifiers) & length(param_node_identifiers) == 1) 
      param_node_identifiers <- rep(FALSE, length(node_paths))
  )
  suppressWarnings(
    if(is.na(param_node_attributes) & length(param_node_attributes) == 1) 
      param_node_attributes <- rep(FALSE, length(node_paths))
  )
  param_node_Xpaths <- vector("character", length(node_paths))
  
  for(i in seq_along(node_paths)) {
    if(is.na(as.logical(identifiers[i]))) {
      Xpaths[i] <- make.Xpath(node_paths[i], identifiers[i], attribs[i],
                              param_node=param_nodes[i], 
                              param_identifier=param_identifiers[i], 
                              is.LHS=TRUE)
      
      if(!is.na(param_nodes[i])) {
        if(is.na(as.logical(param_node_identifiers[i]))) {
          param_node_Xpaths[i] <- make.Xpath(param_nodes[i], 
                                             param_node_identifiers[i], 
                                             param_node_attributes[i],
                                             param_identifier=param_identifiers[i], 
                                             is.LHS=TRUE)
          
        } else {
          param_node_Xpaths[i] <- param_nodes[i]
        }
        
        Xpaths[i] <- paste0(Xpaths[i], param_node_Xpaths[i])
      }
      
    } else {
      Xpaths[i] <- node_paths[i]
    }
  }
  return(Xpaths)
}
#----------------------------------------------------------------------------#

#' Check whether the param is an attribute or a node
#' 
#' Check whether the param being changed by LHS.scenarios or 
#' xml.cond.replacement is an attribute or a node. This function is used 
#' internally by HexSimR.
#' 
#' @param attrib Whether the identifier is an attribute
#' @param param_node_identifier Whether param_node has an identifier
#' @param param_node_attribute Whether the param_node identifier is an attribute
#' @param param_identifier Whether the param has an identifier (it is assumed
#'   this is an attribute)
#' @return A logical vector with the Xpathof length=1
#' @export
is.attribs <- function(attrib, 
                       param_node_attribute, param_node_identifier, 
                       param_identifier){
  chk <- is.na(as.logical(param_identifier)) |
    (!is.na(as.logical(param_identifier)) & param_node_attribute) |
    (!is.na(as.logical(param_identifier)) & 
       !is.na(as.logical(param_node_identifier)) &
       attrib)
  return(chk)
}
#----------------------------------------------------------------------------#

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
#' these columns as arguments. See \code{system.file("extdata", "test_csv.csv", 
#' package="HexSimR")} for an example file.
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
#' are used in \bold{mode}. When not relevant, \code{NA} is used. When 
#' \bold{ref} is used, then a search is performed in the scenario xml file and 
#' the fields \bold{ref_identifier} and	\bold{ref_attribute} must also be passed
#' when relevant. \bold{ref_identifier} and	\bold{ref_attribute} have the same 
#' meaning as \bold{identifier} and \bold{attribute}, but they refer to the 
#' \bold{ref} node. Use FALSE when these are not relevant.
#' 
#' @param path.scenarios The path to the 'Scenarios' folder
#' @param xml.template The name of the xml file to use as template for the new 
#'   nodes
#' @param csv.in The name of the .csv file in the "Scenarios" folder with the
#'   information on the nodes that need to be modified (or checked in case of
#'   xml.cond.replacement)
#' @inheritParams collate.census
#' @import xml2
#' @export
scenarios.batch.modifier <- function(
  path.scenarios=NULL,
  scenarios="all", 
  xml.template=NULL,
  csv.in) {
  
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
  if(sum(!is.na(refs))) ref_Xpaths <- vector("character", length(node_paths))
  if(!is.null(xml.template)) 
    xml_template <- read_xml(file.path(path.scenarios, xml.template), options="")
  
  for(i in seq_along(node_paths)) {
    
    if(is.na(as.logical(identifiers[i]))) {
      Xpaths[i] <- make.Xpath(node_paths[i], identifiers[i], attribs[i])
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
      if(is.na(as.logical(ref_identifiers[i]))) {
        ref_Xpaths[i] <- make.Xpath (refs[i], ref_identifiers[i], ref_attribs[i])
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
#' @export
workspace.path.modifier <- function(
  path.scenarios=NULL,
  scenarios="all", 
  new.grid.path) {
  txt <- "Please, select the 'Scenarios' folder within the workspace"
  if(is.null(path.scenarios)) path.scenarios <- choose.dir(caption=txt)
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
  

#' Generate LHS scenarios
#' 
#' This function is used to generate scenarios whose parameter combinations 
#' follow a Latin Hypercupe Sampling design. Parameter values can be drawn from 
#' normal, lognormal, binomial, beta or uniform distributions or have a set of 
#' fixed values.
#' 
#' An xml file is passed (with \code{xml.template}) to build the LHS scenarios, 
#' which are saved in the same \code{path.scenarios} where the template is.
#' 
#' If \code{generate} is FALSE, then the function quits after generating the 
#' hypercube matrix.
#' 
#' An .csv file needs to be created, must be located in the scenario folder and 
#' the name should be passed with \code{csv.in} as a character vector (see 
#' \code{system.file("extdata", "test_csv_LHS.csv", package="HexSimR")} for an 
#' example).
#' 
#' The .csv must have the following headings: nodes, identifier, attribute, 
#' param_node, param_node_identifier, param_node_attribute, param_identifier, 
#' param_attribute, param_name, type, value, distribution. See documentation for
#' \code{scenarios.batch.modifier} on the meaning of nodes, identifier, 
#' attribute. When \code{generate=FALSE} only the last four are mandatory.
#' 
#' There might be situation where the parameter values to be changed is in an 
#' internal node respect to the node identifier. In order to identified uniquely
#' this parameter, the identifier of the parameter node needs to be indicated. 
#' This is best explained with an example. An accumulateTrait is identified by 
#' the name attribute (i.e. <accumulateTrait name="XXX">), however the parameter
#' values are contained in the <value> node within the accumulateTrait. The node
#' <value> is itself identified by a name attribute, but the parameters are 
#' stored under a "threshold" attribute. To avoid multiple hit \bold{nodes}, 
#' \bold{identifier} and \bold{attribute} are used to identify uniquely the 
#' parent node where the parameter is contained. 
#' \bold{param_node},	\bold{param_node_identifier}, and 
#' \bold{param_node_attribute} are used to identify the node where the parameter
#' values are contained (if necessary, use NA for \bold{param_node} and FALSE 
#' for the others columns if not relevant), and \bold{param_identifier} and 
#' \bold{param_attribute} are used to identify the actual parameter values that 
#' need to be changed. Use FALSE when the latter two are not relevant (e.g. if 
#' the node identifier is the parameter that needs to be changed).
#' 
#' \bold{type} refers to the type of parameter. It can take exactly one of the 
#' following: "integer", "numeric" or "character". \bold{value} refers to the 
#' parameters of the distribution from which values are drawn (separated by a 
#' comma) if one is used: mean and sd for normal, meanlog and sdlog for 
#' lognormal, shape1 and shape2 for beta, prob for binomial and min and max for 
#' uniform, otherwise a collection of values if \bold{distribution}="fixed". 
#' When \bold{distribution}="fixed" or \bold{type}="character" the elements in 
#' \bold{value} have equal probability.
#' 
#' @param samples The number of LHS samples (i.e. parameter combinations)
#' @param generate Whether generate (TRUE) the xml files or stop after having 
#'   created the hypercube matrix (FALSE)
#' @inheritParams scenarios.batch.modifier
#' @return A list where the first element is the hyercube matrix and the second
#'   are the nodes found in the template (if generate = TRUE)
#' @import xml2
#' @importFrom lhs randomLHS
#' @importFrom stats qbeta qbinom qlnorm qnorm qunif
#' @export
LHS.scenarios <- function(
  path.scenarios=NULL,
  xml.template=NULL,
  samples, 
  csv.in,
  generate=TRUE) {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  distribute <- list(
    uniform=function(p, argums) qunif(p, as.numeric(argums[1]), as.numeric(argums[2])),
    beta=function(p, argums) qbeta(p, as.numeric(argums[1]), as.numeric(argums[2])),
    binom=function(p, argums) qbinom(p, size=1, prob=as.numeric(argums[1])),
    lnorm=function(p, argums) qlnorm(p, as.numeric(argums[1]), as.numeric(argums[2])),
    norm=function(p, argums) qnorm(p, as.numeric(argums[1]), as.numeric(argums[2])), 
    fixed=function(p, argums) {
      br <- seq(0, by=1/length(argums), length.out=length(argums) + 1)
      p <- as.numeric(argums[cut(p, br, labels=FALSE)])
      return(p)
    }
  )
  #----------------------------------------------------------------------------#
  
  #### Setting arguments ####
  txt <- "Please, select the 'Scenarios' folder within the workspace"
  if(is.null(path.scenarios)) path.scenarios <- choose.dir(caption=txt)
  
  csv_file <- read.csv(file=file.path(path.scenarios, csv.in), 
                       stringsAsFactors=FALSE)
  pnames <- csv_file[, "param_name"]
  types <- csv_file[, "type"]
  values <- strsplit(csv_file[, "value"], ",")
  distrbs <- csv_file[, "distribution"]
  k <- nrow(csv_file)
  if(!is.null(xml.template)) {
    root_name <- substr(xml.template, start=1, stop=nchar(xml.template) - 4)
  } else {
    root_name <- NULL
  }
  
  #### write hypercube matrix ####
  hypercube <- as.data.frame(randomLHS(n=samples, k=k))
  
  for(i in seq_along(types)) {
    if(types[i] == "character") {
      br <- seq(0, by=1/length(values[[i]]), length.out=length(values[[i]]) + 1)
      hypercube[, i] <- values[[i]][cut(hypercube[, i], br, labels=FALSE)]
    } else {
      hypercube[, i] <- distribute[[distrbs[[i]]]](hypercube[, i], values[[i]])
      if(types[i] == "integer") hypercube[, i] <- round(hypercube[, i])
    }
  }
  names(hypercube) <- pnames
  write.csv(hypercube, row.names=FALSE,
            file=file.path(path.scenarios, 
                           paste0(root_name, 
                                  if(!is.null(xml.template)) "_", 
                                  "hypercube.csv")) 
            )
  
  #### generate xml files ####
  if(generate) {
    # Setting arguments
    node_paths <- csv_file[, "nodes"]
    identifiers <- csv_file[, "identifier"]
    attribs <- csv_file[, "attribute"]
    param_nodes <- csv_file[, "param_node"]
    param_node_identifiers <- csv_file[, "param_node_identifier"]
    param_node_attributes <- csv_file[, "param_node_attribute"]
    param_identifiers <- csv_file[, "param_identifier"]
    Xpaths <- vector("character", length(node_paths))
    
    # Create Xpaths
    Xpaths <- make.Xpaths(node_paths, Xpaths, identifiers, attribs, 
                            param_nodes=param_nodes, 
                            param_node_identifiers=param_node_identifiers,
                            param_node_attributes=param_node_attributes,
                            param_identifiers=param_identifiers, is.LHS=TRUE)
    
    xml_template <- read_xml(file.path(path.scenarios, xml.template), options="")
    
    #### Modify nodes ####
    nodes <- vector("list", length(node_paths))
    
    for(i in seq_along(node_paths)) {
      nodes[[i]] <- xml_find_all(xml_template, Xpaths[i])  
    }
    
    for(r in 1:samples) {
      for(i in seq_along(node_paths)) {
        if(is.attribs(attrib=attribs[i],
                      param_node_attribute=param_node_attributes[i], 
                      param_node_identifier=param_node_identifiers[i], 
                                  param_identifier=param_identifiers[i])
           ) {
          xml_attr(nodes[[i]], param_identifiers[i]) <- as.character(hypercube[r,i]) 
        } else {
          xml_text(nodes[[i]]) <- as.character(hypercube[r,i])
        }
      }
      
      write_xml(x=xml_template, file=file.path(path.scenarios, 
                                               paste0(root_name, "_LHS", r, ".xml")))
    }
  }
  return(list(hypercube=hypercube, if(generate) nodes=nodes))
}

#' Conditional replacement of xml elements
#' 
#' This function replaces values in specific element nodes of xml scenario files
#' if a condition is satisfied. This may be useful when, for example, a few 
#' resource maps have been used to generate scenarios for a LHS analysis. When 
#' this occurs, all the nodes (events) that use the resource map need to match 
#' the value in the node that was used to generate the LHS matrix. If the name 
#' of the map is unique, it may be possible to simply search and replace for a 
#' text string within all files. However, this approach will cause problem if 
#' the same string is used in different components of the xml file (for example,
#' if there is a trait that is named as the resource map is).
#' 
#' The file \code{csv.in}, which is constructed in the same way as in 
#' \code{LHS.scenarios}, identifies element nodes whose value needs to be 
#' satisfied. That is, a search is performed to identify a node and its values 
#' is compared to the value reported in the column 'value' in \code{csv.in}. For
#' each row in \code{csv.in}, a \code{lookup} csv file needs to be present, 
#' where a list of nodes that need to be modified and their new values is 
#' stored. These values are applied in the xml files where the condition in the 
#' relevant row of \code{csv.in} is met.
#' 
#' @inheritParams scenarios.batch.modifier
#' @param lookup character vector with the csv file names of the element values 
#'   to changed. One file for each row of csv.in
#' @param verbose Whether report to screen nodes found and changed (default: TRUE)   
#' @import xml2
#' @export
xml.cond.replacement <- function (path.scenarios=NULL,
                                  scenarios="all",
                                  csv.in,
                                  lookup,
                                  verbose=TRUE) {
  #### Setting arguments ####
  txt <- "Please, select the 'Scenarios' folder within the workspace"
  if(is.null(path.scenarios)) path.scenarios <- choose.dir(caption=txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.files(path=path.scenarios, pattern=".xml$", 
                            full.names=FALSE, recursive=FALSE))
  csv_file <- read.csv(file=file.path(path.scenarios, csv.in), 
                       stringsAsFactors=FALSE)
  
  node_paths <- csv_file[, "nodes"]
  identifiers <- csv_file[, "identifier"]
  attribs <- csv_file[, "attribute"]
  param_nodes <- csv_file[, "param_node"]
  param_node_identifiers <- csv_file[, "param_node_identifier"]
  param_node_attributes <- csv_file[, "param_node_attribute"]
  param_identifiers <- csv_file[, "param_identifier"]
  param_attributes <- csv_file[, "param_attribute"]
  Xpaths <- vector("character", length(node_paths))
  values <- csv_file[, "value"]
  
  # Create Xpaths
  Xpaths <- make.Xpaths(node_paths, Xpaths, identifiers, attribs, 
                        param_nodes=param_nodes, 
                        param_node_identifiers=param_node_identifiers,
                        param_node_attributes=param_node_attributes,
                        param_identifiers=param_identifiers, is.LHS=TRUE)
  
  for(scenario in scenarios) {
    xml_scenario <- read_xml(file.path(path.scenarios, scenario))
    nodes <- vector("list", length(node_paths))
    for(i in seq_along(node_paths)) {
      lookup_file <- read.csv(file=file.path(path.scenarios, lookup[i]), 
                              stringsAsFactors=FALSE)
      nodes[[i]] <- xml_find_all(xml_scenario, Xpaths[i])
      if(length(nodes[[i]]) > 0) {
        if(is.attribs(attrib=attribs[i],
                      param_node_attribute=param_node_attributes[i], 
                      param_node_identifier=param_node_identifiers[i], 
                      param_identifier=param_identifiers[i])
        ) {
          chk <-  xml_attr(nodes[[i]], param_identifiers[i]) 
        } else {
          chk <- xml_text(nodes[[i]])
        }
        if(chk == values[i]) {
          message(paste("Found a node for", Xpaths[i], "in", scenario))
          message(paste("The node value is", chk))
          rep_node_paths <- lookup_file[, "nodes"]
          rep_identifiers <- lookup_file[, "identifier"]
          rep_attribs <- lookup_file[, "attribute"]
          rep_param_nodes <- lookup_file[, "param_node"]
          rep_param_node_identifiers <- lookup_file[, "param_node_identifier"]
          rep_param_node_attributes <- lookup_file[, "param_node_attribute"]
          rep_param_identifiers <- lookup_file[, "param_identifier"]
          rep_Xpaths <- vector("character", length(rep_node_paths))
          rep_nodes <- vector("list", length(rep_node_paths))
          rep_values <- lookup_file[, "value"]
          
          # Create rep_Xpaths
          rep_Xpaths <- make.Xpaths(node_paths=rep_node_paths, 
                                    Xpaths=rep_Xpaths, 
                                    identifiers=rep_identifiers, 
                                    attribs=rep_attribs,
                                    param_nodes=rep_param_nodes, 
                                    param_identifiers=rep_param_identifiers, 
                                    param_node_identifiers=rep_param_node_identifiers,
                                    param_node_attributes=rep_param_node_attributes,
                                    is.LHS=TRUE)
          
          for(r in seq_along(rep_node_paths)) {
          rep_nodes[[r]] <- xml_find_all(xml_scenario, rep_Xpaths[r])
          
          if(is.attribs(attrib=rep_attribs[r],
                        param_node_attribute=rep_param_node_attributes[r], 
                        param_node_identifier=rep_param_node_identifiers[r], 
                        param_identifier=rep_param_identifiers[r])
             ) {
            if(verbose) {
            message(paste("Found node", rep_Xpaths[r], "in", scenario))
            message(paste("Replacing the value", 
                          xml_attr(rep_nodes[[r]], rep_param_identifiers[r]),
                          "with",
                          as.character(rep_values[r])))
            }
            xml_attr(rep_nodes[[r]], rep_param_identifiers[r]) <- as.character(rep_values[r]) 
          } else {
            if(verbose) {
            message(paste("Found node", rep_Xpaths[r], "in", scenario))
            message(paste("Replacing the value", 
                          xml_text(rep_nodes[[r]]),
                          "with",
                          as.character(rep_values[r])))
            }
            xml_text(rep_nodes[[r]]) <- as.character(rep_values[r])
          }
          }
          
        }
      }
    }
    write_xml(x = xml_scenario, file=file.path(path.scenarios, scenario))
  }
}

    
    
    


































