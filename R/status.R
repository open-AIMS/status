#' @keywords internal
##' @import cli
##' @import crayon
"_PACKAGE"

##' Initialize the settings required for status to operate
##'
##' Perform the following initialization steps:
##' - obtain a temp directory
##' - define a file in which to store the status of the project
##' - define a file in which to store the logfile of the project
##' - establish a status_ list that comprises:
##'   - settings - mainly paths and global constants
##'   - status - essentially a hierarchical task list
##' - populate the status element of the status_ list by scanning
##'   through the R scripts for the pertenant metadata
##' @title Initialize the status
##' @param pkgs a vector of package names to be scanned for status metadata
##' @param project_name a string representing the name of the project
##' @return NULL
##' @author Murray Logan
##' @export
status_initialize <- function(pkgs = NULL, project_name = "Sediment Quality Analysis Status", box_width = 80) {
        assign("box_width", box_width, envir = .GlobalEnv)
        assign("debug_mode", TRUE, envir = .GlobalEnv)
        assign("status_dir", tempdir(), envir = .GlobalEnv)
        assign("project_name", project_name, envir = .GlobalEnv)
        dir.create(status_dir)
        assign("status_file", paste0(status_dir, "/status.Rdata"), envir = .GlobalEnv)
        assign("log_file", paste0(status_dir, "/project.log"), envir = .GlobalEnv)
        assign("do_display", TRUE, envir = .GlobalEnv)
        assign("do_log", TRUE, envir = .GlobalEnv)
        if (file.exists(log_file)) unlink(log_file)
        ## Initial settings
        settings <- list(
          status_dir = list(item = status_dir, name = "Status directory"),
          status_file = list(item = basename(status_file), name = "Status filename"),
          log_file = list(item = basename(log_file), name = "Log filename"),
          time = list(item = get_current_time(), name = "Date/Time"),
          current_stage = list(item = 1, name = "Current stage"),
          current_item = list(item = "Initialising", name = "Current item")
        )

        status_ <- list(
          settings = settings,
          status = NULL
        )
        ## look through each of the *.R files in the project working
        ## directory and extract stages, items, and names and assign them
        ## all as pending
        files <- list.files(pattern = "*.R$")
        script_text <- NULL
        for (f in files) {
          script_text <- c(script_text, readLines((f)))
        }
        ## Now look through any suggested packages
        if (!is.null(pkgs)) {
          pkg_str <- lsf.str(envir = asNamespace(pkgs))
          pkg_str <- sapply(pkg_str, function(x)
            eval(parse(text = x), envir = asNamespace(pkgs)) |> deparse()
          )
          script_text <- c(script_text, paste(do.call("c", pkg_str), collapse = "\n"))
        }
        ## Exclude commented out lines
        script_text <- script_text[grep("^\\s*##.*", script_text, invert=TRUE)]
        status_$status <- parse_status(status_, script_text = script_text)
        status_$settings$run_stages <- list(item = names(status_$status), name = "Run stages")
        write_status(status_)
}


## merge_lists <- function(list1, list2) {
##   combined_list <- list2
##   if (length(list1) > 0) {
##     keys <- unique(c(names(list1), names(list2)))
##     combined_list <- setNames(mapply(c, list1[keys], list2[keys]), keys)
##   }
##   return(combined_list)
## }

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' This is essentially a copy of gtools::mixedsort
##' @title 
##' @param x 
##' @param decreasing 
##' @param na.last 
##' @param blank.last 
##' @param numeric.type 
##' @param roman.case 
##' @param scientific 
##' @return 
##' @author Murray
##' @export
status_mixedsort <- function (x, decreasing = FALSE, na.last = TRUE, blank.last = FALSE, 
                              numeric.type = c("decimal", "roman"), roman.case = c("upper", 
                                "lower", "both"), scientific = TRUE) 
{
  ord <- status_mixedorder(x, decreasing = decreasing, na.last = na.last, 
    blank.last = blank.last, numeric.type = numeric.type, 
    roman.case = roman.case, scientific = scientific)
  x[ord]
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' This is essentially a copy of gtools::mixedorder
##' @title 
##' @param x 
##' @param decreasing 
##' @param na.last 
##' @param blank.last 
##' @param numeric.type 
##' @param roman.case 
##' @param scientific 
##' @return 
##' @author Murray
##' @export
status_mixedorder <- function (x, decreasing = FALSE, na.last = TRUE, blank.last = FALSE, 
    numeric.type = c("decimal", "roman"), roman.case = c("upper", 
        "lower", "both"), scientific = TRUE) 
{
    numeric.type <- match.arg(numeric.type)
    roman.case <- match.arg(roman.case)
    if (length(x) < 1) {
        return(NULL)
    }
    else if (length(x) == 1) {
        return(1)
    }
    if (!is.character(x)) {
        return(order(x, decreasing = decreasing, na.last = na.last))
    }
    delim <- "\\$\\@\\$"
    if (numeric.type == "decimal") {
        if (scientific) {
            regex <- "((?:(?i)(?:[-+]?)(?:(?=[.]?[0123456789])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)(?:(?:[eE])(?:(?:[-+]?)(?:[0123456789]+))|)))"
        }
        else {
            regex <- "((?:(?i)(?:[-+]?)(?:(?=[.]?[0123456789])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)))"
        }
        numeric <- function(x) as.numeric(x)
    }
    else if (numeric.type == "roman") {
        regex <- switch(roman.case, both = "([IVXCLDMivxcldm]+)", 
            upper = "([IVXCLDM]+)", lower = "([ivxcldm]+)")
        numeric <- function(x) roman2int(x)
    }
    else {
        stop("Unknown value for numeric.type: ", numeric.type)
    }
    nonnumeric <- function(x) {
        ifelse(is.na(numeric(x)), toupper(x), NA)
    }
    x <- as.character(x)
    which.nas <- which(is.na(x))
    which.blanks <- which(x == "")
    delimited <- gsub(regex, paste(delim, "\\1", delim, sep = ""), 
        x, perl = TRUE)
    step1 <- strsplit(delimited, delim)
    step1 <- lapply(step1, function(x) x[x > ""])
    suppressWarnings(step1.numeric <- lapply(step1, numeric))
    suppressWarnings(step1.character <- lapply(step1, nonnumeric))
    maxelem <- max(sapply(step1, length))
    step1.numeric.t <- lapply(1:maxelem, function(i) {
        sapply(step1.numeric, function(x) x[i])
    })
    step1.character.t <- lapply(1:maxelem, function(i) {
        sapply(step1.character, function(x) x[i])
    })
    rank.numeric <- sapply(step1.numeric.t, rank)
    rank.character <- sapply(step1.character.t, function(x) as.numeric(factor(x)))
    rank.numeric[!is.na(rank.character)] <- 0
    rank.character <- t(t(rank.character) + apply(matrix(rank.numeric), 
        2, max, na.rm = TRUE))
    rank.overall <- ifelse(is.na(rank.character), rank.numeric, 
        rank.character)
    order.frame <- as.data.frame(rank.overall)
    if (length(which.nas) > 0) {
        if (is.na(na.last)) {
            order.frame[which.nas, ] <- NA
        }
        else if (na.last) {
            order.frame[which.nas, ] <- Inf
        }
        else {
            order.frame[which.nas, ] <- -Inf
        }
    }
    if (length(which.blanks) > 0) {
        if (is.na(blank.last)) {
            order.frame[which.blanks, ] <- NA
        }
        else if (blank.last) {
            order.frame[which.blanks, ] <- 1e+99
        }
        else {
            order.frame[which.blanks, ] <- -1e+99
        }
    }
    order.frame <- as.list(order.frame)
    order.frame$decreasing <- decreasing
    order.frame$na.last <- NA
    retval <- do.call("order", order.frame)
    return(retval)
}



##' Scan through all the R scripts in the current working director
##' and extract the `status::status_try_catch` and blocks from which
##' to extract the general structure of the status hierarchy.
##'
##' Construct a hierarchical status/task list via the following:
##' - isolate `status_try_catch` blocks
##' - search for tokens (stages, items and names) and initialize
##'   status as 'pending'
##' - if a status element in the `status_` list does not already exist
##'   (which it should), create it
##' - append the extracted tokens to the current task list hierarchy
##' - isolate `status_set_stage` blocks
##' - search for tokens (stage and title) and use these to provide
##'   names for the top level hierarchy
##' @title Parse status
##' @param status_ a status_ list
##' @param script_text a [readLine] object
##' @return list element
##' @author Murray Logan
parse_status <- function(status_, script_text = "") {
  script <- paste(script_text, collapse = "\n")
  ## Status blocks
  status_blocks <-
    regmatches(
      script,
      gregexpr("status_try_catch\\s*\\(\\s*\\{.*\\}.*?_?stage_.*\\)", script)
    )[[1]]
  status_blocks

  status_stage <- gsub(".*_?stage_?\\s*=\\s*([^,]*).*", "\\1", status_blocks)
  status_item <- gsub(".*_?item_?\\s*=\\s*([^,\\)\n]*).*", "\\1", status_blocks)
  status_item <- gsub("\"", "", status_item)
  ## status_item <- gsub('.*_item\\s*=\\s*\"([^\"]*)\".*', "\\1", status_blocks)
  status_name <- gsub(".*_?name_?\\s*=\\s*([^,\\)\n]*).*", "\\1", status_blocks)
  status_name <- gsub("\"", "", status_name)
  status_order_wch <- grep("order", status_blocks)
  status_order <- rep(NA, length(status_blocks))
  if (any(!is.na(status_order_wch))) {
    status_order[status_order_wch] <- gsub(
      ".*_?order_?\\s*=\\s*([^,\\)\n]*).*", "\\1",
      status_blocks[status_order_wch]
    )
    status_order[status_order_wch] <- gsub(
      "\"", "",
      status_order[status_order_wch]
    )
  }
  status_status <- rep("pending", length(status_name))

  if (is.null(status_$status)) {
    status_$status <- vector("list", length(unique(status_stage)))
    names(status_$status) <- unique(status_stage)
  }

  status_$status <-
    lapply(status_mixedsort(unique(c(names(status_$status), status_stage))), function(x) {
      ord <- status_mixedorder(status_order[status_stage == x])
      list(
        stages = c(status_$status[[x]]$stages, status_stage[status_stage == x][ord]),
        names = c(status_$status[[x]]$names, status_name[status_stage == x][ord]),
        items = c(status_$status[[x]]$items, status_item[status_stage == x][ord]),
        status = c(status_$status[[x]]$status, status_status[status_stage == x][ord])
      )
    })

  ## status_$status <-
  ##   ## sapply(unique(status_stage), function(x) {
  ##   lapply(sort(unique(c(names(status_$status), status_stage))), function(x) {
  ##     ## list(name = c(x$name, status_name[status_stage == as.numeric(names(x))]))
  ##     list(
  ##       stages = c(status_$status[[x]]$stages, status_stage[status_stage == x]),
  ##       names = c(status_$status[[x]]$names, status_name[status_stage == x]),
  ##       items = c(status_$status[[x]]$items, status_item[status_stage == x]),
  ##       status = c(status_$status[[x]]$status, status_status[status_stage == x])
  ##     )
  ##   })

  status_$status <- setNames(status_$status, sapply(status_$status, function(x) unique(x$stages)))

  ## See if there are any status titles that can be applied
  status_title_blocks <-
    regmatches(
      script,
      gregexpr("status_set_stage\\s*\\(.*?\\)", script)
    )[[1]]
  status_title_blocks <- unique(status_title_blocks)

  status_stage <- gsub(".*stage\\s*=\\s*([^,]*).*", "\\1", status_title_blocks)
  status_title <- gsub(".*title\\s*=\\s*([^,\\)\n]*).*", "\\1", status_title_blocks)
  status_title <- paste0("Stage ", status_stage, " - ", gsub("\"", "", status_title))

  all_names <- sort(unique(c(names(status_$status), status_stage)))
  status_$status <-
    lapply(all_names, function(x) {
    status_$status[[x]]$title <- status_title[status_stage == x]
    ## setNames(status_$status[x], status_stage[status_stage == x])
    status_$status[[x]]
  })
  ## status_$status <- setNames(status_$status, sapply(status_$status, function(x) unique(x$stages)))
  status_$status <- setNames(status_$status, all_names)
  return(status_$status)
}


read_status <- function() return(readRDS(status_file))

write_status <- function(status_) {
  assign("status_", status_, env = globalenv())
  saveRDS(status_, file = status_file)
}

get_current_time <- function() {
  format(Sys.time(), "%d/%m/%Y %H:%M:%S")
}


##' Get the item of a setting
##'
##' Return the item of a setting element
##' @title Get the item of a setting
##' @param element the name of the element to return
##' @return item of a setting element
##' @author Murray
##' @export
get_setting <- function(element) {
  status_ <- read_status()
  return(status_$settings[[element]]$item)
}

##' Get the current stage
##'
##' Return the current analysis stage
##' @title Get the current stage
##' @return current analysis stage
##' @author Murray
##' @export
get_current_stage <- function() {
  status_ <- read_status()
  return(status_$settings$current_stage$item)
}

##' Set the current stage
##'
##' Set the current analysis stage
##' @title Set the analysis stage (a number)
##' @param stage a numeric value representing the current analysis stage
##' @param title a string representing a descriptive title to use for the stage title
##' @return NULL
##' @author Murray Logan
##' @export
status_set_stage <- function(stage, title) {
  if (!exists("status_file")) {
          return()
  } else {
          if (!file.exists(status_file)) {
            return()
          }
  }
  status_ <- read_status()
  status_$settings$current_stage$item <- stage
  write_status(status_)
}


##########################################################################
## The following function provides a more useful error handling         ##
## routine.                                                             ##
##    expr:      an R expression to be evaluated                        ##
##    logFile:   a character string represetnation of the log file name ##
##               (including path relative to the current working        ##
##               directory)                                             ##
##    Category:  a character string representation of error category    ##
##    msg:       a character string with a message to appear verbatim   ##
##               in the log                                             ##
##    return:    boolean, whether to return a TRUE or FALSE             ##
##    progressive: denotes whether we have a finished extracting a      ##
##               dataset (FALSE) or further steps remain (TRUE) -       ##
##               if FALSE, appends filesize to console status           ##
##########################################################################

##' Awaiting a description
##'
##' Update status_ with one of:
##' - 'pending' - awaiting action
##' - 'progress' - in progress
##' - 'success' - successfully complete
##' - 'failure' - failed
##' @title Status try catch
##' @param expr a block of code to be evaluated
##' @param stage_ a numeric value representing the current analysis stage
##' @param name_ name of the current task
##' @param item_ id of the current task
##' @return NULL
##' @author Murray Logan
##' @export
status_try_catch_old <- function(expr, stage_, name_, item_) {
  status_ <- read_status()
  ## MMP_tryCatch <- function(expr, logFile, item, Category, expectedClass=NULL, msg=NULL, return=NULL, showWarnings=FALSE) {
  ## if (!exists('PROGRESS')) PROGRESS=NULL
  max_warnings <- 10
  warnings <- 0
  W <- NULL
  w.handler <- function(w) { # warning handler
    print(w)
    if (class(w)[[1]] %in% c("simpleWarning", "warning", "condition", "rlang_warning")) { ## if a single list (one warning)
      m <- w$message[grepl("WARNING", w$message)]
    } else {  ## if a list of lists (multiple warnings)
      m <-
        sapply(w, function(x) {
          print(x$message)
          x$message[grepl("WARNING", x$message, ignore.case = FALSE)]
        }, simplify = TRUE) |>
        unlist()
    }
    print("here")
    ## If the warning is one that I have issued
    if ((warnings < max_warnings) && any(grepl('WARNING', m)>0)) {
      status_log('WARNING', log_file, stage_, paste0(name_, ": ", warnings+1, ". ", gsub("WARNING:", "", m)))
      update_status_status(stage = status_$settings$current_stage$item, item = item_, status = "warning")
      warnings <<- warnings + 1
    } else {  ## if the warnings are not generated specifically by me
      status_log('SUCCESS', log_file, stage_, name_)
      update_status_status(stage = status_$settings$current_stage$item, item = item_, status = "success")
    }
    print("there")
    print(stage_)
    print(name_)
     display_status_terminal()
     invokeRestart("muffleWarning")
  }
  ## ret <- list(value = withCallingHandlers(tryCatch(expr,
  ##                                                  error = function(e) e,
  ##                                                  warning = function(w) w,
  ##                                                  message = function(m) m),
  ##                                         warning = w.handler),warning = W)

  ret <- list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                          warning = w.handler), warning = W)
  ## if(!is.atomic(ret$value) && !is.null(ret$value$message)){
  ## print(!is.atomic(ret$value))
  ## print(any(class(ret$value) %in% c("simpleError", "error", "rlang_error")))
  if(!is.atomic(ret$value) && any(class(ret$value) %in% c("simpleError", "error", "rlang_error"))){
    ## An error occurred
    ## PROGRESS <<- c(PROGRESS,'Fail')
    class(ret) <- "try-error"
    status_log('ERROR', log_file, stage_, paste(name_, ret$value$message))
    update_status_status(stage = status_$settings$current_stage$item, item = item_, status = "failure")
    if(!is.null(return)) {
      FALSE
    }else {
      if (debug_mode) {
        "An error occured, please refer to the status line above..."
      } else {
        quit(status = -1,save = "no")
      }
    }
  } else {    #no error check for warning
    ## PROGRESS <<- c(PROGRESS,'Pass')
    status_log('SUCCESS', log_file, stage_, name_)
    update_status_status(stage = status_$settings$current_stage$item, item = item_, status = "success")
    if(!is.null(return)) {
      TRUE
    }
  }
  display_status_terminal()
  return(ret$value)
}

##' Awaiting a description
##'
##' Update status_ with one of:
##' - 'pending' - awaiting action
##' - 'progress' - in progress
##' - 'success' - successfully complete
##' - 'failure' - failed
##' @title Status try catch
##' @param expr a block of code to be evaluated
##' @param stage_ a numeric value representing the current analysis stage
##' @param name_ name of the current task
##' @param item_ id of the current task
##' @return NULL
##' @author Murray Logan
##' @export
status_try_catch <- function(exp, stage_, name_, item_, order_, sub_ = NULL) {
  status <- exists("status_file")
  if (status) status_ <- read_status()
  max_warnings <- 10
  nwarnings <- 0
  ## if it exists
  ## 1. duplicate item
  ## 2. delete item
  ## If it doesnt exist
  ## 1. add item
  if (!is.null(sub_)) {
    if (item_ %in% status_$status[[stage_]]$items) {
      status::duplicate_status_item(
        stage = stage_, order = "current",
        item = abbreviate(sub_),
        name = paste0(" (", abbreviate(sub_),")"),
        original_item = item_
      )
      status::remove_status_item(stage = stage_, item = item_) 
    } else {
      status::add_status_item(
        stage = stage_, order = NULL,
        item = paste0(item_, "_", abbreviate(sub_)),
        name = paste0(name_, " (", abbreviate(sub_), ")")
      )
    }
    item_ <- paste0(item_, "_", abbreviate(sub_))
  }
  update_setting(element = "current_item", item = item_)
  
  tryCatch.W.E <- function(expr)
  {
    W <- NULL
    w.handler <- function(w) { # warning handler
      W <<- w
      invokeRestart("muffleWarning")
    }
    list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
      warning = w.handler),
      warning = W)
  }
  ret <- tryCatch.W.E(exp)
  ## if there are warnings
  if (!is.null(ret$warning)) {
    m <- ret$warning$message
    ## m <- str_replace(m, "\\n$", "")
    ## Only if it is an internal (by my code) warning
    if (any(grepl("WARNING", m))) {
      if (status) update_status_status(
        stage = status_$settings$current_stage$item,
        item = item_, status = "warning"
      )
      if (status & do_log) status_log("WARNING", log_file, stage_, paste0(
        name_, ": ",
        gsub("WARNING:", "", m)
      ))
    } else {  ## If the warning is externally generated - for dev only
      if (status & do_log) status_log("WARNING", log_file, stage_, paste0(
        name_, ": ",
        ## nwarnings + 1, ".",
        paste0(m, ".  This warning is for developers only")
      ))
      
    }
    nwarnings <<- nwarnings + 1
  }
  ## if there are errors
  if(!is.atomic(ret$value) && any(class(ret$value) %in% c("simpleError", "error", "rlang_error"))){
    class(ret) <- "try-error"
    ## if (status) status_log('ERROR', log_file, stage_, paste(name_, ret$value$message))
    mess <- gsub("\033\\[[0-9;]*[mK]", "", rlang::cnd_message(ret$value))
    trace_string <- paste(capture.output(print(ret$value$parent$trace)), collapse = "\n")
    mess <- paste(mess, trace_string, sep = "\n")
    if (status & do_log) status_log('ERROR', log_file, stage_, paste(name_, mess))
    if (status) update_status_status(
      stage = status_$settings$current_stage$item,
      item = item_, status = "failure"
    )
    if(!is.null(return)) {
      FALSE
    }else {
      if (debug_mode) {
        "An error occured, please refer to the status line above..."
      } else {
        quit(status = -1,save = "no")
      }
    }
  } else {
    go <- TRUE
    if (!is.null(ret$warning)) { # if there are warnings
      if (any(grepl("WARNING", ret$warning$message))) { # if the warnings are internal
        go <- FALSE
      }
    }
    if (go) {
      if (status & do_log) status_log("SUCCESS", log_file, stage_, name_)
      if (status) update_status_status(
        stage = status_$settings$current_stage$item,
        item = item_, status = "success"
      )
    }
    if(!is.null(return)) {
      TRUE
    }
  }
  if (status & do_display) display_status_terminal()
  return(ret$value)
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param stage 
##' @param order 
##' @param item 
##' @param name 
##' @param original_item 
##' @return 
##' @author Murray
##' @export
duplicate_status_item <- function(stage, order, item, name, original_item) {
  status <- exists("status_file")
  if (status) status_ <- read_status()
  wch_current <- which(status_$status[[stage]]$items == status_$settings$current_item$item)
  wch <- which(status_$status[[stage]]$items == original_item)
  original_status <- status_$status[[stage]]$status[wch]
  order <- ifelse(order == "current", wch_current, wch) ## status_$status[[stage]]$status[wch]
  item <- paste0(original_item, "_", item)
  name <- paste0(status_$status[[stage]]$names[wch], " ", name)
  status::add_status_item(
    stage = stage,
    order = order,
    item = item,
    name = name,
    status = original_status)
  update_setting(element = "current_item", item = item)
  if (status & do_display) display_status_terminal()
}

##' Added a status item
##'
##' Append a status item to the status_ list
##' @title Add status item
##' @param stage integer representing the analysis stage
##' @param item character - a status item key
##' @param name character - the name of the status item
##' @return NULL
##' @author Murray Logan
##' @export
add_status_item <- function(stage, order = NULL, item, name, status = "pending") {
  status_ <- read_status()

  ## discover what the minimum order of an pending item is
  ## wch_order <- min(c(length(status_$status[[1]]$status),
  ##  which(status_$status[[stage]]$status == "pending"))) - 1
  wch_order <- which(status_$status[[stage]]$items == status_$settings$current_item$item)
  if (!is.null(order)) wch_order <- order
  ## need to slot the current item in before that

  status_$status[[stage]]$status
  status_$status[[stage]]$stages <- append(status_$status[[stage]]$stages,
    stage,
    after = wch_order
  )
  status_$status[[stage]]$names <- append(
    status_$status[[stage]]$names,
    name,
    after = wch_order
  )
  status_$status[[stage]]$items <- append(
    status_$status[[stage]]$items,
    item,
    after = wch_order
  )
  status_$status[[stage]]$status <- append(
    status_$status[[stage]]$status,
    status,
    after = wch_order
  )
  write_status(status_)
}
##' Remove status item
##'
##' Remove a status item from the status_ list
##' @title Remove status item 
##' @param stage integer representing the analysis stage 
##' @param item character - a status item key 
##' @return NULL 
##' @author Murray
##' @export
remove_status_item <- function(stage, item) {
  status_ <- read_status()
  wch <- which(status_$status[[stage]]$item == item)
  status_$status[[stage]]$stages <- status_$status[[stage]]$stages[-wch]
  status_$status[[stage]]$names <- status_$status[[stage]]$names[-wch]
  status_$status[[stage]]$items <- status_$status[[stage]]$items[-wch]
  status_$status[[stage]]$status <- status_$status[[stage]]$status[-wch]
  write_status(status_)
}

##' Update the status of status items
##'
##' Update the status of the status items
##' @title Update the status' status field
##' @param stage integer representing the analysis stage
##' @param item character - a status item key
##' @param status character - the items status
##' @return NULL
##' @author Murray Logan
update_status_status <- function(stage, item, status) {
  status_ <- read_status()
  status_$status[[stage]]$status[which(status_$status[[stage]]$item == item)] <- status
  write_status(status_)
}
##' Update status name
##'
##' Update the name of a status item
##' @title Update the status' name field
##' @param stage integer representing the analysis stage
##' @param item character - a status item key
##' @param name character - a status item name
##' @return NULL
##' @author Murray Logan
update_status_name <- function(stage, item, name) {
  status_ <- read_status()
  status_$status[[stage]]$names[which(status_$status[[stage]]$item == item)] <- name
  write_status(status_)
}
##' Add a status setting item
##'
##' Add a status setting item
##' @title Add setting
##' @param element character representing the element to add to the 
##' @param item character - a status item key
##' @param name character - a status item name
##' @return NULL
##' @author Murray Logan
##' @export
add_setting <- function(element, item, name) {
  status_ <- read_status()
  tmplist <- list(list(item = item, name = name))
  tmplist <- setNames(tmplist, element)
  status_$settings <-
    append(status_$settings, values = tmplist)
  write_status(status_)
}

##' Update a status setting item
##'
##' Update a status setting item
##' @title Update setting
##' @param element character representing the element to add to the 
##' @param item character - a status item key
##' @return NULL
##' @author Murray Logan
##' @export
update_setting <- function(element, item) {
  status_ <- read_status()
  status_$settings[[element]]$item <- item
  write_status(status_)
}
####################################################################################
## The following function writes out log information to a file named by the       ##
## LOG_FILE global variable.                                                      ##
## Arguments:                                                                     ##
## - log_status:     a string indicating either 'FAILURE',  'SUCCESS',            ##
##              'WARNING' or 'INFO                                                ##
## - logFile:    a character string representation of the log file name           ##
##               (including path relative to the current working director)        ##
## - Category:   a character string with a category to appear verbatim in the log ##
## - success:    boolean or string. One of TRUE (for success), 'WARNING'          ##
##               (for warnings) or anything else for a failure                    ##
## - msg:        the message (as a string) to appear verbatim in the log          ##
####################################################################################

##' Write the status to log_file
##'
##' Write the status to log_file.  If there is a status setting called
##' "write_external_log", then this will be called as a function to
##' write the log to an external file (which may be on a remote server).
##' @title Write status log
##' @param status the status of the task
##' @param log_file the full path to the log file
##' @param Category the overal category of the task
##' @param msg the specific message associated with the log
##' @return NULL
##' @author Murray Logan
status_log <- function(status, log_file = log_file, Category, msg=NULL) {
    d <- dirname(log_file)
    files <- list.files(d)
    ## print(files)
    if(!any(grepl(paste0('^',log_file,'$'),files))) system(paste0('touch "',log_file,'"'))
    now <- Sys.time()
    options(digits.secs=2)              ## switch to subsecond display
    msg = paste0(format(now), "|", status, ": ", paste0("Stage ", Category), " - ", msg)
    ## cat(paste0(msg,'\n'))
    if (!is.null(msg)) {
        write(msg,  file=paste0(log_file), append=TRUE)
        if (!is.null(get_setting(element = "write_external_log"))) {
         fn <- get_setting(element = "write_external_log")
         fn()
        }
    }
}

