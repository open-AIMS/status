##' @title Display status in terminal
##' @return NULL
##' @author Murray Logan
##' @export
display_status_terminal <- function(dest = "term") {
    status_ <- read_status()
    system('clear')
    cat(" ")
    status_$settings$time$item <- get_current_time()
    write_status(status_)

    ## box_style <- cli:::box_styles()
    box_width <- 80
    box_margins <- 1

    ## get the width of the path box
    settings_box_nchar <-nchar(
      paste0(
        sapply(status_$settings, function(x) x$name),
        ": ",
        sapply(status_$settings, function(x) x$item)
      ))
    settings_box_width <- max(settings_box_nchar) +
        2 +              # add one for the status character
        box_margins*2    # add the left and right margin

    top <- outer_box_top(box_width, settings_box_width)

    ## Settings box
    settings_box_text <- settings_box(settings = status_$settings,
                                     box_width = settings_box_width,
                                     box_nchar = settings_box_nchar,
                                     box_margins = box_margins)

    ## Main box
    main_box_text <- main_box(status_$status,
      run_stages = status_$settings$run_stages$item,
      current_stage = status_$settings$current_stage$item,
      box_width,
      settings_box_width,
      box_margins)


    ## Outer box (bottom)
    bottom <- outer_box_bottom(box_width, settings_box_width)

    ## bottom <- paste0(box.style["double", "bottom_left"],
    ##                  strrep(box.style["double", "horizontal"], settings.box.width),
    ##                  '\u2567',
    ##                  strrep(box.style["double", "horizontal"], box.width - settings.box.width),
    ##                  box.style["double", "bottom_right"],
    ##                  "\n"
    ##                  )

    ## Combine boxes
    combined_boxes_text <- combined_boxes(
        top,
        settings_box_text,
        main_box_text,
        bottom,
        box_width,
        settings_box_width,
        box_margins)

    cat(combined_boxes_text)

    ## log box
    log.box <- log_box(box_width, box_margins)
    cat(log.box)
    if (dest != "term") {
        return(c(combined_boxes_text, log.box))
    }
}


outer_box_top <- function(outer_box_width, this_box_width) {
    top <- paste0("\u2554",
                  strrep("\u2550", this_box_width),
                  "\u2564",
                  strrep("\u2550", outer_box_width - this_box_width),
                  "\u2557",
                  "\n"
                  )
    top
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Settings box
##' @param settings
##' @param box_width
##' @param box_nchar
##' @param box_margins
##' @return string representing the settings box
##' @author Murray Logan
settings_box <- function(settings, box_width, box_nchar, box_margins) {
  box_text <- NULL
  keys <- names(settings)
  values <-  sapply(settings, function(x) x$item)
  names <-  sapply(settings, function(x) x$name)
  for (i in seq_along(keys)) {
    box_text <- c(box_text,
      paste0("\u2551",
        strrep(" ", box_margins),
        ## crayon::green(cli::symbol$star),
        "-",
        " ", crayon::blue(names[i]), ": ",
        crayon::white(values[i]),
        strrep(" ", box_width - (box_nchar[i]) - box_margins * 2 - 1),
        "\u2502",
        strrep(" ", box_margins)
      )
    )
  }
  box_text
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Main box
##' @param status
##' @param run_stages
##' @param current_stage
##' @param box_width
##' @param settings_box_width
##' @param box_margins
##' @return string representing the main status box
##' @author Murray Logan
main_box <- function(status, run_stages, current_stage, box_width, settings_box_width, box_margins) {
    main_box_text <- c("Sediment Quality Analysis Status", "")
    ## format the title to be centered
    for (i in 1:length(main_box_text))
        main_box_text[i] <- cli::ansi_align(main_box_text[i],
                                            width = box_width - settings_box_width - 1,
                                            align = 'center')

    ## add the stages as left justified
    for (j in 1:length(run_stages)) {
            main_box_text <- c(main_box_text,
                               cli::ansi_align(status[[run_stages[j]]]$title,
                                               width = box_width - settings_box_width - 1,
                                               align = 'left')
                               )
            if (length(status[[run_stages[[j]]]]$items) == 0) next
            for (i in 1:length(status[[run_stages[j]]]$items)) {
                if (run_stages[j] == current_stage | status[[run_stages[j]]]$status[i] == 'failure') {
                    main_box_text <- c(main_box_text,
                                       cli::ansi_align(
                                                paste0(strrep(" ", box_margins),
                                                       switch(status[[run_stages[j]]]$status[i],
                                                              'pending' = crayon::white(cli::symbol$line),
                                                              'progress' = crayon::magenta("/"), #crayon::magenta("\u23F1"),
                                                              'success' = crayon::green("√"), # crayon::green(cli::symbol$tick),
                                                              "warning" = crayon::magenta("!"),
                                                              'failure' = crayon::red("x") #crayon::red(cli::symbol$cross)
                                                              ),
                                                       " ", crayon::blue(status[[run_stages[j]]]$name[i])
                                                       ),
                                                width = box_width - settings_box_width - 1,
                                                align = 'left'
                                            )
                                       )
                }
        }
    }
    main_box_nchar <- nchar(main_box_text)
    main_box_text
}

outer_box_bottom <- function(outer_box_width, this_box_width) {
    bottom <- paste0("\u2560",
                     strrep("\u2550", this_box_width),
                     '\u2567',
                     strrep("\u2550", outer_box_width - this_box_width),
                     "\u2563",
                     "\n"
                     )
    bottom
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Combined boxes
##' @param top
##' @param settings_box_text
##' @param main_box_text
##' @param bottom
##' @param box_width
##' @param settings_box_width
##' @param box_margins
##' @return string representing the combined status boxes
##' @author Murray Logan
combined_boxes <- function(top, settings_box_text, main_box_text, bottom, box_width, settings_box_width, box_margins) {
    combined_text <- NULL
    for (i in 1:max(length(settings_box_text), length(main_box_text))) {
        combined_text <- c(combined_text,
                           paste0(
                               ifelse(i > length(settings_box_text),
                                      paste0("\u2551",
                                             cli::ansi_align("", width = settings_box_width, align = 'center'),
                                             "\u2502",
                                             strrep(" ", box_margins)),
                                      settings_box_text[i]),
                               ifelse(i > length(main_box_text),
                                      cli::ansi_align("", width = box_width - settings_box_width - 1, align = 'center'),
                                      main_box_text[i]),
                               "\u2551",
                               "\n"))
        }
    combined_text <- c(top,combined_text,bottom)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Log box
##' @param box_width
##' @param box_margins
##' @return a string representin the tail of the logs
##' @author Murray Logan
log_box <- function(box_width, box_margins) {
  if (file.exists(log_file)) {
    log <- system(paste0("tail -n 5 ", log_file), intern = TRUE)
  } else {
    log <- ""
  }
  pos <- max(stringr::str_locate(log, "\\|[^| ]*:\ ")[,"end"])
  log <- cli::ansi_strwrap(log,width = 80, exdent = pos)
  log_text <- paste0(
    cli::ansi_align(
      paste0(
        "\u2551",
        strrep(" ", box_margins),
        log),
      width = box_width + box_margins*2,
      align = "left"),
    "\u2551\n")
  log_text <- c("",log_text,
    paste0("\u255A", strrep("\u2550", box_width + 1), "\u255D\n")
  )
  log_text
}
