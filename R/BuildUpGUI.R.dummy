# Building the GUI and/or provide methods for it
# package: gWidget, RGtk2,gWidgetsGtk2


# Some example code for a simple gui

library(gWidgets)
library(gWidgetsGtk2) #gWidgetsrJava , gWidgetsGtk2, gWidgetstcltk
require("RGtk2")


# GUI Behnaz 
win2<-gtkWindow("hallo")
frame<-gtkFrameNew("test")
win2$add(frame)















win <- gwindow("Tab delimited file upload example")

grp_name <- ggroup(container = win)

lbl_data_frame_name <- glabel(
  "Variable to save data to: ",
  container = grp_name
)

txt_data_frame_name <- gedit("dfr", container = grp_name)

grp_upload <- ggroup(container = win)

btn_upload <- gbutton(
  text      = "Upload tab delimited file",
  container = grp_upload,
  handler   = function(h, ...)
  {
    gfile(
      text    = "Upload tab delimited file",
      type    = "open",
      action  = ifelse(svalue(chk_eurostyle), "read.delim2", "read.delim"),
      handler = function(h, ...)
      {
        tryCatch(
          {
            data_frame_name <- make.names(svalue(txt_data_frame_name))
            the_data <- do.call(h$action, list(h$file))
            assign(data_frame_name, the_data, envir = globalenv())
            svalue(status_bar) <-
              paste(nrow(the_data), "records saved to variable", data_frame_name)
          },
          error = function(e) svalue(status_bar) <- "Could not upload data"
        )
      },
      filter = list(
        "Tab delimited" = list(patterns = c("*.txt","*.dlm","*.tab")),
        "All files" = list(patterns = c("*"))
      )
    )
  }
)


use_comma_for_decimal <- function()
{
  unname(Sys.localeconv()["decimal_point"] == ",")
}

chk_eurostyle <- gcheckbox(
  text      = "Use comma for decimal place",
  checked   = use_comma_for_decimal(),
  container = grp_upload
)

status_bar <- gstatusbar("", container = win)


lmwidget <- ggenericwidget(lm)
