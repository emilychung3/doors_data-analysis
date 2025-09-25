get_subs <- function(version) {
  if (version == "pilot-ver1") {
      subs <- c("sub-29", "sub-30", "sub-31", "sub-32")
  } else if (version == "pilot-ver2") {
      subs <- c("sub-01", "sub-02", "sub-03", "sub-04", "sub-05", "sub-06", "sub-07", "sub-08")
  }
  else if (version == "data_sandpit") {
    subs <- c("sub-17", "sub-18", "sub-19", "sub-20", "sub-21", "sub-22")
  }
  } #"sub-01", "sub-02", "sub-03", "sub-04", "sub-05", "sub-06", "sub-07", "sub-08", "sub-09", "sub-10",
#"sub-11", "sub-12", "sub-13", "sub-14", "sub-15", "sub-16",