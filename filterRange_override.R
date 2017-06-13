# Monkey patch the filterRange() function in the DT package so that server-side filtering returns
# values *outside* the range instead of inside.

DT <- getNamespace("DT")
unlockBinding("filterRange", DT)

####################################################################################################
# This part of the code is deliberately kept as similar to the original as possible, in order to
# make potential updates easier. See https://github.com/rstudio/DT/blob/v0.2/R/shiny.R#L474.

# filter a numeric/date/time vector using the search string "lower ... upper"
filterRange = function(d, string) {
  if (!grepl('[.]{3}', string) || length(r <- strsplit(string, '[.]{3}')[[1]]) > 2)
    stop('The range of a numeric / date / time column must be of length 2')
  if (length(r) == 1) r = c(r, '')  # lower,
  r = gsub('^\\s+|\\s+$', '', r)
  r1 = r[1]; r2 = r[2]
  if (is.numeric(d)) {
    r1 = as.numeric(r1); r2 = as.numeric(r2)
  } else if (inherits(d, 'Date')) {
    if (r1 != '') r1 = as.Date(r1)
    if (r2 != '') r2 = as.Date(r2)
  } else {
    if (r1 != '') r1 = as.POSIXct(r1, tz = 'GMT', '%Y-%m-%dT%H:%M:%S')
    if (r2 != '') r2 = as.POSIXct(r2, tz = 'GMT', '%Y-%m-%dT%H:%M:%S')
  }
  if (r[1] == '') return(d <= r2)
  if (r[2] == '') return(d >= r1)
  d <= r1 | d >= r2
}

# End pastiche of original DT code.
####################################################################################################

DT$filterRange <- filterRange
lockBinding("filterRange", DT)