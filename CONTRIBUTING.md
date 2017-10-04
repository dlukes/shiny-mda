# Adding new functionality

Implement new functionality against data in the format returned by the function
`loadData()` in `load_data.R`:

```r
source("loadData.R")
data <- loadData("results/2017-05-18.RData")
# use `data` to prototype the new feature
```

A new feature should then consist of a separate `*.R` file with a function that
does whatever it is that the feature is supposed to do and returns the result.
This can then be easily integrated into the existing UI. If possible, **include
a short, runnable usage example** at the top of this file (see e.g.
`dim_graph.R` for inspiration).
