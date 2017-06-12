# Bookmarking

Kind of working right now, apart from the fact that datatable filtering gets
applied, but the UI doesn't reflect the filters. Which is probably a bug in
state restoration on the datatable side of things, not on my end, so maybe
report it...?

# DataTables

Maybe rewrite table coloring and other styling options, which are now in
`loadingsTable.js`, using the formatting helper functions in the `DT`
library...? (See [here](https://rstudio.github.io/DT/functions.html)).

(Though on the other hand, I might have to cycle repeatedly through the rows
in order to apply various different options controlled by different functions,
whereas with the callback, it's done in just one pass.)

See if built-in datatable column range filters can be made to filter outer
ranges instead of inner ranges.
