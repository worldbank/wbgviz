library(knitr)

knit_hooks$set(write.df = function(before, options, envir) {
  if (!before) {
    df <- get(options$write.df)
    # may need to create dir if this is the first time running
    if(!dir.exists(dirname(fig_path()))) dir.create(dirname(fig_path()))
    write.csv(df, fig_path("csv"), row.names=FALSE)
    return(paste0('\n\n[Download data](',fig_path("csv"),')'))
  }
})
