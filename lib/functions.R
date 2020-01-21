


#################################################################################
# usage - add '-sadgg' to the end of ggplot string to store the plot in a directory
# sadgg
#################################################################################

library(janitor)
`-.gg` <- function(e1, e2) e2(e1)

sadgg <- function ( plot ) {
  base_dir='~/ggplots/'
  filetype='svg'
  if( !dir.exists( base_dir ) ) {
    dir.create( base_dir )
  }

  list.files( base_dir ) %>%
    as.tibble() %>%
    separate(value, into='n', sep="_", extra='drop') %>%
    mutate( n=as.numeric(n) ) %>%
    filter( !is.na(n)) %>%
    summarise( maxn=max(n)) %>%
    pluck('maxn') %>%
    { . } -> maxn

  if (maxn==-Inf) { maxn=0}

  plot %>%
    pluck('mapping') %>%
    paste( collapse = '_') %>%
    make_clean_names() %>%
    { . } -> filename_base

  filename = paste0( sprintf( "%03d_", maxn+1),
                      filename_base,
                    '.', filetype)

  ggsave( filename, plot, device=filetype, path=base_dir)
  print(plot)
  #svgPanZoom( plot )

  #    if( system2('pidof', args='geeqie', stdout='/dev/null') ) {
  system2( 'geeqie', args=c( '-r', '-t',  paste0( 'file:', base_dir, filename)), wait=FALSE)
  #system2( 'firefox', args=c( paste0(  base_dir, filename)), wait=FALSE)
  #system2( 'display', args=c( paste0(  base_dir, filename)), wait=FALSE)
  #    }
}
mView = function( df ) {
  f = tempfile(fileext='.csv')
  write_excel_csv(df, f)

  system(paste('gnumeric', f), 
         intern=TRUE,
         wait=FALSE,
         ignore.stderr=TRUE,
  ignore.stdout=TRUE
  )

}



mkpct = function( x ) {
  round(x * 100, 2  )
}
