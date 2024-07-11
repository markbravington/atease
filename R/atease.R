# This is package atease 

".onAttach" <-
function( libname, pkgname){
  # Ready for future where @ and @<- are generic *and actually work :/ *
  if( FALSE && (getRversion() >= '4.3')){
    rm( list= c( '@', '@<-'), 
        envir=as.environment( sprintf( 'package:%s', pkgname)))
  }
}


".onLoad" <-
function( libname, pkgname){
r"--{
In ~R4.3, @ became generic. Under some circs that will be found before atease::@, even after library(atease), so that the latter won't work. One example is knitr::knit() [actually knitr::eng_r()]; it's arguably a bug in the latter (or in evaluate::evaluate) but I doubt I would win the argument!
I could just register atease::@ as a default method for @ in the NAMESPACE file, but in previous versions of R that wouldn't work because there'd be no generic. So for now, I hack in hope... don't think this is working yet, though.
One major problem (for me; no doubt a minor arthropoidal feature from R's PoV) is that '@.default' does not get used--- instead, R whines about "no applicable method for class "integer"" etc. FFS.
}--"

  if( FALSE && (getRversion() >= '4.3') && 
      exists( '@', baseenv(), mode='function') && 
      is.primitive( base::'@')){
    ns <- asNamespace( 'atease')
    # From ?typeof via ?class
    types <- c( "default", "logical", "integer", "double", "complex", 
        "character", "raw", "list") 
    for( itype in types){
      registerS3method( '@', itype, ns$'@', baseenv())
      registerS3method( '@<-', itype, ns$'@<-', baseenv())      
    }
    # rs3m can take an actual function rather than a name
    # registerS3method( '@', 'default', ns$'@', baseenv())
    # registerS3method( '@<-', 'default', ns$'@<-', baseenv())
  }
NULL
}


"@" <-
function( object, name){
  if( isS4( object) && (getRversion() < '4.3')) {
    mc <- match.call()  
    mc[[1]] <- baseenv()$'@'
return( eval( mc, parent.frame())) # eval.parent() doesn't debug well
  } else
return( attr( object, as.character( substitute( name)))) # as.character( mc$name)))
}


"@<-" <-
function( object, name, value){
  arg <- substitute(name)
  if (is.name(arg)) {
    name <- as.character(arg)
  }
  if( isS4( object) && (getRversion() < '4.3')) {
    if( getRversion() >= '3') {
      `@<-`( object, name, value)
    } else {
      # anti cmd check
      # `slot<-`(object, name, TRUE, value)
      slotgets <- asNamespace( 'methods')[[ paste0('slot', '<-')]]
      slotgets( object, name, TRUE, value)
    }
  } else {
    attr( object, name) <- value
    object
  }
}


"old.attassig" <-
function( object, name, value){
## Old code. Stringified to defeat cmd bloody check
r"--{
  mc <- match.call()
  if( isS4( object)) {
    mc[[1]] <- methods:::'@<-'
return( eval( mc, parent.frame())) # eval.parent() doesn't debug well
  }
  
  attr( object, as.character( mc[[3]])) <- value
return( object)
}--"
}

