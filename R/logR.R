
newLogger <- function(dir,file="%Y_%m_%d_%H_%M_%S.Rhistory",cmdfile){

	StartTime  <-  Sys.time()

	# CREATE THE TEMP DIRECORY IF IT DOES NOT ALREADY EXIST
	dir  <-  path.expand(dir)
	dir.create(dir, showWarnings = FALSE)

	ARGS  <-  list(file=file.path(dir,format(StartTime,file)))
	if(!missing(cmdfile))
		ARGS$cmdfile  <-  file.path(dir,format(StartTime,cmdfile)) # JT added

	# generate a random suffix for the task names
	suffix  <-  strsplit(tempfile(""),.Platform$file.sep)[[1]]
	suffix  <-  suffix[length(suffix)]


	## -----------------------------------------------------------------
	## What follows is copied, nearly verbatim from the file R2txt.R 
	## of the TeachingDemos package.  The major differences are that 
	## (1) calls to `options(prompt= , continue=  )` were removed,
	## (2) task names are given a suffix and the various logging functions
	## are defined in the scope of a call to `newLogger` in order to 
	## allow for multiple active loggers.
	## -----------------------------------------------------------------

	### consider adding option to include errors Can implement by using
	### options(error=newfunction) and newfunction would use the
	### savehistory command to get the expression and geterrmessage to get
	### the error message.  Warnings can be included by checking to see if
	### last.warning has changed, use print.warnings to format.

	R2txt.vars <- new.env()

	R2txt <- function(cmd,res,s,vis) {
	  if(R2txt.vars$first) {
		  R2txt.vars$first <- FALSE
		  if( R2txt.vars$res ) {
			  sink()
			  close(R2txt.vars$outcon)
			  R2txt.vars$outcon <- textConnection(NULL, open='w')
			  sink(R2txt.vars$outcon, split=TRUE)
		  }
	  } else {

		  if( R2txt.vars$cmd ){
			  cmdline <- deparse(cmd)
			  cmdline <- gsub('    ', paste("\n",R2txt.vars$continue, sep=''),
							  cmdline)
			  cmdline <- gsub('}', paste("\n",R2txt.vars$continue,"}", sep=''),
							  cmdline)
			  cat(R2txt.vars$prompt, cmdline, "\n", sep='',
				  file=R2txt.vars$con)
		  }
		  if( R2txt.vars$cmdfile ) {
			  cmdline <- deparse(cmd)
			  cmdline <- gsub('    ', "\n ", cmdline)
			  cmdline <- gsub('}', "\n}", cmdline)
			  cat(cmdline,"\n", file=R2txt.vars$con2)
		  }

		  if( R2txt.vars$res ) {
			  tmp <- textConnectionValue(R2txt.vars$outcon)
			  if(length(tmp)) {
				  cat(tmp,sep='\n',file=R2txt.vars$con)
				  sink()
				  close(R2txt.vars$outcon)
				  R2txt.vars$outcon <- textConnection(NULL, open='w')
				  sink(R2txt.vars$outcon, split=TRUE)
			  }
		  }
	  }

	  TRUE
	}

	txtStart <- function(file, commands=TRUE, results=TRUE, append=FALSE,
						 cmdfile, visible.only=TRUE) {

	  tmp <- TRUE
	  if(is.character(file)){
		if(append){
		  con <- file(file,open='a')
		} else {
		  con <- file(file,open='w')
		}
		tmp <- FALSE
	  } else if( any( class(file) == 'connection' ) ) {
		con <- file
	  } else {
		stop('file must be a character string or connection')
	  }
	  if( tmp && isOpen(con) ) {
		R2txt.vars$closecon <- FALSE
	  } else {
		R2txt.vars$closecon <- TRUE
		if(tmp){
			if(append) {
				open(con, open='a')
			} else {
				open(con, open='w')
			}
		}
	  }
	  R2txt.vars$vis <- visible.only
	  R2txt.vars$cmd <- commands
	  R2txt.vars$res <- results
	  R2txt.vars$con <- con
	  R2txt.vars$first <- TRUE

	  if(results) {
		  R2txt.vars$outcon <- textConnection(NULL, open='w')
		  sink(R2txt.vars$outcon, split=TRUE)
	  }

	  if( !missing(cmdfile) ) {
		tmp <- TRUE
		if(is.character(cmdfile)) {
		  con2 <- file(cmdfile, open='w')
		  tmp <- FALSE
		} else if( any( class(cmdfile) == 'connection' ) ) {
		  con2 <- cmdfile
		}
		if( tmp && isOpen(con2) ) {
		  R2txt.vars$closecon2 <- FALSE
		} else {
		  R2txt.vars$closecon2 <- TRUE
		  if(tmp) {
			  open(con2, open='w')
		  }
		}
		R2txt.vars$con2 <- con2
		R2txt.vars$cmdfile <- TRUE
	  } else {
		R2txt.vars$cmdfile <- FALSE
	  }

	  cat('Output being copied to text file,\nuse txtStop to end\n')
	  addTaskCallback(R2txt, name=paste('r2txt',suffix,sep='_'))
	  invisible(NULL)
	}

	txtStop <- function() {
	  removeTaskCallback(paste('r2txt',suffix,sep='_'))
	  if( R2txt.vars$closecon ) {
		close( R2txt.vars$con )
	  }
	  if( R2txt.vars$cmdfile && R2txt.vars$closecon2 ) {
		close( R2txt.vars$con2 )
	  }
	  if(R2txt.vars$res) {
		  sink()
		  close(R2txt.vars$outcon)
	  }
	  evalq( rm(list=ls()), envir=R2txt.vars )
	  invisible(NULL)
	}

	txtComment <- function(txt,cmdtxt) {
		R2txt.vars$first <- TRUE
		if(!missing(txt)) {
			cat("\n",txt,"\n\n", file=R2txt.vars$con)
		}
		if(!missing(cmdtxt)) {
			cat("# ",cmdtxt,"\n", file=R2txt.vars$con2)
		}
	}

	txtSkip <- function(expr) {
		R2txt.vars$first <- TRUE
		expr
	}


	## -----------------------------------------------------------------
	## the above section is copied, nearly verbatim from the file R2txt.R 
	## of the TeachingDemos package.  
	## -----------------------------------------------------------------

	list(Start=function(...)
			do.call(txtStart,modifyList(list(...),ARGS)),
		Stop=txtStop,
		Comment=txtComment,
		Skip=txtSkip,
		clean=function(maxFiles,
					   age=7,
					   units=c("days", "secs", "mins", "hours", "weeks")){
		units  <-  match.arg(units)

		# DELETE FILES OLDER THAN age (units) in excess of maxFiles
		FNAMES  <-  list.files(dir)
		isLogFile  <-  FNAMES %in% c(file,cmdfile) | 
						!is.na(as.POSIXct(FNAMES,format=file)) |
						!is.na(as.POSIXct(FNAMES,format=cmdfile))
		FNAMES <- FNAMES[isLogFile]
		if(!length(FNAMES))
			return()

		# get file data from the log files
		FILEDATA  <-  file.info(file.path(dir,FNAMES))
		FILEDATA  <-  FILEDATA[order(FILEDATA$mtime),]
		if(!missing(maxFiles))
			FILEDATA  <-  FILEDATA[-seq(maxFiles),]
		if(nrow(FILEDATA))
		for(i in nrow(FILEDATA))
			if(difftime(TIME, FILEDATA[i,'mtime'], units=units) > age ) 
				file.remove(rownames(FILEDATA)[i])
	})

}


	

