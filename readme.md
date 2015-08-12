
## Overview

#### The Problem

You use R interactively and accidentally shut down an R session and reaize that you've 
just lost the last X hours of work.

#### The Solution

The `logR` package enables more thorough logging than is currently implemented
in the `.Rhistory` file, such as dated log files and keeping logs of 
simultaneous interactive R sessions.  

`logR` is based on the functions `txtStart` and `txtStop` from the 
[`TeachingDemos`](https://cran.r-project.org/web/packages/TeachingDemos/index.html) 
package.

## Getting Started

1. Install the release version of `devtools`from CRAN via: 

        install.packages("devtools")

2. To install mfactor from github use:

    ```R
    devtools::install_github("jdthorpe/logR")
    ```
    note that in windows, you may have to build the github tools using: 

    ```R
    library(devtools)
    build_github_devtools()
    #### Restart R before continuing ####
    ```

    * Alternatively, download the github repository to a zip file and use:

        ```R
        install.packages("logR.zip", repos = NULL)

        # Remove the package after installation
        unlink("logR.zip")
        ```

3. Use logR to log your R sessions by adding this to your 
[.Rprofile](http://www.statmethods.net/interface/customizing.html):

	```R
	if('logR' %in% utils::installed.packages()[,1] ){
		library('logR')
		.logger  <-  newLogger('~/temp/Rhistory')
		.First <- function(){
			if( interactive() ){
				# clean up log files older than 7 days in excess of 100 files
				.Logger$clean(maxfiles=100,7,'days')
				# `sink()` A COPY OF THE TERMINAL OUTPUT TO A DATED LOG FILE
				.Logger$start()
			}
		}
		.Last  <-  function(){
			.Logger$stop()
		}
	}
	else cat('install package "logR" to enable console logging\n')
	```

