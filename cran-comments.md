January 28, 2024

The following edits have been made to the Description file as requested:
* space added before and after the year (2024).

Documents have been checked and ensured that no more than 2 cores are used in examples and vignettes. Specifically:
* inst/doc/annulus_demo.R -- Chunk 2/Line 25 sets cores to be the minimum of 2 or however many cores are detected. The maximum value cores can be is 2, but can still function if only one core is available.
* inst/doc/torus_demo.R -- Chunk 2/Line 23 sets cores to be the minimum of 2 or however many cores are detected. The maximum value cores can be is 2, but can still function if only one core is available.
* R/tau_bound.R -- Lines 35-39 is a safety check within the function to ensure no more cores are called than what is available. Default for the function is 1 core. If more cores are called than available, the number of cores will be at least 1 but no more than what is available. In the case of the vigenttes annulus_demo and torus_demo, no more than 2 cores will be called and where 2 cores are called, they have passed the availability check.
* R/mcmc.R -- Lines 42-47 and 177-182 are the same safety check as for tau_bound.R and function the same way.

January 26, 2024

The following edits have been made to the Description file as requested:
* "Statistical Package for" removed from title for redundancy
* Package names in Description are now in single quotation marks
* doi link for relevant citations has been fixed

January 25, 2024

Typos in Description file fixed. 

Vignette compilation time reduced to under 2 minutes.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
