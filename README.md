# ashapesampler

This repository contains an R package for generating synthetic alpha shapes from either (i) probabilistic sampling from a known distribution function on shapes or (ii) empirically sampling based on an existing data set.

## R Packages for ashapesampler and Tutorials

The ashapesampler software requires the installation of the following R libraries:

[alphahull](https://cran.r-project.org/web/packages/alphahull/index.html)

[alphashape3d](https://cran.r-project.org/web/packages/alphashape3d/index.html)

[doParallel](https://cran.r-project.org/web/packages/doParallel/index.html)

[dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)

[foreach](https://cran.r-project.org/web/packages/foreach/index.html)

[ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html)

[pracma](https://cran.r-project.org/web/packages/pracma/index.html)

[Rvcg](https://cran.r-project.org/web/packages/Rvcg/index.html)

[TDA](https://cran.r-project.org/web/packages/TDA/index.html)

[truncnorm](https://cran.r-project.org/web/packages/truncnorm/index.html)

Unless stated otherwise, the easiest way to install many of these packages is with the following example command entered in an R shell:

    install.packages("alphahull", dependecies = TRUE)

Alternatively, one can also [install R packages from the command line](http://cran.r-project.org/doc/manuals/r-release/R-admin.html#Installing-packages).

## C++ Packages for ashapesampler and Tutorials

The code in this repository assumes that basic C++ functions and applications are already set up on the running personal computer or cluster. If not, some of the packages (e.g., TDA and alphashape3d) needed to build alpha complexes and alpha shapes in three dimensions will not work properly. A simple option is to use [gcc](https://gcc.gnu.org/). macOS users may use this collection by installing the [Homebrew package manager](http://brew.sh/index.html) and then typing the following into the terminal:

    brew install gcc

For macOS users, the Xcode Command Line Tools include a GCC compiler. Instructions on how to install Xcode may be found [here](http://railsapps.github.io/xcode-command-line-tools.html). Additional installs for macOS users are automake, curl, glfw3, glew, xquartz, and qpdf. For extra tips on how to run C++ on macOS, please visit [here](http://seananderson.ca/2013/11/18/rcpp-mavericks.html). For tips on how to avoid errors dealing with "-lgfortran" or "-lquadmath", please visit [here](http://thecoatlessprofessor.com/programming/rcpp-rcpparmadillo-and-os-x-mavericks-lgfortran-and-lquadmath-error/).

## R Package Installation

To install the package, we recommend using the remotes package by running the command:

	remotes::install_github('lcrawlab/ashapesampler')

To then load the package in R, use the command

	library(ashapesampler)

Other common installation procedures may apply.

# Code Usage

## Vignettes

The `vignettes` folder contains the following demonstrations for running and analyzing results in the ashapesampler: 

* Sampling alpha shapes from a probability distribution in two dimensions.
* Sampling alpha shapes from a probability distribution in three dimensions.
* Generating new 2D annuli from a simulated set of annuli.

* Generating new 3D tori from a simulated set of tori.

* Converting binary masks to simplicial complexes for input into the alpha shape sampler.

* Generating new 3D teeth from data set of primate manibular molars.

  
The auto3dgm paradigm for assigning landmarks via unsupervised learning can be found [here](https://toothandclaw.github.io/)

## Data 

Primate manibular molar data can be accessed and downloaded [here](https://www.dropbox.com/sh/rs8pjmhrwcdcuxk/AAC3Fj2_RNZLTVR_XhN4jiGxa?dl=0).

## Relevant Citations

E.T. Winn-Nuñez, H. Witt, D. Bhaskar, R. Huang, I.Y. Wong, J. Reichner, and L. Crawford. A probabilistic method for sampling alpha-shapes.

## Questions and Feedback

Please send any questions or feedback to the corresponding authors [Emily Winn-Nuñez](mailto:emily_winn-nunez@brown.edu) or [Lorin Crawford](mailto:lcrawford@microsoft.com).

We appreciate any feedback you may have with our repository and instructions.
