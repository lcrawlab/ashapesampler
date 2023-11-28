# ashapesampler

This repository contains an R package for generating synthetic alpha shapes from either (i) empirically sampling based on an existing data set, or (ii) probabilistic sampling from a known distribution function on shapes.

## Introduction

Understanding morphological variation is an important task in many applications. Recent studies in computational biology have focused on developing computational tools for the task of sub-image selection which aims at identifying structural features that best describe the variation between classes of shapes. A major part in assessing the utility of these approaches is to demonstrate their performance on both simulated and real datasets. However, when creating a model for shape statistics, real data can be difficult to access and the sample sizes for these data are often small due to them being expensive to collect. Meanwhile, the landscape of current shape simulation methods has been mostly limited to approaches that use black-box inference---making it difficult to systematically assess the power and calibration of sub-image models.

## The Method

The **ashapesampler** package supports two mechanisms for sampling shapes in two and three dimensions, which we outline below. The first, empirically sampling based on an existing data set, was highlighted in the original main text of the paper. The second, probabalistic sampling from a known distrubtion, is the computational implementation of the theory derived in that paper.  

### Generating New Shapes to Fit Existing Data Set

The pipeline consists of four key steps:
1. Input the aligned shapes as simplicial complexes. A simplicial complex object in this case is a list containing (a) the Euclidean coordinates of the vertices and (b) a list of all vertices, edges, faces, and tetrahedra.
2. Calculate the reach for each shape in the data set - this reach is estimated based on boundary points of the simplicial complex. Users can tune the summary statistic used for the estimated reach to be mean, median, or minimum. Default is mean. Once we have the reach for each shape, users can take some summary statistic - usually mean - of the `J` shapes randomly chosen to produce the new shape.
3. Sample new points, using the combined point cloud of the randomly selected `J` shapes and the estimated reach `tau` derived from the `J` shapes. Parameters for rejection sampling can be adjusted by the users and are discussed further in the vignettes. Note that this step is generally the longest computationally - if the user reaches a bottleneck, check to the value of `tau` relative to the area/volume of the combined point cloud. Parallelizing also speeds up the algorithm.
4. Output new shape as an alpha shape object.

Users should note that it is critical to align shapes to maximize the pipeline's success, and that there may be some manual parameter tuning for the best results. 

Demonstrations for pipeline implementation are in the vignettes. Functions are broken into parts instead of integrated altogether so that users can troubleshoot the pipeline at different stages. 

### Sampling New Shapes from Probability Distribution

Users an also use our package to generate shapes in two and three dimensions from a probability distribution. The tool can prove particularly useful for simulating shapes and comparing analyises of multiple methods. Here, we list the parameters for simulating new shapes in two and three dimensions. Options for user-adjusted parameters and defaults can be found in the vignettes. Users should keep in mind a few key points:
* The ``bound`` parameter is the manifold from which points are sampled. At this time, the package only supports a square, a circle (disk - function assumes it is filled in), and an annulus in two dimensions and a cube, sphere (ball - function assumes it is filled in), and torus in three dimensions. The size of these manifolds can be specified using the ``rmax`` parameter and the ``rmin`` parameter, where applicable. Adjusting the size may affect computational time if ``tau`` is not adjusted with it.
* The reach ``tau`` needs to be specified as a finite value in advance, as this hyperparameter affects the choice of ``alpha``. Default of ``tau`` is 1, but it can be any finite value. Keep in mind that the smaller that ``tau`` is relative to the area or volume of the manifold, the more detail in the shapes produced, but the more time it will take to produce a shape.
* By default, ``alpha`` will be as large as theoretically allowed. The smaller ``alpha`` is relative to ``tau``, the more points will need to be sampled, and the more time it will take to produce a shape. This is particularly true when the bounds of the number of points are tied to desire for connectivity/no isolated points as well as preserving the homology.
* At this time, the package only supports the truncated normal distribution for randomly selecting ``alpha``. Bounds of this truncated normal can be adjusted by the user up to what is theoretically allowed. Keep in mind that the general bounds of this distribution should keep ``alpha`` as large as possible for best computational performance.

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

Primate manibular molar data can be accessed and downloaded [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K9A0EG&faces-redirect=true).

## Relevant Citations

E.T. Winn-Nuñez, H. Witt, D. Bhaskar, R. Huang, I.Y. Wong, J. Reichner, and L. Crawford. A probabilistic method for sampling alpha-shapes.

## Questions and Feedback

Please send any questions or feedback to the corresponding authors [Emily Winn-Nuñez](mailto:emily_winn-nunez@brown.edu) or [Lorin Crawford](mailto:lcrawford@microsoft.com).

We appreciate any feedback you may have with our repository and instructions.
