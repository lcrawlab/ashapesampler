# ashapesampler

[![R CMD check](https://github.com/lcrawlab/ashapesampler/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/lcrawlab/ashapesampler/actions/workflows/check-standard.yaml)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/ashapesampler)](https://cranlogs.r-pkg.org/badges/grand-total/ashapesampler)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ashapesampler)](https://cran.r-project.org/package=ashapesampler)

Now available to download from [CRAN](https://cran.r-project.org/package=ashapesampler).

This repository contains an R package for generating synthetic alpha shapes by either (i) empirical sampling based on an existing dataset with reference shapes, or (ii) probabilistic sampling from a known distribution function on shapes.

## Introduction

Understanding morphological variation is an important task in many applications. Recent studies in computational biology have focused on developing computational tools for the task of sub-image selection which aims at identifying structural features that best describe the variation between classes of shapes. A major part in assessing the utility of these approaches is to demonstrate their performance on both simulated and real datasets. However, when creating a model for shape statistics, real data can be difficult to access and the sample sizes for these data are often small due to them being expensive to collect. Meanwhile, the landscape of current shape simulation methods has been mostly limited to approaches that use black-box inference---making it difficult to systematically assess the power and calibration of sub-image models.

In this R package, we introduce the $\alpha$-shape sampler: a probabilistic framework for simulating realistic 2D and 3D biological shapes and images based on probability distributions which can be learned from real data or explicitly stated by the user.

## The Method

The **ashapesampler** package supports two mechanisms for sampling shapes in two and three-dimensions, which we outline below. The first strategy empirically samples new shapes based on an existing dataset --- this was highlighted in the main text of Winn-Nuñez et al. The second strategy probabalistically samples new shapes from a known distrubtion --- this approach is also implementated in this software package with the corresponding theory being derived in the Supporting Information of Winn-Nuñez et al. 

### Generating New Shapes from an Existing Dataset

The $\alpha$-shape sampler consists of four key steps:
1. Input aligned reference shapes as simplicial complexes. A simplicial complex object in this case is a list containing (a) the Euclidean coordinates of the vertices and (b) a list of all vertices, edges, faces, and tetrahedra. Functions are available to read OFF files into R in the correct format and to extract the simplical complex information from a generated alpha complex. A method to convert a binary mask to a 2D simplicial complex for use in the algorithm can be found in the vignettes. 
2. Calculate the _reach_ for each shape in the dataset. The reach is estimated based on boundary points of the simplicial complex. Users can choose the summary statistic used for the estimated reach for a reference shape to be either the mean, median, or minimum across points. Default is mean. Once we have the reach for each shape, users can take some summary statistic (usually the minimum) over a `J` subset of randomly selected reference shapes to produce new shapes.
3. Sample new points, using the combined point cloud of the randomly selected `J` shapes and the estimated reach `tau` derived from the `J` reference shapes. Parameters for rejection sampling can be adjusted by the users and are discussed further in the vignettes. Note that this step is generally the longest computationally---if the user reaches a computational bottleneck, check to the value of `tau` relative to the area/volume of the combined point cloud. Parallelizing also speeds up the algorithm.
4. Output newly generated shape as an alpha shape object.

Users should note that it is critical to align shapes to maximize the pipeline's success and that there may be some manual parameter tuning for the best results. 

Demonstrations for pipeline implementation are in the vignettes. Functions are broken into parts instead of integrated altogether so that users can troubleshoot the pipeline at different stages. 

### Generating New Shapes from Probability Distributions

Users an also use the ashapesampler package to generate shapes in two and three dimensions from probability distributions. This approach can prove particularly useful for simulating shapes and benchmarking the performance of different statistical methods. Here, we list the parameters for generating new shapes in two and three dimensions. Options for user-adjusted parameters and defaults can be found in the vignettes. Users should keep a few key points in mind when generating shapes this way:
* The ``bound`` parameter is the manifold from which points are sampled. At this time, the package only supports a square, a circle (i.e., a disk where the function assumes it is filled in), and an annulus in two dimensions. In three-dimensions, it supports a cube, sphere (i.e., a ball where the function assumes it is filled in), and torus. The size of these manifolds can be specified using the ``rmax`` and ``rmin`` parameters, where applicable. Adjusting the size may affect computational time if the reach ``tau`` is not adjusted with it.
* The reach ``tau`` needs to be specified as a finite value in advance, as this hyperparameter affects the choice of ``alpha``. Default of ``tau`` is 1, but it can be any finite value. Keep in mind that the smaller that ``tau`` is relative to the area or volume of the manifold, the more detail in the shapes produced and the more time it will take to produce a new generate opbject shape.
* By default, ``alpha`` will be as large as theoretically allowed. The smaller ``alpha`` is relative to ``tau``, the more points will need to be sampled and the more time it will take to produce a new generate shape. This is particularly true when the goal is to have shapes to have both full connectivity/no isolated points as well as preserve the homology.
* At this time, the package only supports the truncated normal distribution for randomly selecting ``alpha``. Bounds of this truncated normal can be adjusted by the user up to what is theoretically allowed. Keep in mind that the general bounds of this distribution should keep ``alpha`` as large as possible for best computational performance.

## R Packages for ashapesampler and Tutorials

The ashapesampler software requires the installation of the following R libraries:

[alphahull](https://cran.r-project.org/package=alphahull)

[alphashape3d](https://cran.r-project.org/package=alphashape3d)

[doParallel](https://cran.r-project.org/package=doParallel)

[dplyr](https://cran.r-project.org/package=dplyr)

[foreach](https://cran.r-project.org/package=foreach)

[ggplot2](https://cran.r-project.org/package=ggplot2)

[pracma](https://cran.r-project.org/package=pracma)

[Rvcg](https://cran.r-project.org/package=Rvcg)

[TDA](https://cran.r-project.org/package=TDA)

[truncnorm](https://cran.r-project.org/package=truncnorm)

Unless stated otherwise, the easiest way to install many of these packages is with the following example command entered in an R shell:

    install.packages("alphahull", dependecies = TRUE)

Alternatively, one can also [install R packages from the command line](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Installing-packages).

## C++ Packages for ashapesampler and Tutorials

The code in this repository assumes that basic C++ functions and applications are already set up on the running personal computer or cluster. If not, some of the packages (e.g., TDA and alphashape3d) needed to build alpha complexes and alpha shapes in three dimensions will not work properly. A simple option is to use [gcc](https://gcc.gnu.org/). macOS users may use this collection by installing the [Homebrew package manager](https://brew.sh/index.html) and then typing the following into the terminal:

    brew install gcc

For macOS users, the Xcode Command Line Tools include a GCC compiler. Instructions on how to install Xcode may be found [here](http://railsapps.github.io/xcode-command-line-tools.html). Additional installs for macOS users are automake, curl, glfw3, glew, xquartz, and qpdf. For extra tips on how to run C++ on macOS, please visit [here](http://seananderson.ca/2013/11/18/rcpp-mavericks.html). For tips on how to avoid errors dealing with "-lgfortran" or "-lquadmath", please visit [here](https://thecoatlessprofessor.com/programming/rcpp-rcpparmadillo-and-os-x-mavericks-lgfortran-and-lquadmath-error/).

## R Package Installation

To install the package from CRAN, run the following command:

	install.packages("ashapesampler", dependencies = TRUE)

To install the package from GitHub, we recommend using the remotes package by running the command:

	remotes::install_github('lcrawlab/ashapesampler')

To then load the package in R, use the command

	library(ashapesampler)

Other common installation procedures may apply.

# Code Usage

## Vignettes

The `vignettes` folder contains the following demonstrations for running and analyzing results in the ashapesampler: 

* Sampling alpha shapes from a probability distribution in two-dimensions.
* Sampling alpha shapes from a probability distribution in three-dimensions.
* Generating new 2D annuli from a simulated set of annuli.
* Generating new 3D tori from a simulated set of tori.

Additional vignettes and source code can be found in the corresponding [results repository](https://github.com/lcrawlab/ashapesampler_paper_results).

  
The auto3dgm paradigm for assigning landmarks via unsupervised learning can be found [here](https://toothandclaw.github.io/).

## Data 

Primate manibular molar data and neutrophil binary masks can be accessed and downloaded [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K9A0EG&faces-redirect=true).

## Relevant Citations

E.T. Winn-Nuñez, H. Witt, D. Bhaskar, R.Y. Huang, I.Y. Wong, J.S. Reichner, and L. Crawford. Generative modeling of biological shapes and images using a probabilistic $\alpha$-shape sampler. _bioRxiv_.

## Questions and Feedback

Please send any questions or feedback to the corresponding authors [Emily Winn-Nuñez](mailto:emily_winn-nunez@brown.edu) or [Lorin Crawford](mailto:lcrawford@microsoft.com).

We appreciate any feedback you may have with our repository and instructions.
