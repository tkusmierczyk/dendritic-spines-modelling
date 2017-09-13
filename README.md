Code used in the following paper:

**G. Bokota, M. Magnowska, T. Kusmierczyk, M. Lukasik, et al.: Computational approach to dendritic spine taxonomy and shape transition analysis. Frontiers in Computational Neuroscience, 2016.**

-----------------------------------------------------------------------------------------------------------------

### Abstract 

The common approach in morphological analysis of dendritic spines of mammalian neuronal cells is to categorize spines into subpopulations based on whether they are stubby, mushroom, thin, or filopodia shaped. The corresponding cellular models of synaptic plasticity, long-term potentiation, and long-term depression associate the synaptic strength with either spine enlargement or spine shrinkage. Although a variety of automatic spine segmentation and feature extraction methods were developed recently, no approaches allowing for an automatic and unbiased distinction between dendritic spine subpopulations and detailed computational models of spine behavior exist. We propose an automatic and statistically based method for the unsupervised construction of spine shape taxonomy based on arbitrary features. The taxonomy is then utilized in the newly introduced computational model of behavior, which relies on transitions between shapes. Models of different populations are compared using supplied bootstrap-based statistical tests. We compared two populations of spines at two time points. The first population was stimulated with long-term potentiation, and the other in the resting state was used as a control. The comparison of shape transition characteristics allowed us to identify the differences between population behaviors. Although some extreme changes were observed in the stimulated population, statistically significant differences were found only when whole models were compared. The source code of our software is freely available for non-commercial use.


### Installation and requirements
The software was implemented with R (version 3.0.2). R is an interpreted language and to run the software an user should install appropriate interpreter that can be freely downloaded from the Internet. What is more, default installations may not contain all required libraries. In such case the user should install them manually. The following libraries (can be installed with `install.packages`) are required:
* igraph
* e1071
* pracma
* tiff (and libtiff4-dev on Linux)
* png

Further details can be found at [R-project](https://www.r-project.org/) or [R-bloggers](http://www.r-bloggers.com/installing-r-packages/). Additionally, authors recommend to work with supplied R scripts in an interactive way using  [Rstudio](https://www.rstudio.com/). 

### Data format
Below we show an example header for a data file representing our data format. The meaning of the consecutive columns is: 
* `unique_id` - unique identifier
* `nencki_id` - identifier assigned to a spine by the data provider
* `animal_id` - identifier of an animal
* `group_id` - identifier of a group (subpopulation) of spines
* `source0, source10, source40` - consecutive sources for three consecutive timestamps
* `0MIN_length, 0MIN_head_width, ... , 10_MIN_length 10_MIN_head_width, ...` -  feature values for consecutive timestamps. 
As shown above, the separator is space.

#### Spines identifiers
Spines identifiers (`unique_id` column) should be composed of the following parts merged with the separator "-":
* identifier for the dataset (for self-reference, may be 00)
* animal id
* spine id
* a letter denoting control (d) or active (f) group

Sample identifier is: `00-001-001-d`

### Simplification of shape representations
The initial features describing spines can be reduced with the dimensionality reduction technique to render the data representation to be more compact and simple and to filter out the noise. The code responsible for this analysis can be found in the files *routine_data_preparation.r* and *pca.r*. However, the calculation of the simplified representation was integrated in the main scripts (see below) and should not be run separately. 

###  Parameters selection
The selection of parameters (fuzzifier `m` and number of clusters `k`) can be done with the help of the script *script_shape_transitions_parameters_selection.r*. 

The script contains the following parts that should be run consecutively:
* routines and libraries including
* configuration and parameters setting (to be edited by the user by the execution)
* data loading and preparation (including PCA) 
* construction and plotting of errors matrices
* automatic selection of the parameters `k` (number of clusters) and `m` (fuzzifier).

The script is parametrized by the following variables (that should be edited manually by the user):
* `train.data.file` - a path to the input data file
* format-specific parameters:
 * `features.names.t0` - a list of column names of spine features at t<sub>0</sub>
 * `features.names.t1` - a list of column names of spine features at t<sub>1</sub>
 * `features.names.nice` - a list of short names of the features (arbitrary names by the user)
 * `spine.id = "unique_id"` - a name of the column containing spine identifier ()
 * `group.id = "group_id"` - a name of the column containing population identifier (must be a single character)
 * `spine.id.field.separator = "-"` - a character used to separate parts in spine identifiers ("-" by default)
 * `groups.ids` - a list of two characters containing possible names of sub-populations to be compared (see `group.id`)
* PCA-related parameters:
 * `pca.feature.groups` - a list of feature lists. Each sub-list should contain subset of names from `features.names.nice`. Each subset will be considered separately and will be used to produce the output features (named Comp.1, Comp.2, etc.). 
 * `pca.num.features` - a number of PCA features to be generated (there might more than one output feature per each subset of features). 
 * `normalization` - should the output features be normalized with Z-score or not (False by default).
* Clustering-related parameters 
 * `clustering.method` - a name of the clustering method (supported values: hierarchical, cmeans, kmeans)
 * `ms` - a list with `m` (fuzzifier) values to be tested
 * `ks` - a list of `k` (number of clusters) values to be tested
 * `compute.index.value.routine` - a path to the sub-routine that calculates the validation index (WSS by default)
 * `index.label` - a name of the validation index (to be used on the output plots)
* `output.dir` - a path to the directory where output plots and results should be stored


### Clustering and models comparison
The main script is *script_shape_transitions_analysis.r*. The script contains the following parts:
* routines and libraries including
* configuration and parameters setting (to be edited by the user by the execution)
* data loading and preparation (including PCA) 
* K-fold crossvalidation of prediction error estimation on the training data (can be run optionally and both before and after data filtering)
* clustering of the data
* a calculation of transition matrices and errors using bootstrap (can be run both before and after data filtering)
* data filtering to keep only spines from balanced data subset (optional; see below `balanced.ids.file`).
* a calculation of differences between transition matrices 
* a calculation of transition prediction quality with different models

The script is parametrized by the following variables (should be edited manually by the user):
* `train.data.file` - a path to the input data file
* `test.data.file` - a path to the additional test file if available; otherwise should be equal to `train.data.file`
* `balanced.ids.file` - a path to the file with spine identifiers (must contain the column `spine.id`) from the balanced subset of the data (see "Balanced subset selection"). If you want to construct transition matrices for the whole data set this variable to NaN.
* format-specific parameters (see above)
* PCA-related parameters (see above)
* spines visualization:
 * `images.base.dir` - a path to the directory containing source TIFF images of spines. If images are not supplied the variable should be commented out.
 * `spine.image.size` - how big area (measured in number of pixels) around the spine should be plotted
 * `visualise.num.representants` - how many spines per cluster should be plotted
 * `source0` - a name of the column that contains TIFF image name. The file should contain the spine image at t<sub>0</sub>. However, one file can contain many spines.
 * `xpos0` - a name of the column that contains spine's center x-coordinate at the image from `source0` 
 * `ypos0` - a name of the column that contains spine's center y-coordinate at the image from `source0` 
 * `source1` - a name of the column that contains TIFF image name.  The file should contain the spine image at t<sub>1</sub>  (one file can contain many spines).
 * `xpos1` - a name of the column that contains spine's center x-coordinate at the image from `source1` 
 * `ypos1` - a name of the column that contains spine's center y-coordinate at the image from `source1` 
 * `image.features.t0 = c(source0, xpos0, ypos0)` - a list of columns related to spine look at t<sub>0</sub>
 * `image.features.t1 = c(source1, xpos1, ypos1)` - a list of columns related to spine look at t<sub>1</sub> 
* `clustering.method` - a name of the clustering method (supported values: hierarchical, cmeans, kmeans)
* `m` - the fuzzifier value
* `k` - a number of clusters
* parameters related to transition matrices:
 * `TransitionMatrixCalculator = BuildNonnegativeTransitionMatrixLeastSquares` -  how to calculate transitions of elements (for crisp clusterings `BuildTransitionMatrix` can be used as well)
 * `PredictionErrorEstimator = AvgElementError` - how to calculate prediction errors
 * bootstrap parameters for errors estimation and comparing matrices:
  * `num.repetitions` - number of sampled populations for transition errors computation. More is better.
  * `bootstrap.fraction = 1.0` - bootstrap parameter (should not be changed)
  * `num.repetitions.comp.distribution.changes` - number of sampled populations for comparison of relative changes in clusters distribution for subpopulations (groups). More is better.
  * `num.repetitions.comp.trans.matrices` - number of sampled populations for comparison of transition matrices. More is better.
* `k.fold ` - a parameter used for testing prediction error on the training data set using k-fold crossvalidation (number of folds)
* parameters related to plotting transition grahps:
 * `threshold.counts` - min weight of the edge to be plotted (for example 6 spines)
 * `threshold.percents` - min weight percentage of the edge to be plotted (for example 20% of spines)
* `output.dir` - a path to the directory where output plots and results should be stored


