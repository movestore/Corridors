# Corridor Use Behavior 

MoveApps

Github repository: github.com/movestore/Corridors

## Description
Identification of corridor use behavior based on movement characteristics of the animal. Currently only visualization possible!.

## Documentation
This App is embedded in an shiny UI, enabling the user to interactively change the parameters. The corridor identification is based on the movement of the animals, corridors are defined as those areas in the track where movement is fast and parallel (see *"LaPoint et al. (2013) Animal Behavior, Cost-based Corridor Models, and Real Corridors. Landscape Ecology.*" for more information). What is considered as fast and parallel is user defined.
The corridor use behavior is calculated on each individual separately (each individual is displayed on a separate tab). For the calculation the corridors the user can define the proportion of speeds which are considered high enough to be a valid corridor point, and the proportion of the circular variance that is low enough to be a valid corridor point. Low values of the circular variance indicate that the segments are (near) parallel. Identifying the corridors might take some playing around with these two values.

The midpoints of the identified corridor segments are displayed. These are grouped into corridor clusters according to a user selected circle size.

It is possible to zoom in to a plot by selecting an area with the mouse and double click. To go back to the full plot, double click again. *Currently there is a BUG, the zoom affects all plots of the different individuals, so one must double click on the plot of a new individual to see the track.*

**NOTE:** *This is a very preliminary version of the app. On the ToDo list is to improve the plots (e.g. give color to the corridor segments; improve scale bar), add functions to clean the results (e.g. remove corridor "outliers" which are obviously not a corridor; select specific corridors; export the results), and add function to export the results. If there is a specific feature that you would find very helpful, please get in touch with me (ascharf@ab.mpg.de) and I will check if it is possible to implement.*

### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format

### Artefacts
none

### Parameters
`Speed`: Proportion of speeds which are high enough to be a valid corridor point (default: speeds that are greater than 75 % of all speeds).

`Parallelism`: Proportion of the circular variances that is low enough to be a valid corridor point. Low values indicate that the segments are (near) parallel (default: variances that are lower than 25 % of all variances).

`Thin track to X mins`: This is specially recommended for high resolution tracks to ease finding regions with parallel segments. Default (=0) no thinning.

`Radius of corridor cluster circles (mts)`: All identified corridor segments that fall within a circle will be grouped as a corridor cluster.

`Update!`: after changing any parameter use this button to update the calculation.

### Null or error handling
**Data**: For use in further Apps the input data set is returned unmodified. Empty input will give an error.
