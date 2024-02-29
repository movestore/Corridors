# Corridor Use Behavior - Per Track

MoveApps

Github repository: github.com/movestore/Corridors

## Description
Identification of corridor use behavior based on movement characteristics of the animal. Tracks are plotted separately and settings can be adjusted to each track.

## Documentation
This App is embedded in an shiny UI, enabling the user to interactively change the parameters. The corridor identification is based on the movement of the animals, corridors are defined as those areas in the track where movement is fast and parallel (see *"LaPoint et al. (2013) Animal Behavior, Cost-based Corridor Models, and Real Corridors. Landscape Ecology.*" for more information). What is considered as fast and parallel is user defined.
The corridor use behavior is calculated on each track separately. One tab displays all tracks at once, and also each track is displayed on a separate tab. For the calculation the corridors the user can define the proportion of speeds which are considered high enough to be a valid corridor point, and the proportion of the circular variance that is low enough to be a valid corridor point. Low values of the circular variance indicate that the segments are (near) parallel. Identifying the corridors might take some playing around with these two values. In the tab with all tracks, the settings affect all tracks simultaneously. To give different tracks different settings, each tab per track can be used.

Interpretation caution: the method sometimes identifies segments as corridors that probably aren't, which mostly consist of only a few segments. With visual inspection it normally is quite straight forward to distinguish in most cases the true corridors, as they consist of a large amount of segments identified as *corridor segments*. 

To help removing wrongly identified corridors, the settings "Cluster radius" and "Segment number" can be adjusted. This will define a cluster of a minimum number of corridor segments that are found within a certain radius.

The method is highly sensitive to the length of the segments (i.e. resolution of the data). If the data have a high fix rate, with many short segments, finding parallel segments will be rather difficult. Therefore it is recommended to thin the track to a lower fix rate to ease finding regions with parallel segments. Experience shows that for many mammal species 15min fix rate seems to work well.

After any setting has been changed, the button "update!" must be clicked to make the change effective.

It is possible to select & unselect corridor clusters by clicking on the menu on the top right corner of the map. Only the locations of the select corridor clusters get recorded in the added attribute "corridorBehavior" in the output data.

**CAUTION:** Calculations can take long. The higher the number of locations, the longer it takes to calculate. Every time you click on a tab the calculation starts from scratch. Be patient and the results will be shown when the calculation is done.


### Input data
move2_loc

### Output data
move2_loc

### Artefacts
none

### Settings
`Speed`: Proportion of speeds which are high enough to be a valid corridor point (default: speeds that are greater than 75 % of all speeds).

`Parallelism`: Proportion of the circular variances that is low enough to be a valid corridor point. Low values indicate that the segments are (near) parallel (default: variances that are lower than 25 % of all variances).

`Thin track to X mins`: This is specially recommended for high resolution tracks to ease finding regions with parallel segments. Default (=0) no thinning.

`Cluster radius (m)`: All identified corridor segments that fall within a circle of the given radius will be grouped as a corridor cluster. These clusters can than be selected or unselected in the menu on the top right corner of the map. Only the locations of the select corridor clusters get recorded in the added attribute "corridorBehavior" in the output data. Default is 500m (adjust value to what best fits the data)

`Segment number`: Minimum number of segments that will define a cluster. Clusters with fewer segments will be excluded. Default is 3.

`Update!`: after changing any setting use this button to update the calculation.

### Null or error handling
**Data**: For use in further Apps the input data set is returned unmodified. Empty input will give an error.
