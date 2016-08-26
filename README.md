# HexSimR
An R package for Post HexSim Simulation Analysis.  

Using this package, data of population viability analysis (PVA) that were generated with the software HexSim (Schumaker N, HexSim (Version 3 & 4). US Environmental Protection Agency. Environmental Research Laboratory, Corvallis, Oregon. [http://www.epa.gov/hexsim](http://www.epa.gov/hexsim)), can be collated, plotted and analysed.

## Quickstart
Install the package from version control from within R:
```
library(devtools)
install_github("carlopacioni/HexSimR")
```
If you are on Windows and have not used `devtools` before, then you have to download the Rtools executable file from CRAN webpage and run it. `devtools` can be installed from within R with 
```
install.packages("devtools")
```

**NOTE (August 2016)**
At the time of writing, the current official release of devtools (1.12.0) has an issue with R 3.3.1 on windows, which results in the dependencies of the package not being installed. This problem is resolved in the dev version. If you have problem installing `HexSimR` try the following (from devtools manual for the function `build_github_devtools`):

```
# Install devtools from CRAN if you haven't already
install.packages("devtools")

library(devtools)
build_github_devtools()

#### Restart R before continuing ####
install.packages("./devtools.zip", repos = NULL)

# Remove the package after installation
unlink("./devtools.zip")

library(devtools)
install_github("carlopacioni/HexSimR")

```

## Documentation
Use `help(package = "HexSimR")` to see the documentations available. 

## Questions and problems
You can use [HexSim google group](https://groups.google.com/forum/m/#!forum/hexsim) if you have questions or doubts. Use the [issue tracker](https://github.com/carlopacioni/HexSimR/issues) if you have feature/enhancement requests, or found a bug.

## Citation
If you use `HexSimR`, please cite:
Pacioni C, Kennedy MS, Berry O, Stephens D and Schumaker NH. In prep. Spatially-explicit modelling predicts effects of a major exclusion fence and lethal control on wild dog abundance.