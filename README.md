# HexSimR
An R package for Post HexSim Simulation Analysis.  

Using this package, data of population viability analysis (PVA) that were generated with the software HexSim (Schumaker N, HexSim (Version 3 & 4). US Environmental Protection Agency. Environmental Research Laboratory, Corvallis, Oregon. [https://www.hexsim.net](https://www.hexsim.net)), can be collated, plotted and analysed.

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
If you want to access the package tutorials, you can use the option:
```
install_github("carlopacioni/HexSimR",  build_vignettes=TRUE)
```
but this requires some setting up. If this fails and you don't know what you should do to fix, then try to install from the binary version. To do this, download the binary version from [github](https://github.com/carlopacioni/HexSimR/releases) (either the zip or tar.gz files) and, assuming the downloaded package is in the working directory, run:
```
install.packages("./HexSimR-X.X.X.zip", repos = NULL)
```
where X.X.X is the version number. 

You should be able to access to see the available topics with:
```
vignette(package="HexSimR")
```
And open the vignette with:
```
vignette("name", package="HexSimR")
```
where "name" is the name of the vignette you want to open.


## Documentation
Use `help(package = "HexSimR")` to see the documentations available. For help on specific functions use `?function_name`, where function_name is the name of the function you are seeking information for.


The [manual](https://www.researchgate.net/publication/306884751_HexSimR_manual?ev=prf_pub) and a short [tutorial](https://www.researchgate.net/publication/306911593_HexSimR_tutorial) can also be downloaded as PDF.


## Questions and problems
You can use [HexSim google group](https://groups.google.com/forum/m/#!forum/hexsim) if you have questions or doubts. Use the [issue tracker](https://github.com/carlopacioni/HexSimR/issues) if you have feature/enhancement requests, or found a bug.

## Citation
If you use `HexSimR`, please cite:
Pacioni C, Kennedy MS, Berry O, Stephens D and Schumaker NH. In prep. Spatially-explicit modelling predicts effects of a major exclusion fence and lethal control on wild dog abundance.