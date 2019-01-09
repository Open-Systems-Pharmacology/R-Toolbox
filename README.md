# R-Toolbox

## About the Toolbox
The R Toolbox is a collection of R functions which allows the processing of models developed in PK-Sim® or MoBi® from within R. A variety of functions is offered to manipulate models, to simulate models, and to handle simulation results. 
The functions are designed very similar to the functions that are provided with the [MoBi® Toolbox for Matlab®](https://github.com/Open-Systems-Pharmacology/Matlab-Toolbox).
Details are available in the build-in help of R upon loading the R package.

## Installing the R Toolbox
The R Toolbox is provided to the user as a zip file. This zip file can be found under **"C:\Program Files\Open Systems Pharmacology\MoBi Toolbox for R _X.Y_\MoBiToolboxForR__X.Y.Z_.zip"** . The standard package install via zip files is used. On Windows, **_install.packages_** can install a binary package from a local zip file by setting argument _repos_ to NULL.
Rgui.exe has a menu Packages with a GUI interface to **_install.packages_** that may be used.

**The R Toolbox is only available for the 64bit R (versions 3.5 or later).**

## Getting Started
* Install nuget and add nuget.exe to your path
* Install ruby version 2.2 or higher
* clone the repository with recursive option 
```
git clone <url.git> --recursive
```
* restore nuget packages manually
```
nuget restore packages.config -PackagesDirectory packages
```
* create the package and setup
```
rake create_setup[7.4.0]
```

## Code Status
[![Build status](https://ci.appveyor.com/api/projects/status/6wssc33akfebg3yk/branch/develop?svg=true&passingText=develop%20-%20passing)](https://ci.appveyor.com/project/open-systems-pharmacology-ci/r-toolbox/branch/develop)

## Code of conduct
Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

## Contribution
We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md). If you are contributing code, please be familiar with the [coding standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS.md).

## License
R-Toolbox is released under the [GPLv2 License](LICENSE).
