# R-Toolbox

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
rake create_setup[7.1.0]
```

## Code Status
[![Build status](https://ci.appveyor.com/api/projects/status/6wssc33akfebg3yk/branch/master?svg=true&passingText=master%20-%20passing)](https://ci.appveyor.com/project/open-systems-pharmacology-ci/r-toolbox/branch/master)

## Code of conduct
Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

## Contribution
We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md). If you are contributing code, please be familiar with the [coding standard](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARD.md).

## License
R-Toolbox is released under the [GPLv2 License](LICENSE).
