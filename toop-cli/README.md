
# SIGMAC

Console sigma compiler

Version: 0.1.1

## OS:

* Linux OS (i386, amd64)
* Windows 8+

## For linux

### Prerequisites for run sigmac 

* openjdk-8-jre

### Run in dev mode

* sbt
* project toopCli
* compile
* run /path/to/file.sigma

### Package jar

* sbt
* project toopCli
* assembly

### Run in prod mode

* java -jar sigmac.jar /path/to/file.sigma

### Build deb package

* cd toop-cli
* fakeroot dpkg --build debian
* sudo dpkg -i debian.deb

## For windows

### Install package locally

* cd ./toop-cli/windows/sigmac/src/tools
* powershell -noexit "& ""./chocolateyInstall.ps1"""

### Uninstall package locally

* cd ./toop-cli/windows/sigmac/src/tools
* powershell -noexit "& ""./chocolateyUninstall.ps1"""

### Manage package globally (not implemented yet)

Install:
* choco install sigmac

Uninstall:
* choco uninstall sigmac

## Run sigmac

* sigmac /path/to/file.sigma
