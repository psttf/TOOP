
# SIGMAC

Console sigma compiler

Version: 0.1.0

## OS:

* Linux OS (i386, amd64)

## Prerequisites for run sigmac 

* openjdk-8-jre

## Run in dev mode

* sbt
* project toopCli
* compile
* run /path/to/file.sigma

## Package jar

* sbt
* project toopCli
* assembly

## Run in prod mode

* java -jar sigmac.jar /path/to/file.sigma

## Build deb package

* cd toop-cli
* fakeroot dpkg --build debian
* sudo dpkg -i debian.deb

## Run sigmac

* sigmac /path/to/file.sigma
