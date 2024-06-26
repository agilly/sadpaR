## Prerequisites

### Windows Subsystem for Linux (WSL)

#### Test if you have WSL already
Go to your start menu and search for Ubuntu in your apps. If it is there, it should open a terminal window. If it is not there, then you probably don't have WSL.

#### Why do we need WSL and how to install it?
This package requires R on Linux. It was developed and tested in WSL2. If you have a Windows computer, it is recommended that you install WSL2 as well. Instructions can be found [online](https://learn.microsoft.com/en-us/windows/wsl/install).

### (optional) Install a terminal emulator
After installing WSL above, you will be able to start ubuntu from your start menu. A terminal should appear asking you to configure your user. However, that terminal is quite limited. It is advised to install a more complete solution. [Tabby](tabby.sh) is a very good, fully free option, [MobaXTerm](https://mobaxterm.mobatek.net/download.html) is an older alternative.

### R
To install R on Linux, follow the instructions here: https://www.digitalocean.com/community/tutorials/how-to-install-r-on-ubuntu-22-04

### Dependencies
The package requires the command line tools `gifsicle`, `ffmpeg` and `ExifTool`. Install the former 2 using `apt`:
```bash
sudo apt update && sudo apt install -y gifsicle ffmpeg make build-essential libharfbuzz-dev libfribidi-dev libssl-dev libfontconfig1-dev libxml2-dev libpng-dev libtiff5-dev libjpeg-dev libcurl4-openssl-dev
```

`ExifTool` is a dependency of the R library `EXIFr`, and can be installed with :

```bash
fn=$(wget https://exiftool.org -q -O- | grep ".tar.gz"| sed 's/.tar.gz.*/.tar.gz/;s/.*\"//')
wget https://exiftool.org/$fn
tar -xvzf $fn
cd ${fn/.tar.gz/}
perl Makefile.PL
make test
sudo make install
```

### R libraries
#### Windows users
One particular library is troublesome to install and requires additional steps. Follow these instructions if you are running WSL (Linux on Windows).
In a terminal, type
```bash
export R_INSTALL_STAGED=FALSE
```
Then start R, and type:
```R
install.packages("xml2")
```
and quit R.

### All users (incl. Windows)
Start R on linux and type:
```R
toinstall=c("data.table",  "shiny", "zoo", "optparse", "exifr",  "chron", "DT", "tools", "shinyjs", "shinyFiles", "jsonlite", "config", "devtools", "R.utils")
new.packages <- list.of.packages[!(toinstall %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
```
Finally we install the non-canonical library `EXIFr`:
```R
library(devtools)
devtools::install_github("cmartin/EXIFr")
```

### Installation
Copy the following files to a directory of your choice:
```
CTpreprocess
makegifs.sh
runIntervalDetector.R
IntervalDetector/app.R
IntervalDetector/dataMgmt.R
IntervalDetector/internalSelectInputCoherenceAndDurationGeneration.R
IntervalDetector/lib.R
IntervalDetector/splitGIF.sh
```

## Running

### `CTpreprocess`
This tool generates the database of timestamps and aggregates them into events:
```
CT data processing tool. Author: Arthur Gilly (arthur.gilly@protonmail.ch)

Options:
        -h, --help
                Show this help message and exit

        -i INPUTDIR, --inputdir=INPUTDIR
                [REQUIRED] Input directory. This should contain locations, which should contain CT subdirs. Any downstream directory structure will be ignored.

        -o OUTBASE, --outbase=OUTBASE
                [REQUIRED] Base for naming output files. "-o test" will generate test.csv.gz, etc.

        -t NUMBER, --interval=NUMBER
                Maximum time between snapshots to trigger an independent event, in minutes. [default 30 min]

        -l, --location
                Consider all cameras to be dependent at a given location [default FALSE].
```

Please note the `--location` argument.

### `makegifs.sh`

The command is very simple, it uses the output of the above and makes movies that will be displayed in the app:

```
Usage: ./makegifs.sh input.csv out.filename.base
```

The first argument is the output of `CTpreprocess` and the second is a prefix that will be appended to the output. You can use the landscape name and year for this e.g. `NKD2022`.

### Organizing data

TBD.
