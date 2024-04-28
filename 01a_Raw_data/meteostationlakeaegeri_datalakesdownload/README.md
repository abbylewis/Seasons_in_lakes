# Lake Aegeri Monitoring

## Project Information

The data is used and displayed on the [Datalakes website](https://www.datalakes-eawag.ch/).

## Sensors

The Lake Aegeri monitoring station consists of a profiler, meteostation and extra water temperature sensors. 

## Installation

## Installation

**You need to have [git](https://git-scm.com/downloads) and [git-lfs](https://git-lfs.github.com/) installed in order to successfully clone the repository.**

- Clone the repository to your local machine using the command: 

 `git clone --depth 1 https://renkulab.io/gitlab/eawag-surf/lake-aegeri-monitoring.git`
 
 Note that the repository will be copied to your current working directory.

- Use Python 3 and install the requirements with:

 `pip install -r requirements.txt`

 The python version can be checked by running the command `python --version`. In case python is not installed or only an older version of it, it is recommend to install python through the anaconda distribution which can be downloaded [here](https://www.anaconda.com/products/individual). 

## Code

[![License: MIT][mit-by-shield]][mit-by]

### Process new data

In order to process new data locally on your machine the file path needs to be adapted to your local file system. The following steps are therefore necessary: 

- Edit the `automate.sh` file. Change all the directory paths to match your local file system. This file contains all the file paths necessary to launch the scripts.

- Edit the `scripts/input_python.py` file. Change all the directory paths to match your local file system. This file contains all the directories where the python script outputs data to.

To process new data from the sftp server, you need to create a creds.json file with the sftp credentials then run automate.sh. 

### Adapt/Extend data processing piepeline

The python script `scripts/main.py` defines the different processing steps while the python script `scripts/sensors.py` contains the python classes **Profiler** and **Timeseries** with all the corresponding 
class methods to process the data. To add a new processing or visualization step, a new class method can be created in the `sensors.py` file and the step can be added in `main.py` file.
Both above-mentioned python scripts are independent of the local file system.

## Data

[![CC BY 4.0][cc-by-shield]][cc-by] 

The data can be found in the folder `data`. The data is structured as follows:

### Data Structure

- **Level 0**: Raw data collected from the different sensors.

- **Level 1**: Raw data stored to NetCDF file where attributes (such as sensors used, units, description of data, etc.) are added to the data. Column with quality flags are added to the Level 1A data. Quality flag "1" indicates that the data point didn't pass the 
quality checks and further investigation is needed, quality flag "0" indicates that no further investiagion is needed.

- **Level 2**: The data from all the different depth levels is transformed into a temporal grid with a temporal spacing of 10 minutes and stored to a 
NetCDF file spanning over a time period of 10 days.



## Quality assurance

Quality checks include but are not limited to range validation, gradient check, data type checking and flagging missing data.

## Credentials

In order to download live data `creds_example.json` should be renamed to `creds.json` and completed.

## Docker

#### Build the docker environment

`docker build -t aegeri .`

#### Download data from S3

`docker run -v $(pwd):/repository --rm -t aegeri --download`

#### Upload data to S3

`docker run -e AWS_ACCESS_KEY_ID=****** -e AWS_SECRET_ACCESS_KEY=****** -v $(pwd):/repository --rm -t aegeri --upload`

#### Reprocess data

`docker run -v $(pwd):/repository --rm -t aegeri --reprocess`

#### Run live pipeline

`docker run -e AWS_ACCESS_KEY_ID=****** -e AWS_SECRET_ACCESS_KEY=****** -v $(pwd):/repository --rm -t aegeri`

Add to crontab 

`0,30 * * * * docker run -e AWS_ACCESS_KEY_ID=****** -e AWS_SECRET_ACCESS_KEY=****** -v /home/user/aegeri:/repository --rm aegeri`



[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-g.svg?label=Data%20License
[mit-by]: https://opensource.org/licenses/MIT
[mit-by-shield]: https://img.shields.io/badge/License-MIT-g.svg?label=Code%20License
