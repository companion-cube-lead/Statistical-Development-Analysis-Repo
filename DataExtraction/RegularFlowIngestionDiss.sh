#!/bin/bash

module add apps/R/3.5.2

Rscript ScheduledScript.R

Rscript ScheduledAzureUpload.R

module purge