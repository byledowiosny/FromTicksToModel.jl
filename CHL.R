#!/usr/bin/env Rscript

setwd("/home/jerzy/data/EURUSD-daily")
#Load libraries quietly.
suppressMessages(require(data.table))
suppressMessages(require(LaplacesDemon))
suppressMessages(require(MASS))
suppressMessages(require(optparse))
#######################################################
option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="dataset file name", metavar="character"),
  make_option(c("-o", "--out"), type="character", default="CMM.dat", 
              help="output file name [default= %default]", metavar="character")
);
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);
#######################################################
CHL <- fread(opt$file)
Y.orig <- as.matrix(CHL[,2:4])
Y <- diff(log(Y.orig[,]))
Y.scales <- sqrt(.colVars(Y))
Y <- Y / matrix(Y.scales, nrow(Y), ncol(Y), byrow=TRUE)
write.matrix(format(Y, scientific=FALSE), file = opt$out, sep=",")
