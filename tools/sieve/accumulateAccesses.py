# Copyright 2009-2015 Sandia Corporation. Under the terms
# of Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
# Government retains certain rights in this software.
#
# Copyright (c) 2009-2015, Sandia Corporation
# All rights reserved.
#
# This file is part of the SST software package. For license
# information, see the LICENSE file in the top level directory of the
# distribution.

# File:   accumulateAccesses.py

# This script sums the page accesses leaving the sieves in a node.
# If there is more than one node simulated then we need to sum the
# accesses on a per node basis. It basically means that this script
# needs to be modified if more than one node is being simulated.

# input: StatisticOutput.csv
# output: hotPages.txt


import re
import sys
import string


binCols = {} # Identify the columns where the histogram bins appear

# Identify the column numbers corresponding to the histogram bins
def findBins(line):
    cols = string.split(line, ',')
    patt = re.compile('Bin\d+:(\d+-\d+)\.u64')

    totcols = len(cols)
    for i in range(totcols):
        result = patt.search(cols[i])
        if result:
            binCols[i] = result.group(1) # capture just the address range

    return totcols

# Accumulate the accesses for the read and write histograms
def populateHisto(histo, line):
    cols = string.split(line, ',')
    # Iterate over just the columns corresponding to the bins
    for idx in binCols:
        col = string.strip(cols[idx])
        histo[idx] = histo[idx] + int(col)

        
def main():
    hpFile = "hotPages.txt"
    hp = open(hpFile, "w")
    
    # Open the statististics file
    with open('StatisticOutput.csv', 'r') as f:
        # read the first header line
        line = f.readline()
        totcols = findBins(line)

        # Create lists for the accumulated read and write histograms
        rdHisto = [0] * totcols
        wrHisto = [0] * totcols

        for line in f:
            buf = line[0:100] # read the first 100 chars
            if string.find(buf, 'sieve') > -1:
                if string.find(buf, 'histogram_reads') > -1:
                    populateHisto(rdHisto, line)
                elif string.find(buf, 'histogram_writes') > -1:
                    populateHisto(wrHisto, line)
                else:
                    "Error: the sieve statistic must either be a read or write histogram"
                    sys.exit(0)

        sortRidx = sorted(range(totcols), key=lambda k:rdHisto[k], reverse=True)
        sortWidx = sorted(range(totcols), key=lambda k:wrHisto[k], reverse=True)

        hp.write("read bins sorted:\n")
        for i in sortRidx:
            if rdHisto[i] < 1:
                break

            bounds = string.split(binCols[i], '-')
            hp.write('[{0}, {1}] {2}\n'.format(bounds[0], bounds[1], rdHisto[i]))

        hp.write('\n')
        hp.write("write bins sorted:\n")
        for i in sortWidx:
            if wrHisto[i] < 1:
                break
            bounds = string.split(binCols[i], '-')
            hp.write('[{0}, {1}] {2}\n'.format(bounds[0], bounds[1], wrHisto[i]))

    hp.close()
        
            
main()
