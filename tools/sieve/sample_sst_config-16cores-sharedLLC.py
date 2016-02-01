# stream based sample sst configuration file
# for using Sieve to model a 16-core processor
# with a shared LLC
import sst
import os
from optparse import OptionParser

#set the number of threads
os.environ['OMP_NUM_THREADS']="16"

sst_root = os.getenv( "SST_ROOT" ) # Path to the sst root directory containing the tools
fsroot = os.getenv( "FS_ROOT" )    # File system root
tools_root = os.getenv( "SST_TOOLS_ROOT" )
experiment_root = fsroot + "/stream/stream" # Path to the experiment folder

memDebug = 0
memDebugLevel = 7
baseclock = 2000  # in MHz
clock = "%g MHz"%(baseclock)
busLat = "100ps"

memSize = 8192 # in MB"
pageSize = 4  # in KB"
num_pages = memSize * 1024 / pageSize

sieveProfilerParams = {
        "profiler": "cassini.AddrHistogrammer",
        "profiler.addr_cutoff" : "16GiB" # Cutoff - don't profile virtual addresses over 16 GiB
        }

op = OptionParser()
op.add_option("-n", "--num_elems", action="store", type="int", dest="nelem", default=50)
(options, args) = op.parse_args()

#memDebug = options.memDebug
corecount = 16

minifeArgs = ({
        "envparamcount" : 3,             # environment variables
        "envparamname0" : "LD_PRELOAD",  # For weak dynamic linking
        "envparamval0" : tools_root + "/tools/sieve/libbtmalloc.so",
        "envparamname1" : "LD_LIBRARY_PATH",
        "envparamval1" : tools_root + "/tools/ariel/api:" + os.environ['LD_LIBRARY_PATH'],
        "envparamname2" : "CUTOFF_SIZE", # cut off size for allocations to be traced by btmalloc.cpp
        "envparamval2" : 64,
        "executable": experiment_root, # application
    })

# ariel cpu
ariel = sst.Component("a0", "ariel.ariel")
ariel.addParams(minifeArgs)
ariel.addParams({
	"verbose" : 0,
        "clock" : clock,
	"maxcorequeue" : 256,
 	"maxissuepercycle" : 2,
 	"pipetimeout" : 0,
	"corecount" : corecount,
 	"arielmode" : 1,
 	"memorylevels" : 1,
        "pagecount0" : num_pages,
	"defaultlevel" : 0
    })

sieve = sst.Component("sieve", "memHierarchy.Sieve")
sieve.addParams({
            "cache_size": "32MB",
            "associativity": 16,
            "cache_line_size": 64,
})
sieve.addParams(sieveProfilerParams)
def doCores(cores):
    for x in range(cores):
        arielL1Link = sst.Link("cpu_cache_link_%d"%x)
        arielL1Link.connect((ariel, "cache_link_%d"%x, busLat), (sieve, "cpu_link_%d"%x, busLat))


doCores(corecount)


statoutputs = dict([(1,"sst.statOutputConsole"), (2,"sst.statOutputCSV"), (3,"sst.statOutputTXT")])

sst.setStatisticLoadLevel(7)
sst.setStatisticOutput(statoutputs[2])
sst.enableStatisticForComponentType("memHierarchy.Sieve",
                                    "histogram_reads",
                                        {"type":"sst.HistogramStatistic",
                                         "minvalue" : "0",       # The beginning virtual address to track
                                         "numbins"  : "4000000", # The ending virtual address relative to the beginning address
                                         "binwidth" : "4096"
                                         #"rate" : "100ns"
                                         })
sst.enableStatisticForComponentType("memHierarchy.Sieve",
                                    "histogram_writes",
                                        {"type":"sst.HistogramStatistic",
                                         "minvalue" : "0",
                                         "numbins"  : "4000000", 
                                         "binwidth" : "4096"
                                         })


print "done configuring SST"

