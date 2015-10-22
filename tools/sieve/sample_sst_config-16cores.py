# miniFE based sample sst configuration file
import sst
import os
from optparse import OptionParser

#set the number of threads
os.environ['OMP_NUM_THREADS']="16"

sst_root = os.getenv( "SST_ROOT" ) # Path to the sst root directory containing the tools
fsroot = os.getenv( "FS_ROOT" )    # File system root
experiment_root = fsroot+"/src/sst-simulator/mytest/" # Path to the experiment folder
omp_root = fsroot + "/src/libomp_oss/tmp/lin_32e-rtl_5_nor_dyn.deb.41.c0.s0-volta" # Intel OpenMP built using "debug" mode

memDebug = 0
memDebugLevel = 7
baseclock = 2000  # in MHz
clock = "%g MHz"%(baseclock)
busLat = "50ps"

# For Ariel, may not be necessary
memSize = 8192 # in MB"
pageSize = 4  # in KB"
num_pages = memSize * 1024 / pageSize

sievePrefetchParams = {
        "prefetcher": "cassini.AddrHistogrammer",
        "prefetcher.addr_cutoff" : "16GiB" # Cutoff - don't profile virtual addresses over 16 GiB
        }

op = OptionParser()
op.add_option("-n", "--num_elems", action="store", type="int", dest="nelem", default=50)
(options, args) = op.parse_args()

#memDebug = options.memDebug
corecount = 16
quads = corecount / 4
if corecount % 4:
    quads = quads+1
    
minifeArgs = ({
        "envparamcount" : 3,             # environment variables
        "envparamname0" : "LD_PRELOAD",  # For weak dynamic linking
        "envparamval0" : sst_root + "/tools/sieve/libbtmalloc.so",
        "envparamname1" : "LD_LIBRARY_PATH",
        "envparamval1" : omp_root + ":" + sst_root + "/tools/ariel/api:" + os.environ['LD_LIBRARY_PATH'],
        "envparamname2" : "CUTOFF_SIZE", # cut off size for allocations to be traced by btmalloc.cpp
        "envparamval2" : 64,
        "executable": experiment_root+"/miniFE-2.0_openmp_opt/src/miniFE.x", # application
	"appargcount" : 6,   # application arguments follow
        "apparg0" : "-nx",
	"apparg1" : options.nelem,
	"apparg2" : "-ny",
	"apparg3" : options.nelem,
	"apparg4" : "-nz",
	"apparg5" : options.nelem
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
 	"arielmode" : 0,
        "arieltool" : sst_root + "/tools/ariel/fesimple/fesimple.so",
 	"memorylevels" : 1,
        "pagecount0" : num_pages,
	"defaultlevel" : 0
    })

def doQuad(quad, cores):
    sst.pushNamePrefix("q%d"%quad)

    for x in range(cores):
        core = 4*quad + x
        sieveId = sst.Component("sieve_%d"%core, "memHierarchy.Sieve")
        sieveId.addParams({
            "cache_size": "32KB",
            "associativity": 8,
            "cache_line_size": 64
            })
        arielL1Link = sst.Link("cpu_cache_link_%d"%core)
        arielL1Link.connect((ariel, "cache_link_%d"%core, busLat), (sieveId, "cpu_link", busLat))

        sieveId.addParams(sievePrefetchParams)

    sst.popNamePrefix()



for x in range(quads-1):
    doQuad(x, 4)
doQuad(quads-1, corecount-(4*(quads-1)))


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

