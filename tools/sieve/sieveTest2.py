# miniFE based sample sst configuration file
import sst
import os
from optparse import OptionParser

#set the number of threads
os.environ['OMP_NUM_THREADS']="16"

sst_root = os.getenv( "SST_ROOT" ) # Path to the sst root directory containing the tools
sst_tool_root = os.getenv( "SST_TOOLS_PATH" ) # Path to the sst root directory containing the tools
print "Tool ROOT:" + sst_tool_root
#fsroot = os.getenv( "FS_ROOT" )    # File system root
experiment_root = "/home/afrodri/sst-tools/tools/sieve" # Path to the experiment folder
#omp_root = fsroot + "/src/libomp_oss/tmp/lin_32e-rtl_5_nor_dyn.deb.41.c0.s0-volta" # Intel OpenMP built using "debug" mode
omp_root = ""

memDebug = 0
memDebugLevel = 7
baseclock = 2000  # in MHz
clock = "%g MHz"%(baseclock)
busLat = "50ps"

# For Ariel, may not be necessary
memSize = 8192 # in MB"
pageSize = 4  # in KB"
num_pages = memSize * 1024 / pageSize

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
        "envparamval0" : sst_tool_root + "/sieve/libbtmalloc.so",
        "envparamname1" : "LD_LIBRARY_PATH",
        "envparamval1" : omp_root + ":" + sst_tool_root + "/ariel/api:" + os.environ['LD_LIBRARY_PATH'],
        "envparamname2" : "CUTOFF_SIZE", # cut off size for allocations to be traced by btmalloc.cpp
        "envparamval2" : 64,
        #"executable": "/home/afrodri/sstminiapps/benchmarks/stream/stream_ae", # application
        #"executable": "/home/afrodri/sstminiapps/benchmarks/stream/malloc_ae", # application
        #"appargcount" : 0,   # application arguments follow
        #"executable" : "/home/afrodri/sstminiapps/GTC-P/src/mpi/bench_gtc_serial_gcc",
        #"appargcount" : 3,   # application arguments follow
        #"apparg0" : "/home/afrodri/sstminiapps/GTC-P/run/A.txt",
        #"apparg1" : "30",
        #"apparg2" : "64"

        "executable": "/home/afrodri/sstminiapps/CoMD/exmatex-CoMD-1e850ec/bin/CoMD-serial",
        "appargcount" : 2,   # application arguments follow    
        "apparg0" : "-N",
        "apparg1" : "3"

        #"appargcount" : 6,   # application arguments follow
        #"apparg0" : "-nx",
	#"apparg1" : options.nelem,
	#"apparg2" : "-ny",
	#"apparg3" : options.nelem,
	#"apparg4" : "-nz",
	#"apparg5" : options.nelem
    })

# ariel cpu
ariel = sst.Component("a0", "ariel.ariel")
ariel.addParams(minifeArgs)
ariel.addParams({
    "verbose" : 1,
    "alloctracker" : 1,
    "clock" : clock,
    "maxcorequeue" : 256,
    "maxissuepercycle" : 2,
    "pipetimeout" : 0,
    "corecount" : corecount,
    "arielmode" : 1,
    #"arieltool" : sst_tool_root + "/ariel/fesimple/fesimple.so",
    #"arieltool" : sst_tool_root + "/ariel/femlm/femlmtool.so",
    "arieltool" : "/home/afrodri/sst/sst/elements/ariel/fesimple.so",
    "memorylevels" : 1,
    "pagecount0" : num_pages,
    "vtop_transpate": "yes",
    "defaultlevel" : 0
})

def doQuad(quad, cores):
    sst.pushNamePrefix("q%d"%quad)

    for x in range(cores):
        core = 4*quad + x
        
        arielL1Link = sst.Link("cpu_cache_link_%d"%core)
        arielL1Link.connect((ariel, "cache_link_%d"%core, busLat), (sieveId, "cpu_link_%d"%core, busLat))
        arielALink = sst.Link("cpu_alloc_link_%d"%core)
        arielALink.connect((ariel, "alloc_link_%d"%core, busLat), (sieveId, "alloc_link", busLat))
        
        #        sieveId.addParams(sievePrefetchParams)
        
        sst.popNamePrefix()


sieveId = sst.Component("sieve", "memHierarchy.Sieve")
sieveId.addParams({
    "cache_size": "256KB",
    "associativity": 16,
    "cache_line_size": 64,
    "output_file" : "mallocRank.txt"
})    

for x in range(quads-1):
    doQuad(x, 4)
doQuad(quads-1, corecount-(4*(quads-1)))


statoutputs = dict([(1,"sst.statOutputConsole"), (2,"sst.statOutputCSV"), (3,"sst.statOutputTXT")])

sst.setStatisticLoadLevel(7)
sst.setStatisticOutput(statoutputs[2])

print "done configuring SST"

