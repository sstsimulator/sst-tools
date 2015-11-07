// Copyright 2009-2015 Sandia Corporation. Under the terms
// of Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
// Government retains certain rights in this software.
//
// Copyright (c) 2009-2015, Sandia Corporation
// All rights reserved.
//
// This file is part of the SST software package. For license
// information, see the LICENSE file in the top level directory of the
// distribution.

// File:   printTrace.cpp

#include <iostream>
#include <fstream>
#include <string>
#include <cstring>
#include <cstdio>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <time.h>
#include <cassert>
#include <unordered_map>
#include <tuple>
#include <boost/type_traits/is_same.hpp>
#include <signal.h>
#include <execinfo.h>
#include <typeinfo>
#include <math.h>

// We can change the library default for the interval types by defining
#define BOOST_ICL_USE_STATIC_BOUNDED_INTERVALS
// prior to other inluces from the icl.
// The interval type that is automatically used with interval
// containers then is the statically bounded right_open_interval.

#include <boost/icl/interval_map.hpp>

// Improved assert which can sensibly print the strings passed
#define IASSERT(C,M)           \
  do {                         \
    if (!(C)) {		       \
      std::cout << __FILE__ << " (" <<  __LINE__ << "): " \
                << M << "\n";  \
      exit(0);                 \
    }	                       \
  } while (0)

// Suggestion style
enum SUGGESTION_STYLE { SUGGESTION_INORDER, SUGGESTION_SET };
static SUGGESTION_STYLE suggestStyle = SUGGESTION_SET;

using namespace boost;
using namespace boost::icl;

// type declarations
// (Callsite, access number, size)
//using annCallsiteT = std::tuple<std::string, size_t, size_t>; // annotated callsite type
class annCallsite : public std::tuple<std::string, size_t, size_t> {
public:
  annCallsite() : std::tuple<std::string, size_t, size_t>() { }
  annCallsite(const std::string str, const size_t acc, const size_t sz) : std::tuple<std::string, size_t, size_t>(str, acc, sz) { }
  // interval map uses this addition operator
  annCallsite& operator+=(const annCallsite rhs) {
    *this = rhs; // Copy the tuple
    return *this;
  }
};
using annCallsiteT =  class annCallsite;
  
// Interval type
using intvlT = interval<size_t>::type;
// map:   key - interval of type [size_t, size_t)
//      value - string
using va2srcT  = interval_map<size_t, annCallsiteT>;
//using va2srcT = interval_map<size_t, std::string>;
using annotationT = std::pair<size_t, size_t>;  // (access number, size)
using callsiteH_T = std::unordered_map<std::string, annotationT>; // call site hash table type to track the current annotation
using  histoT  = std::vector<std::string>;

using uniqAccessH_T = std::unordered_map<size_t, size_t>; // hash table type to record unique trip counts
//using annStructuresT = std::pair< uniqAccessH_T, std::vector<annotationT> >; // annotation structures
class annStructuresT : public std::pair< uniqAccessH_T, std::vector<annotationT> > {
public:
  annStructuresT() : std::pair< uniqAccessH_T, std::vector<annotationT> >() { }

  annStructuresT &
  operator=(const annotationT annotation) {
    size_t accessCount, size;
    std::tie( accessCount, size ) = annotation;
    // Check if the trip number (accessCount) is already present
    if ( ((*this).first).find(accessCount) == ((*this).first).end() ) {
      ((*this).first)[accessCount] = size; // Add the trip number
      ((*this).second).push_back( std::make_pair(accessCount, size) ); // populate the vector
    }
  }

  inline std::vector<annotationT> &
  getAnnotationsV() {
    return (*this).second;
  }
};

//using callsiteSetT = std::unordered_map<std::string, annStructuresT>;
class callsiteSetT : public std::unordered_map<std::string, annStructuresT> {
public:
  callsiteSetT() : std::unordered_map<std::string, annStructuresT>() { };
  
};


// Call sites and the set for each callsite access time
static callsiteSetT callsiteSet;
// Store the rank (preference) of the callsites (for fast malloc)
static std::vector<callsiteSetT::iterator> csOrderTable; // Call site ordering table

class Options {
public:
  enum ops {
    exe,
    bt,
    hp,
    l0,
    err
  };

  size_t l0_size;
  
  ops hashit(std::string const & op) {
    if ("exe"      == op) return exe;
    if ("bktrace"  == op) return bt;
    if ("hotpages" == op) return hp;
    if ("l0"       == op) return l0;
    return err;
  }
};



class FileNames {
public:
  std::string bt;      // backtrace with base and bounds for mallocs
  std::string bt4src;  // backtrace for addr2line
  std::string ct;      // cleantrace, output from addr2line
  std::string hp;      // hot pages histograms
  std::string exe;     // executable
  
  FileNames() {
    // Generate a random number between 1 and 200
    srand (time(NULL));
    auto num = rand() % 200 + 1;
    bt4src = std::string("bt4src") + std::to_string(num) + std::string(".txt");
    ct = std::string("ct") + std::to_string(num) + std::string(".txt");
  }

  void setNames(std::string const & exenm,
		std::string const & btnm,
		std::string const & hpnm) {
    exe = exenm;
    bt  = btnm;
    hp  = hpnm;
  }
  
};


size_t
loadHisto(std::string const & contents, size_t pos,
	  histoT & histo, size_t nentries,
	  const char mrkr[], std::string errmsg);

bool
binMalloc(va2srcT const & va2src, size_t begin, size_t end);

void
printSuggestionsSetView();


static Options options;
static FileNames filenames;



inline bool
is_exists(std::string const & fname) {
  struct stat buffer;
  return ( 0 == stat(fname.c_str(), &buffer) );
}


void
checkFiles(std::string const & exenm,
	   std::string const & btnm,
	   std::string const & hpnm) {
  IASSERT( is_exists(exenm) , std::string("Can't find the specified executable '") +
	   exenm + std::string("'"));
  IASSERT( is_exists(btnm) , std::string("Can't find the back trace file '") +
	   btnm + std::string("'"));
  IASSERT( is_exists(hpnm) , std::string("Can't find the hot pages file '") +
	   hpnm + std::string("'"));
}

void
loadConfig(const char * const cconfig){
  // Load the style string from the environment
  char *styleStr = getenv("SIEVE_SUGGESTION_STYLE");
  if (NULL != styleStr) {
    if (strcmp(styleStr, "SUGGESTION_INORDER") == 0) {
      suggestStyle = SUGGESTION_INORDER;
    } else  if (strcmp(styleStr, "SUGGESTION_SET") == 0) {
      suggestStyle = SUGGESTION_SET;
    } else {
      std::cout << "Invalid argument " << styleStr << " for the environment variable SIEVE_SUGGESTION_STYLE\n";
      std::cout << "Use either SUGGESTION_INORDER or SUGGESTION_SET\n";
      exit(0);
    }
  }
  
  std::string sconfig(cconfig); 
  IASSERT( is_exists(sconfig) , std::string("Can't find the specified config file '") +
	   sconfig + std::string("'"));

  std::ifstream config(sconfig);
  IASSERT( config.is_open() , std::string("Can't open the config file '") +
	  sconfig + std::string("'"));
  
  // read one option at a time
  std::string opname, opval, delim;
  std::string exenm, btnm, hpnm; 
  while (! config.eof() ) {
    config >> opname >> delim >> opval;
    // c++ goes past the last line unfortunately
    if (opname.empty()) break;

    std::string sizeStr = "B";
    std::string unitVal = "";
    size_t sizeVal = 0;
    size_t bpos;
    char *end;
    
    switch(options.hashit(opname)) {
    case Options::exe:
      exenm = std::move(opval);
      break;
    case Options::bt:
      btnm = std::move(opval);
      break;
    case Options::hp:
      hpnm = std::move(opval);
      break;
    case Options::l0:
      // Convert the size specified into multiples of 4 KB
      // This information will be helpful to allocate std::vector entries
      // to walk the histo information
      // We are looking for strings such as MB, GB
    
      bpos = opval.find(sizeStr);

      if ( std::string::npos == bpos) {
	// There is space between the number and the units
	sizeVal = std::strtoull(opval.c_str(), &end, 10);
	// We have the number. Now read the units.
	config >> unitVal;
      } else {
	// No space between the numbers and the units
	sizeStr = opval.substr( 0, bpos-1 );
        sizeVal = std::strtoull( sizeStr.c_str(), &end, 10 );
	unitVal = opval.substr( bpos-1, 2 ); // 2 chars wide
      }
      
      // Switch over the units
      switch (unitVal[0]) {
      case 'K':
	options.l0_size = sizeVal / 4; // In terms of 4KB
	break;
      case 'M':
	options.l0_size = sizeVal * 1024 / 4;
	break;
      case 'G':
	options.l0_size = sizeVal * 1024 * 1024 / 4;
	break;
      default:
	std::cout << "Invalid unit for size '" << unitVal << "'\n";
	std::cout << "The only units supported are 'KB', 'MB', and 'GB'"
	  ", all specified in upper case\n";
	exit(0);
      }
      break;
    default:
      std::cout << "Invalid option: " << opname << "\n";
      exit(0);
    }
    // if opname is not reset then the if test fails to do
    // its job
    opname = "";
  }

  checkFiles(exenm, btnm, hpnm);
  
  // set all the filenames
  filenames.setNames(exenm, btnm, hpnm);
  config.close();

}


// parse the input line for numbers separated by delimiters.
// strtok doesn't return empty strings. Therefore it is okay
// to supply whole words such as 'VA:' and 'size' as delimiters. 
// The parsed numbers are of different bases currently.
void
parseLine( std::string const & line, const char delim[],
		size_t vals[], size_t bases[], size_t nvals ) {
  char *pch, *end;
  char buf[64];
  int n = 0;

  // line.c_str() is a const. You need a modifiable buffer
  strcpy( buf, line.c_str() );
  pch = strtok( buf, delim );

  n = 0;
  // strtok doesn't return empty strings
  while ( NULL != pch ) {
    IASSERT ( n < nvals , "Too many values in the line" );
    vals[n] = std::strtoull( pch, &end, bases[n] );    
    pch = strtok( NULL, delim );
    ++n;
  }
  
}


void
callAddr2line() {
  char cmd[256];
  const char *c_bt, *c_bt4src, *c_ct, *c_exe;

  c_bt = filenames.bt.c_str();
  c_ct = filenames.ct.c_str();
  c_bt4src = filenames.bt4src.c_str();
  c_exe = filenames.exe.c_str();
  
  // Preprocess the backtrace file
  // Remove all the base and bound information from the mallocs and
  // leave only the hex addresses from backtrace behind for addr2line
  sprintf(cmd, "cp %s %s; sed -i '/VA:/d' %s",
	  c_bt, c_bt4src, c_bt4src);
  system(cmd);

  // Invoke addr2line and redirect the output to the clean trace file
  sprintf(cmd, "cat %s | addr2line -e %s > %s",
	  c_bt4src, c_exe, c_ct);
  system(cmd);

  // remove the temporary files
  sprintf(cmd, "rm -f %s", c_bt4src);
  system(cmd);

  return;
}


void
mapVA2src(va2srcT & va2src) {
  std::string lineB, lineC, nlineB;
  
  std::ifstream btfile(filenames.bt);
  std::ifstream ctfile(filenames.ct);

  // Make sure we have right open intervals
  // BOOST_STATIC_ASSERT((
  // 		       boost::is_same< interval_map<size_t>::interval_type
  // 		       , right_open_interval<size_t> >::value
  // 		       ));

  // BOOST_STATIC_ASSERT((
  // 		       boost::is_same< intvlT
  // 		       , right_open_interval<size_t> >::value
  // 		       ));

  std::string errmsg = "Can't open the backtrace file '" + filenames.bt;
  IASSERT ( btfile.is_open() , errmsg);
  std::getline(btfile, nlineB); // must be base-n-bound info

  size_t vals[2];
  size_t bases[2] = { 16, 10 };

  callsiteH_T callsites;
  while (!btfile.eof()) {
    int nVAstmt = 0; // number of source addresses corresponding to this VA
    bool bVA = false;

    lineB = std::move(nlineB);
    // goto the next base and bound line
    while (! bVA && ! btfile.eof()) {
      std::getline(btfile, nlineB);
      ('V' == nlineB[0]) ? bVA = true : ++nVAstmt;
    }
    // Hex values contain 'a' and 'e'. Assuming the hex values are all lower case,
    // we can get away by using the uppercase 'A' and 'E' in the delimiter.
    const char delim[] = "VA:SIZE, ";
    parseLine( lineB, delim, vals, bases, 2 );
    auto base = vals[0];
    auto size = vals[1];
    auto intvl = interval<size_t>::right_open( base, base + size );

    std::string srcLines = "";
    for (int i = 0; i < nVAstmt; ++i) {
      std::getline(ctfile, lineC);
      if (lineC[0] != '?')  srcLines += lineC + "\n";
    }

    // Get the access number for a call site as seen by the malloc trace
    if (callsites.find(srcLines) == callsites.end()) callsites[srcLines] = std::make_pair(0, size); // Initialize call site
    annCallsiteT callsite = annCallsite( srcLines, ++(callsites[srcLines].first), size );
    auto VAentry = std::make_pair( intvl, callsite );
    
    // If the keys (ranges) are identical then the values (srcLines) are appended
    // in the map. Eg: va2src += std::pair( [100, 110) , string("i1") ) and
    // va2src += std::pair( [100, 110), string("i2") ) will result in a single
    // entry [100, 110) -> "i1i2". This means the source lines for similar calls
    // such as creating small std::strings will all be lumped together. 
    // va2src += std::make_pair( intvl, srcLines );
    //
    // An overlapping interval will split the original interval.
    // Eg: va2src += std::pair( [80, 120), string("i1") )
    //     va2src += std::pair( [90,110), string("i2") )
    // Will result in [80, 91) -> "i1", [90,110) -> "i1i2", [110,120) -> "i1"
    // A better solution is to leave the larger allocation (interval) as is.
    // For matching intervals, remember only the latest value.
    // All this means that we can't simply use the convenient "+", "-" operators.
    // We must explicitly manage the interval insertion.
    auto itb = va2src.find(base);
    auto ite = va2src.find(base+size);

    // Check if the interval endpoints are unique
    if (va2src.end() == itb && va2src.end() == ite) {
      // Delete any intervals between the end points
      va2src -= intvl;
      va2src += VAentry;
      continue;
    }
    
    size_t bintvlSize = 0; // Interval enclosing "base"
    size_t eintvlSize = 0; // Interval enclosing "base+size"
    size_t blb = 0, bub = 0; // Bounds of the interval enclosing "base"
    size_t ulb = 0, uub = 0; // Bounds of the interval enclosing "base+size"
    
    // Note the intervals are unique. There can't be more than one interval
    // enclosing a single point at any time.
    if (va2src.end() != itb) {
      // Size of the interval enclosing the beginning "base"
      size_t lb = (*itb).first.lower();
      size_t ub = (*itb).first.upper();
      bintvlSize = ub - lb;
    }
    if (va2src.end() != ite) {
      // Size of the interval enclosing the end "base+size"
      size_t lb = (*ite).first.lower();
      size_t ub = (*ite).first.upper();
      eintvlSize = ub - lb;
    }
    // Check if the intervals are identical
    if (blb == base && bub == base + size) {
      // We are replacing the old value for this interval with srcLines,
      // the latest callsite
      va2src -= intvl;
      va2src += VAentry;
    }
    // Else
    if (bintvlSize < size && eintvlSize < size) {
      // The new intvl is clearly bigger than all the existing intervals
      // containing its range.
      // Add the new interval to the map
      bool e_same_b = false; 
      if (itb == ite) e_same_b = true; // e_intvl and b_intvl are the same
      if (va2src.end() != itb) 	va2src.erase(*itb);
      if (va2src.end() != ite && ! e_same_b) va2src.erase(*ite);
      // Delete all the intervals enclosed by intvl
      va2src -= intvl;

      // Now add the interval 'intvl' to the map
      va2src += VAentry;
    }
  }

  btfile.close();
  ctfile.close();
  
  // remove the cleanTrace file
  std::string cmd;
  cmd = "rm -rf " + filenames.ct;
  system(cmd.c_str());
}

void
loadHistos(histoT & rdhisto,
	   histoT & wrhisto) {

  size_t nentries = options.l0_size*4;  
  // The hot pages trapped might not actually coem from the heap.
  // However we are going to draw a line, and not going to look
  // past l0_size*4 entries from each histogram. If the heap pages
  // don't make it in the selected entries then it's probably not
  // worth performing a fast malloc on them.
  // Reserve at least l0_size*4 entries for both the tables.
  // This might appear to be a lot of space, but reserving the heap
  // without actually using it is not going to hurt us.
  rdhisto.reserve(nentries);
  wrhisto.reserve(nentries);
    
  std::ifstream hpfile(filenames.hp); // Open the hot pages files

  std::string contents;
  hpfile.seekg(0, std::ios::end);
  contents.resize(hpfile.tellg());
  hpfile.seekg(0, std::ios::beg);
  hpfile.read(&contents[0], contents.size());

  char rdmrkr[] = "read bins sorted";
  char wrmrkr[] = "write bins sorted";

  std::string rderrmsg = "Can't find the read bins in the hot pages file\n";
  std::string wrerrmsg = "Can't find the write bins in the hot pages file\n";

  size_t pos = 0;
  pos = loadHisto( contents, pos, rdhisto, nentries, rdmrkr, std::move(rderrmsg) );
  pos = loadHisto( contents, pos, wrhisto, nentries, wrmrkr, std::move(wrerrmsg) );

  hpfile.close();
}

size_t
loadHisto(std::string const & contents, size_t pos,
	  histoT & histo, size_t nentries,
	  const char mrkr[], std::string errmsg) {
  pos = contents.find( mrkr, pos );
  IASSERT ( std::string::npos != pos , errmsg );

  // find the first bin
  pos = contents.find( '[', pos );
  size_t nxpos = pos; // initialize next pos

  // populate the table
  for (size_t i = 0; i < nentries; ++i) {
    nxpos = contents.find( '\n', pos );
    histo.push_back( std::move( contents.substr( pos, nxpos-pos ) ) );

    // Check if we are out of read bins
    if ( '[' != contents[nxpos+1] ) break;
    // Else move to the next line
    pos = nxpos+1;
  }

  return nxpos;
}

static size_t nrepeats = 0;

void
binMallocs(va2srcT const & va2src,
	   histoT const & rdhisto,
	   histoT const & wrhisto) {
  size_t rvals[3] = { 0, 0, 0 };
  size_t wvals[3] = { 0, 0, 0 };
  size_t bases[3] = { 10, 10, 10 };
  const char delim[] = "[], ";

  enum {
    BEGIN = 0,
    END,
    COUNT
  };
  
  int rIdx = 0, wIdx = 0;
  int prIdx = -1, pwIdx = -1;
  int rSize = rdhisto.size();
  int wSize = wrhisto.size();
  
  size_t allocSize = 0;
  size_t l0_sizeB = options.l0_size * 4 * 1024; // size in bytes
  // iterate over the entries of the histograms
  while ( (rIdx < rSize || wIdx < wSize)
	  && allocSize < l0_sizeB ) {
    // printf("rIdx = %d,  wIdx = %d\n", rIdx, wIdx);
    // Read pointer advanced: parse a new line
    if ( rIdx > prIdx && rIdx < rSize ) {
      parseLine( rdhisto[rIdx], delim, rvals, bases, 3 );
    }
    // Write pointer advanced: parse a new line
    if ( wIdx > pwIdx && wIdx < wSize )
      parseLine( wrhisto[wIdx], delim, wvals, bases, 3 );
    prIdx = rIdx;
    pwIdx = wIdx;
    bool balloc = false;
    // std::cout << "rvals: " << rvals[BEGIN]
    // 	      << "   " << rvals[END] << "  " << rvals[COUNT] << "\n";
    // std::cout << "wvals: " << wvals[BEGIN]
    // 	      << "   " << wvals[END] << "  " << wvals[COUNT] << "\n";

    // Check if the current read bin or the current write
    // bin has the highest count
    if ( rIdx < rSize && (rvals[COUNT] > wvals[COUNT] || wIdx >= wSize) ) {
      // See if this address range intersects with va2src
      balloc = binMalloc( va2src, rvals[BEGIN], rvals[END] );
      ++ rIdx;
      if ( balloc ) allocSize += (rvals[END] - rvals[BEGIN]);
    } else {
      // See if this address range intersects with va2src
      balloc = binMalloc( va2src, wvals[BEGIN], wvals[END] );
      ++ wIdx;
      if ( balloc ) allocSize += (wvals[END] - wvals[BEGIN]);
    }
  }

  if (SUGGESTION_SET == suggestStyle) printSuggestionsSetView();
}
	   
bool
binMalloc(va2srcT const & va2src, size_t begin, size_t end) {
  va2srcT res;
  intvlT page = interval<size_t>::right_open( begin, end );  
  res = va2src & page;
  
  for (va2srcT::iterator it = res.begin(), nd = res.end();
       it != nd; ++ it) {
    std::string srcLines;
    size_t accessCount, size;
    std::tie(srcLines, accessCount, size) = it->second; // unpack the call site
    switch(suggestStyle) {
    case SUGGESTION_INORDER:
      std::cout << "n-th time the callsite is accessed : " << accessCount << "\n";
      std::cout << "size = " << size << "\n";
      std::cout << srcLines << "\n";
      break;
    default:
      if (callsiteSet.end() == callsiteSet.find(srcLines)) {
	// New entry
	callsiteSet[srcLines] = std::make_pair(accessCount, size);
	csOrderTable.push_back(callsiteSet.find(srcLines)); // Store the iterator for srcLines in orderTable
      } else {
	// Add the current access count, if unique, to the end of the srcLines set
	callsiteSet[srcLines] = std::make_pair(accessCount, size);
      }
    }
  }
  return (! res.empty());
}

void
printSizeStats(std::vector<size_t> const & sizeV) {
  std::unordered_map<size_t, size_t> freqTab; // frequency table to compute mode
  
  size_t nelems = sizeV.size();
  size_t szSum = 0;
  int i=0;
  for (const size_t &sz : sizeV) {
    szSum += sz;
    if ( freqTab.find(sz) == freqTab.end() ) {
      freqTab[sz] = 1;
    } else {
      ++freqTab[sz];
    }
  }
  // std::cout << " szSum = " << szSum << ", nelems = " << nelems;

  double meanSz = szSum / nelems; // nelems > 1
  size_t diffSqSum = 0;
  // variance
  for (const size_t &sz : sizeV) {
    diffSqSum += (meanSz - sz) * (meanSz - sz);
  }
  double szVariance = (double) diffSqSum / nelems;
  double stdDev = sqrt (szVariance);

  size_t maxFq = 0;
  size_t modeSz = 0;
  for (const auto const &it : freqTab) {
    if (it.second > maxFq) {
      modeSz = it.first;
      maxFq  = it.second;
    }
  }

  std::cout << "Allocation size Stats: mean = " << meanSz << ", standard deviation = "
	    << stdDev << ", population size = " << nelems << ", mode = " << modeSz;
  return;
}

void
printSuggestionsSetView() {
  for (auto const & it : csOrderTable) {
    // Print the sets ranked from highest to lowest
    std::cout << it->first; // print the callsite
    std::vector<annotationT> &annotationsV = (it->second).getAnnotationsV();
    
    size_t nelems = annotationsV.size(); // total number of annotations for this callsite

    // Note: the trip numbers (access counts) in the annotations vector are now unique
    // because of the modifications to callsiteSetT. This implies that all the consecutive
    // ranges, or elements, of trip numbers are all unique. We do not have to worry about
    // constructing unique intervals or elements below
    size_t rngbeg, sz;
    std::tie(rngbeg, sz) = annotationsV[0]; // beginning of the range. You are guaranteed to have at least one element.
    size_t rngend = rngbeg;
    
    std::vector<size_t> sizeV; // Vector to store the sizes for computing statistics
    sizeV.push_back(sz);

    std::cout << "Access times: ";
    char *separator = ""; // Empty initial separator
    // Discover the ranges
    for (int i=1; i<nelems; ++i) {
      // Keep stepping until the next trip number is the same or one greater than the previous trip number
      // annotationT = std::pair (trip number, size)
      while ( annotationsV[i].first == annotationsV[i-1].first
	      || annotationsV[i].first == annotationsV[i-1].first + 1
	      && i < nelems) {
	sizeV.push_back( annotationsV[i].second );
	++i;
      }

      rngend = annotationsV[i-1].first; // True for both i<nelems and i==nelems

      // Construct the range string
      if (rngend > rngbeg) {
	std::cout << separator << "[" << rngbeg << "," << rngend << "]"; // Print the unique subranges
	separator = ", ";
      } else {
	// rngbeg == rngend
	std::cout << separator << rngbeg;
      }
      separator = ", ";
      if ( i < nelems ) {
	// Have not reached the end of the annotations yet
	rngbeg = annotationsV[i].first;
	sizeV.push_back( annotationsV[i].second );
      }
    }

    // If the last interval is a singleton then we need to print the element here
    // If the last interval has more than one element then rngend > rngbeg, and
    // the logic inside the above loop would have properly handled the case.
    if ( rngbeg >= rngend )  std::cout << separator << rngbeg;
    
    std::cout << "\n";
    printSizeStats(sizeV);
    std::cout << "\n\n";
  }
}


void
report_backtrace_handler(int sig) {
  void *trace[32];

  size_t size = backtrace(trace, 32);
  char** strings = backtrace_symbols(trace, size);

  printf("Currently at:\n");
  for (int i=0; i<size; ++i) {
    char cmd[128];
    char delim[] = "[]"; // the address will be within the square brackets [ ]
    char *pch = strtok(strings[i], delim); // the string before the open square bracket [
    pch = strtok(NULL, delim); // the address
    sprintf(cmd, "addr2line -e pt -a %s", pch);
    system(cmd);
  }
  exit(-1);
}

void
registerHandler() {
  struct sigaction act;

  //sigemptyset(&act.sa_mask);
  //act.sa_flags = 0;
  memset((void *) &act, 0, sizeof(act));
  act.sa_handler = report_backtrace_handler;

  sigaction(SIGINT, &act, NULL);
  sigaction(SIGQUIT, &act, NULL);
}

int
main(int argc, char *argv[]) {
  assert (argc >= 3 && ( 0 == strcmp(argv[1], "-c") )
	  && "Supply the config file as -c <config>");
  loadConfig(argv[2]);
  registerHandler();

  callAddr2line();

  va2srcT va2src;
  mapVA2src(va2src);

  histoT rdhisto, wrhisto;
  loadHistos( rdhisto, wrhisto );

  binMallocs( va2src, rdhisto, wrhisto );
  
  return 0;
}
