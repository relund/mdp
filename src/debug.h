#ifndef DEBUG_HPP
#define DEBUG_HPP

// Set debug level below for debug output to R. A function is defined for each level and higher levels
// give more output. For each level function DBG<level> is defined
//
// Example. DBG2(endl << "  Calc prM (iR,t,iSW,iSG)=" << getLabel(iRation,t,iSWt,iSGt) << ": ")

//#define RDEBUG
//#define RDEBUG1
//#define RDEBUG2
//#define RDEBUG3
//#define DEBUG4


// Commands for debug. Use all debug levels to get most output info
//-----------------------------------------------------------------------------
/*
// debug first level
#ifdef RDEBUG
#define DBG(x) do { Rcout << x; } while (0);
#else
#define DBG(x)
#endif

// debug second level
#ifdef RDEBUG1
#define DBG1(x) do { Rcout << x; } while (0);
#else
#define DBG1(x)
#endif

// debug third level
#ifdef RDEBUG2
#define DBG2(x) do { Rcout << x; } while (0);
#else
#define DBG2(x)
#endif

// debug 4. level
#ifdef RDEBUG3
#define DBG3(x) do { Rcout << x; } while (0);
#else
#define DBG3(x)
#endif*/

// debug 5. level
#ifdef DEBUG4
#define DBG4(x) do { cout << x; } while (0);
#else
#define DBG4(x)
#endif

#endif
