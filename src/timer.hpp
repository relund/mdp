/**
 * @file timer.hpp
 * Header file for the timer class.
 *
 * Inspired by how time is measured in the microbenchmark R package that use a
 * (BSD 2-Clause License)
 *
 * Current version at <github>
*/
#ifndef TIMER_HPP
#define TIMER_HPP

#include <stdint.h>
#include <limits>
#include <iostream>
#include <string>
#if defined(WIN32) || defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#elif defined(__MACH__) || defined(__APPLE__)
#include <mach/mach_time.h>
#elif defined(linux) || defined(__linux) || defined(__FreeBSD__) || defined(__OpenBSD__)
#include <time.h>
#elif defined(sun) || defined(__sun) || defined(_AIX)
#if defined(sun) || defined(__sun)
#define __EXTENSIONS__
#endif
# include <sys/time.h>
#else /* Unsupported OS */
#error "Unsupported OS."
#endif

#if defined(__GNUC__)
#define NOINLINE  __attribute__((noinline))
#else
#define NOINLINE
#endif

//-----------------------------------------------------------------------------

typedef uint64_t nanotime_t;

/**
* Class for measuring CPU time (Timer). Can be used on most OS.
* \note The measure unit is nanoseconds (10e-9 of a second) which are stored as a uint64_t,
* i.e. on my 64-bit windows machine maximum time measured can be 18446744073709551615
* nanoseconds = 213504 days.
*
* \author Lars Relund.
* \version 1.0
*/
class Timer
{
public:

/** Initialize the timer. i.e. reset to zero.
*/
Timer() {Reset();}


/** Reset the timer to zero. */
void Reset() { startT = endT = elapsedT = cumulT = 0; }


/** Return the time between first \code StartTimer() and \code last StopTimer() call.
* \see StartTimer
* \see StopTimer
*/
nanotime_t CumulativeTime() const { return cumulT; }


/** Return the time between first \code StartTimer() and \code last StopTimer() call.
* \see StartTimer
* \see StopTimer
*/
double CumulativeTime(std::string unit) const {
    if (unit=="nano") return double(cumulT);
    else if (unit=="micro") return double(cumulT)/1000;
    else if (unit=="mili")  return double(cumulT)/1000000;
    else if (unit=="sec")   return double(cumulT)/1000000000LL;
    else if (unit=="min")   return double(cumulT)/(1000000000LL*60);
    else if (unit=="hour")  return double(cumulT)/(1000000000LL*60*60);
    else if (unit=="day")   return double(cumulT)/(1000000000LL*60*60*24);
    std::cerr << "Time unit not defined! Use 'nano', 'micro', 'mili', 'sec', 'min', 'hour' or 'day'\n";
    return -1;
}


/** Return the time between last \code StartTimer() and \code StopTimer() call.
* \see StartTimer
* \see StopTimer
*/
double ElapsedTime() const { return elapsedT; }


/** Return the time between last \code StartTimer() and \code StopTimer() call.
* \see StartTimer
* \see StopTimer
*/
double ElapsedTime(std::string unit) const {
    if (unit=="nano") return double(elapsedT);
    else if (unit=="micro") return double(elapsedT)/1000;
    else if (unit=="mili")  return double(elapsedT)/1000000;
    else if (unit=="sec")   return double(elapsedT)/1000000000LL;
    else if (unit=="min")   return double(elapsedT)/(1000000000LL*60);
    else if (unit=="hour")  return double(elapsedT)/(1000000000LL*60*60);
    else if (unit=="day")   return double(elapsedT)/(1000000000LL*60*60*24);
    std::cerr << "Time unit not defined! Use 'nano', 'micro', 'mili', 'sec', 'min', 'hour' or 'day'\n";
    return -1;
}
/** Start the timer.
* \see StopTimer
*/
void StartTimer() { startT = GetNanoTime(); }


/** Stop the timer.
* \see StartTimer
*/
void StopTimer()
{
    endT = GetNanoTime();
    elapsedT = endT - startT;
    cumulT += elapsedT;
}

/** Max time which can be measured. */
nanotime_t MaxTime() { return std::numeric_limits<uint64_t>::max(); }

private:

/** Function to get the current time. Different depending on OS. */
#if defined(WIN32) || defined(_WIN32) || defined(_WIN64)
static nanotime_t GetNanoTime(void) {
    LARGE_INTEGER time_var, frequency;
    QueryPerformanceCounter(&time_var);
    QueryPerformanceFrequency(&frequency);

    /* Convert to nanoseconds */
    return 1.0e9 * time_var.QuadPart / frequency.QuadPart;
}
#elif defined(__MACH__) || defined(__APPLE__)
/* see http://developer.apple.com/library/mac/#qa/qa2004/qa1398.html */
static nanotime_t GetNanoTime(void) {
    uint64_t time;
    mach_timebase_info_data_t info;

    time = mach_absolute_time();
    mach_timebase_info(&info);

    /* Convert to nanoseconds */
    return time * (info.numer / info.denom);
}
#elif defined(linux) || defined(__linux) || defined(__FreeBSD__) || defined(__OpenBSD__)
static const nanotime_t nanoseconds_in_second = 1000000000LL;
static nanotime_t GetNanoTime(void) {
    struct timespec time_var;

    /* Possible other values we could have used are CLOCK_MONOTONIC,
     * which is takes longer to retrieve and CLOCK_PROCESS_CPUTIME_ID
     * which, if I understand it correctly, would require the R
     * process to be bound to one core.
     */
    clock_gettime(CLOCK_MONOTONIC, &time_var);

    nanotime_t sec = time_var.tv_sec;
    nanotime_t nsec = time_var.tv_nsec;

    /* Combine both values to one nanoseconds value */
    return (nanoseconds_in_second * sec) + nsec;
}
#elif defined(sun) || defined(__sun) || defined(_AIX)
/*
 * Solaris does not define gethrtime if _POSIX_C_SOURCE is defined because
 * gethrtime() is an extension to the POSIX.1.2001 standard. According to [1]
 * we need to define __EXTENSIONS__ before including sys/time.h in order to
 * force the declaration of non-standard functions.
 *
 * [1] http://www.oracle.com/technetwork/articles/servers-storage-dev/standardheaderfiles-453865.html
 */
static nanotime_t GetNanoTime(void) {
    return gethrtime();
}
#else /* Unsupported OS */
#error "Unsupported OS."
#endif

nanotime_t startT, endT, elapsedT, cumulT;
};

//-----------------------------------------------------------------------------

#endif
