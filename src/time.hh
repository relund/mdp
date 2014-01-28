#ifndef TIME_HPP
#define TIME_HPP

#define CLOCK       // Defines that clock() function is used (okay for all OS)
                    // Exclude it if compiled on unix/linux (better to use the times() function)
//-----------------------------------------------------------------------------

#ifdef CLOCK
#include <ctime>           // For use of counting time (Windows)
#else
#include <sys/times.h>      // For use of counting time (Unix/Linux)
#include <sys/types.h>
#include <unistd.h>
#endif

#include <iostream>
using namespace std;
//-----------------------------------------------------------------------------
// Win:  CLOCKS_PER_SEC = 1000
// Unix uni: CLOCKS_PER_SEC = 1000000
// Unix uni: _SC_CLK_TCK = 100
//-----------------------------------------------------------------------------

/**
* Class for measuring CPU time (Time Manager). Can be used both on
* unix/linux and windows OS. If CLOCK is defined using a #define statement
* (see time.hpp) the time is counted using the clock() function. This works
* for all OS's. If CLOCK not defined time is counted using the times() function
* which works only on unix/linux systems.
* \note Not defining CLOCK is recormended since using the clock() function
* normally wrap after approx 36 minutes, i.e. cannot count cpu times afterwards.
* For the times() function it will wrap after approx 1.4 year. Therefore this is a
* better way. However, works only on linux/unix.
* \author Lars Relund.
* \version 3.1
*/
class TimeMan
{

public:

    /** Initialize an array of size 1 of time measures for storing the CPU times.
    */
    TimeMan()
    {
        aTimes = new clock_t[1];
        if (aTimes==NULL)
        {
            cout <<"Error: Insufficient memory, exit in TimeMan::TimeMan";
            exit(1);
        }
        aTDiff = new double[1];
        if (aTDiff==NULL)
        {
            cout <<"Error: Insufficient memory, exit in TimeMan::TimeMan";
            exit(1);
        }
        size=1;
        Reset();
    }

    /** Initialize an array [0,..,n-1] of size n of time measures for storing the CPU times.
    * \param n the size of the array.
    */
    TimeMan(int n)
    {
        aTimes = new clock_t[n];
        if (aTimes==NULL)
        {
            cout <<"Error: Insufficient memory, exit in TimeMan::TimeMan";
            exit(1);
        }
        aTDiff = new double[n];
        if (aTDiff==NULL)
        {
            cout <<"Error: Insufficient memory, exit in TimeMan::TimeMan";
            exit(1);
        }
        size=n;
        Reset();
    }

    /** Reset the time measure i to zero. */
    void Reset(int i){aTDiff[i]=0;}

    /** Reset all time measures to zero. */
    void Reset()
    {
        for(int i=0;i<size;i++) Reset(i);
    }

    /** Free memory on free store again. */
    ~TimeMan()
    {
        delete [] aTimes;
        delete [] aTDiff;
    }

    /** Number of time measures.
    * \return Number of time measures.
    */
    int Size() const {return size;}

    /** Return the total time difference for time measure i.
    * \pre Time measure i must have been set by the StartTime method and stopped by the StopTime method.
    * \param i Index for time measure i
    * \return The time difference for time measure i in seconds.
    * \see StartTime
    * \see StopTime
    */
    double GetTotalTimeDiff(int i) const
    {
        return aTDiff[i];
    }

    /** Stop and return the total time difference for time measure i.
    * \pre Time measure i must have been set by the StartTime method.
    * \param i Index for time measure i
    * \return The time difference for time measure i in seconds.
    * \see StartTime
    * \see StopTime
    */
    double StopAndGetTotalTimeDiff(int i)
    {
        StopTime(i);
        return aTDiff[i];
    }

    /** Return the time difference for time measure i since last started.
    * \pre Time measure i must have been set by the StartTime method.
    * \note Do not stop the timer!
    * \param i Index for time measure i
    * \return The time difference for time measure i in seconds since last started using StartTime.
    * \see StartTime
    * \see StopTime
    */
    double GetLocalTimeDiff(int i) const
    {
        return (double)(clock()-aTimes[i])/(double)CLOCKS_PER_SEC;
    }

#ifdef CLOCK

public:
    /** Start counting time for time measure i.
    * \param i Index for time measure i
    * \note Don't reset the time measure. Time already counted will be stored.
    */
    void StartTime(int i) {aTimes[i] = clock();}

    /** Stop counting the time for time measure i and add the time
    * used to time measure i.
    * The total time can be accessed by calling the TimeDiff function.
    * \note Don't reset the time measure. Can be started again with the StartTime method.
    * \pre Time measure i must have been set by StartTime method.
    * \param i Index for time measure i.
    * \see StartTime
    * \see TimeDiff
    */
    void StopTime(int i)
    {
        aTDiff[i]+=(double)(clock()-aTimes[i])/(double)CLOCKS_PER_SEC;
    }

#else

public:
    /** Start counting time for time measure i.
    * \param i Index for time measure i
    * \note Don't reset the time measure. Time already counted will be stored.
    */
    void StartTime(int i) {times(&buff);aTimes[i]=buff.tms_utime;}

    /** Stop counting the time for time measure i and add the time
    * used to time measure i.
    * The total time can be accessed by calling the TimeDiff function.
    * \note Don't reset the time measure.
    * \pre Time measure i must have been set by StartTime method.
    * \param i Index for time measure i.
    * \see StartTime
    * \see TimeDiff
    */
    void StopTime(int i)
    {
        times(&buff);
        aTDiff[i]+=((double)(buff.tms_utime)-(double)aTimes[i])/(double)(sysconf(_SC_CLK_TCK));
    }

private:

    struct tms buff;    // buffer to store time

#endif


private:

    clock_t * aTimes;   ///< An array for variables to measure cpu times.
    double * aTDiff;    ///< An array for storing the cpu times.
    int size; ///< The size of the array.


};

//-----------------------------------------------------------------------------

#endif
