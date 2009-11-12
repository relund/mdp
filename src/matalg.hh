#ifndef MATALG_HPP
#define MATALG_HPP

#include "basicdt.hh"
#include "matrix.hh"
#include <iostream>
#include <R_ext/Lapack.h>


/** Few algorithms for manipulating and solving matricies. */
class MatAlg
{
public:
    /** Set P := I-P.
     \pre P is a square matrix.
     */
    void IMinusP(MatSimple<double> &P) const {
        P.MultWithMinusOne();
        for (idx i=0; i<P.rows; i++) P(i,i) = 1+P(i,i);
    }

    /** Solve equations Pw = r. */
    void LASolve(const MatSimple<double> &P, MatSimple<double> &w, const MatSimple<double> &r) {
        int rows = P.rows;
        int nrhs = 1;
        int ldp = P.rows;
        int ldr = r.rows;
        int ipiv[P.rows];
        int info = 0;
        w.Inject(r);    // copy r to w;
        F77_CALL(dgesv)(&rows, &nrhs, &P(0,0), &ldp, ipiv, &w(0,0), &ldr, &info);
        if (info!=0) {
            cout << "Error in LASolve" << endl;
            exit(1);
        }
    }
};


#endif

