#ifndef MATRIX_HPP
#define MATRIX_HPP

#include "basicdt.hh"
#include <R_ext/Lapack.h>
#include <iostream>

/** Simpel Dense Rectangular Matrix Class.
 *
 *  - allows 2-d indexing
 *  - inject assignment
 *  - indexing via A(r,c) where r and c are integers
 *  - row/col index are starting from zero
 *  - Stored in an array colum-wise, i.e. A(r,c) is stored in v[rows*c+r].
 *
 * A few methods need for matrix manipulating and solving are given in the
 * class MatAlg. This includes solving of linear equations using lapack.
 */
class MatDouble
{
public:

    /** Constructs a column-wise matrix.
     \note Elements are not initialized!
     \param r Number of rows.
     \param c Number of columns.
     */
    MatDouble(int r, int c) {
        rows = r;
        cols = c;
        v = new double[rows*cols];
    }

    /** Constructs a column-wise square matrix.
     \param identity If true create an identity matrix.
     \param r Number of rows and columns.
     */
    MatDouble(int r, bool identity) {
        rows = cols = r;
        v = new double[rows*cols];
        if (identity) {
            Set(0);
            for(idx i=0; i<rows; i++) v[rows*i+i] = 1;
        }
    }

    /** Set all entries to val. */
    void Set(double val) {
        for (idx i=0; i<rows*cols; i++) v[i] = val;
    }

    /** Copy mat. */
    void Inject(const MatDouble & mat) {
        for (idx i=0; i<rows*cols; i++) v[i] = mat(i);
    }

    /** Multiply the matrix with -1. */
    void MultWithMinusOne() {
        for (idx i=0; i<rows*cols; i++) v[i] = -v[i];
    }

    /* Get entry (r,c). */
    double& operator()(int r, int c) {
       return v[rows*c+r];
    }

    /* Get entry (r,c). */
    double& operator()(int r, int c) const {
       return v[rows*c+r];
    }

    /* Get entry (r,c). */
    double& operator()(int i) const {
       return v[i];
    }

    void Print() {
        for(idx r=0;r<rows;r++) {
            for(idx c=0;c<cols;c++) cout << v[rows*c+r] << "\t";
            cout << endl;
        }
        cout << endl;
    }

    uInt rows;     ///< Number of rows.
    uInt cols;     ///< Number of cols.


private:
    double * v;   ///< Vector of doubles used to store the matrix column-wise.

};

/** Few algorithms for manipulating and solving matricies. */
class MatAlg
{
public:
    /** Set P := I-P.
     \pre P is a square matrix.
     */
    void IMinusP(MatDouble &P) const {
        P.MultWithMinusOne();
        for (idx i=0; i<P.rows; i++) P(i,i) = 1+P(i,i);
    }

    /** Solve equations Pw = r. */
    void LASolve(const MatDouble &P, MatDouble &w, const MatDouble &r) {
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

