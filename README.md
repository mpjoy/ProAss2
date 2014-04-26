ProAss2
=======

Coursera:rprog-002:Programming Assignment 2

### Introduction

This is the second programming assignment, where an R
function that is able to cache potentially time-consuming computations is given.

### Assignment: Caching the Inverse of a Matrix

Matrix inversion is usually a costly computation and there may be some
benefit to caching the inverse of a matrix rather than computing it
repeatedly. In this assignment a pair of functions that
cache the inverse of a matrix is provided.

The two fucntions given:

1.  `makeCacheMatrix`: This function creates a special "matrix" object
    that can cache its inverse.
2.  `cacheSolve`: This function computes the inverse of the special
    "matrix" returned by `makeCacheMatrix` above. If the inverse has
    already been calculated (and the matrix has not changed), then
    `cacheSolve` should retrieve the inverse from the cache.

Computing the inverse of a square matrix can be done with the `solve`
function in R. For example, if `X` is a square invertible matrix, then
`solve(X)` returns its inverse.

I assume that the matrix supplied is always invertible.

This is a modified version of the the GitHub repository containing the stub R files at 
    [https://github.com/rdpeng/ProgrammingAssignment2](https://github.com/rdpeng/ProgrammingAssignment2) provided by the instructor Dr. Roger Peng.
