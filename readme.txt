This directory contains various implementations in various programming languages of the embarrassingly
parallel problem of calculating pi using quadrature. (Quadrature in this case is the process of finding the
area under a curve using the approximation of filling the area with rectangles and summing the areas of the
rectangles.)

The integral equation:

    \fraction{\pi}{4} = \int_{0}^{1}  \fraction{1}{1 + x^2} dx

leads to the summation:

      \pi = \fraction{4}{n} \sum_{i = 1}^{n} \fraction{1}{1 + (\fraction{i - 0.5}{n})^2}

This summation can be partitioned into partial sums that are them themselves summed.  This is an
embarrassingly parallel scatter/gather (aka farmer/worker) problem that can check scalability.  If the speed
of execution does not increase linearly with the number of processors available then there is an issue to
investigate.

Various different idioms and techniques in the various languages are tried as part of showing which
languages are better than others for this problem.  Also the examples investigate the properties of, and
indeed idioms and techniques appropriate to, the various languages.
