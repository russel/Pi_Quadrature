This directory contains various implementations in various programming languages of the embarrassingly
parallel problem of calculating pi using quadrature.

The integral equation:

    \fraction{\pi}{4} = \int_{0}^{1}  \fraction{1}{1 + x^2} dx

leads to the summation:

      \pi = \fraction{4}{n} \sum_{i = 1}^{n} \fraction{1}{1 + (\fraction{i - 0.5}{n})^2}

This summation can be partitioned into partial sums that are them themselves summed.  i.e. this is an
embarrassingly parallel scatter/gather (aka farmer/worker) problem that can check scalability.

Various different idioms and techniques in al the various languages are tried as part of showing which
languages are better than others for this problem.  Also to investigate the properties of, and indeed idioms
and techniques appropriate to, the various languages.
