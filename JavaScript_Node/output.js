/**
 *  Output functions for the JavaScript codes.
 *
 *  Copyright © 2014  Russel Winder
 */

// For nodejs
if (typeof print === 'undefined') var print = (msg) => {console.log(msg)};

function out(prefix, π, n, elapseTime, workers) {
  print('==================== ' + prefix);
  print('\tπ = ' + π);
  print('\titeration count = ' + n);
  print('\telapse time = ' + elapseTime);
  print('\tworkers = ' + workers);
}

// For nodejs
module.exports = out;
