#! /usr/bin/env dart

void out(prefix, pi, n, elapseTime) {
  print('==================== ${prefix}');
  print('\tπ = ${pi}');
  print('\titeration count = ${n}');
  print('\telapse time = ${elapseTime}');
}

void main() {
  final n = 100000000; // 10 times fewer.
  final delta = 1.0 / n;
  final stopwatch = new Stopwatch()..start();
  var sum = 0.0;
  for (var i = 0; i < n; ++i) {
    final x = (i - 0.5) * delta;
    sum += 1.0 / (1.0 + x * x);
  }
  final pi = 4.0 * delta * sum;
  final elapseTime = stopwatch.elapsedMicroseconds / 1e6;
  out('pi_sequential', pi, n, elapseTime);
}
