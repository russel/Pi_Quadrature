use "collections"
use "itertools"
use "time"

use "../Output"

actor Main
	new create(env: Env) =>
		let n: USize = 1000000000
		let delta: F64 = 1.0 / n.f64()
		let start_time = Time.nanos()
		var sum: F64 = 0.0
		for i in Range(1, n + 1) do
			let x = (i.f64() - 0.5) * delta
			sum = sum + (1.0 / (1.0 + (x * x)))
		end
		let pi = 4.0 * delta * sum
		let elapse_time = (Time.nanos() - start_time).f64() / 1e9
		Output.output(env, "Sequential Loop", pi, n, elapse_time)

//  Local Variables:
//  mode: ponylang
//  indent-tabs-mode: t
//  tab-width: 4
//  End:
