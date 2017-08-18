use "collections"
use "itertools"
use "time"

use "../Output"

actor Main
	new create(env: Env) =>
		let n: USize = 1000000000
		let delta: F64 = 1.0 / n.f64()
		let start_time = Time.nanos()
		try
			let pi = 4.0 * delta * Iter[USize](Range(1, n + 1)).fold[F64]({(t: F64, i: USize): F64 =>
				let x = (i.f64() - 0.5) * delta
				t + (1.0 / (1.0 + (x * x)))
			}, 0.0)?
			let elapse_time = (Time.nanos() - start_time).f64() / 1e9
			Output.output(env, "Sequential Fold", pi, n, elapse_time)
		end

//  Local Variables:
//  mode: ponylang
//  indent-tabs-mode: t
//  tab-width: 4
//  End:
