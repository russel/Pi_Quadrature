use "itertools"
use "promises"
use "time"

use "../Output"

actor Accumulator
	let env: Env
	let task_count: USize
	let n: USize
	let delta: F64
	let start_time: U64
	let p: Promise[None]

	new create(env': Env, task_count': USize, n': USize, delta': F64, start_time': U64, p': Promise[None]) =>
		env = env'
		task_count = task_count'
		n = n'
		delta = delta'
		start_time = start_time'
		p = p'

	be calculateAndReport(a: Array[F64] val) =>
		let pi = 4.0 * delta * Iter[F64](a.values()).fold[F64](0.0, {(t: F64, x: F64): F64 => t + x})
		let elapse_time = (Time.nanos() - start_time).f64() / 1e9
		Output.output(env, "Parallel", pi, n, elapse_time, task_count)
		p(None)

//	Local Variables:
//	mode: ponylang
//	indent-tabs-mode: t
//	tab-width: 4
//	End:
