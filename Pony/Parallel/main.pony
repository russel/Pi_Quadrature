use "collections"
use "itertools"
use "promises"
use "time"

actor Main

	new create(env: Env) =>
		let main: Main tag = this
		execute(env, 1).next[None]({(x: None) =>
			main.execute(env, 2).next[None]({(x: None) =>
				main.execute(env, 8).next[None]({(x: None) =>
					main.execute(env, 32)
				} iso)
			} iso)
		} iso)


	fun tag execute(env: Env, task_count: USize): Promise[None] =>
		let n: USize = 1000000000
		let delta: F64 = 1.0 / n.f64()
		let start_time = Time.nanos()
		let slice_size = n / task_count
		let createPromises = {(i: USize): Promise[F64] =>
			let prom = Promise[F64]
			Worker.create(i, slice_size, delta, prom).calculate()
			prom
		} iso
		let p = Promise[None]
		let accumulator = Accumulator.create(env, task_count, n, delta, start_time, p)
		Promises[F64].join(
			Iter[USize](Range(0, task_count)).map[Promise[F64]](consume createPromises)
		).next[None](recover accumulator~calculateAndReport() end)
		p

//	Local Variables:
//	mode: ponylang
//	indent-tabs-mode: t
//	tab-width: 4
//	End:
