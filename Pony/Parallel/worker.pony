use "collections"
use "itertools"
use "promises"

actor Worker
	let id: USize
	let slice_size: USize
	let delta: F64
	let result: Promise[F64]

	new create(id': USize, slice_size': USize, delta': F64, result': Promise[F64]) =>
		id = id'
		slice_size = slice_size'
		delta = delta'
		result = result'

	be calculate() =>
		/*
		result.apply(
			Iter[USize](Range(1 + (id * slice_size), ((id + 1) * slice_size) + 1)).fold[F64](0.0, {(t: F64, i: USize): F64 =>
				let x: F64 = (i.f64() - 0.5) * delta
				t + (1.0 / (1.0 + (x * x)))
			})
		)
		F64(0.0)
		*/
		var sum: F64 = 0.0
		for i in Range(1 + (id * slice_size), ((id + 1) * slice_size) + 1) do
			let x: F64 = (i.f64() - 0.5) * delta
			sum = sum + (1.0 / (1.0 + (x * x)))
		end
		result.apply(sum)

//	Local Variables:
//	mode: ponylang
//	indent-tabs-mode: t
//	tab-width: 4
//	End:
