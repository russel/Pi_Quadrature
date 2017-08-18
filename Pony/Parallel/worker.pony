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
		try
			result.apply(
				Iter[USize](Range(1 + (id * slice_size), ((id + 1) * slice_size) + 1)).fold[F64]({(t: F64, i: USize): F64 =>
					let x: F64 = (i.f64() - 0.5) * delta
					t + (1.0 / (1.0 + (x * x)))
					}, 0.0)?)
		end
		F64(0.0)

//	Local Variables:
//	mode: ponylang
//	indent-tabs-mode: t
//	tab-width: 4
//	End:
