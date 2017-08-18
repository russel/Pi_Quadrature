use "format"

primitive Output

	fun output(env:Env, name: String, pi: F64, n: USize, elapse_time: F64, task_count: USize = 0) =>
		env.out.print(
			"================ " + name
			+ if task_count > 0 then ": task count = " + task_count.string() else "" end
			+ "\n\tπ = " + Format.float[F64](pi where prec=20)
			+ "\n\titeration count = " + n.string()
			+ "\n\telapse time = " + elapse_time.string()
		)

//  Local Variables:
//  mode: ponylang
//  indent-tabs-mode: t
//  tab-width: 4
//  End:
