# -*- mode:python; -*-

project(
    'π by quadrature in C',
    'c',
    default_options: ['c_std=c11',]
)

extra_source = ['output.c', 'microsecondTime.c']

dependencies = [dependency('threads')]

openmp_cflags = ['-fopenmp']
openmp_libs = ['-lgomp']

mpi_cflags = run_command('pkg-config', '--cflags', 'mpi').stdout().strip().split()
mpi_libs = run_command('pkg-config', '--libs', 'mpi').stdout().strip().split()

targets = [
    [['pi_sequential.c', extra_source], [], [], []],
    [['pi_pthreadParameters.c', extra_source], dependencies, [], []],
    [['pi_pthreadGlobalState.c', extra_source], dependencies, [], []],
    [['pi_openmpImplicit.c', extra_source], dependencies, openmp_cflags, openmp_libs],
    [['pi_openmpExplicit.c', extra_source], dependencies, openmp_cflags, openmp_libs],
    [['pi_mpi.c', extra_source],  dependencies, mpi_cflags, mpi_libs],
]

foreach t: targets
    sources = t.get(0)
    root = sources.get(0).split('.').get(0)
    executable(root, sources: sources, dependencies: t.get(1), c_args: t.get(2), link_args: t.get(3))
endforeach
