# -*- mode:python; -*-

#  Calculation of π using quadrature.
#
#  Copyright © 2016 Russel Winder

project(
    'π by quadrature in C++',
    'cpp',
    default_options: ['cpp_std=c++14',]
)

rlw_home = '/home/users/russel'

extra_source = ['output.cpp']

thread_dependencies = dependency('threads')

openmp_cflags = ['-fopenmp']
openmp_libs = ['-lgomp']

mpi_cflags = run_command('pkg-config', '--cflags', 'mpi-cxx').stdout().strip().split()
mpi_libs = run_command('pkg-config', '--libs', 'mpi-cxx').stdout().strip().split()

targets = [
    [['pi_boostMPI.cpp', extra_source],  [], mpi_cflags, ['-lboost_mpi'] + mpi_libs],
    [['pi_mpi.cpp', extra_source],  [], mpi_cflags, mpi_libs],
    [['pi_parallel_boostThread.cpp', extra_source], [], [], ['-lboost_thread', '-lboost_system']],
    [['pi_parallel_cppcsp2.cpp', extra_source], [], ['-I' + rlw_home + '/include'], ['-L' + rlw_home + '/Built/lib', '-lcppcsp2',  '-lboost_system', '-lpthread']],
    [['pi_parallel_futures_async.cpp', extra_source], thread_dependencies, [], []],
    [['pi_parallel_futures_thread_AW.cpp', extra_source], thread_dependencies, [], []],
    [['pi_parallel_futures_thread.cpp', extra_source], thread_dependencies, [], []],
    [['pi_parallel_futures_thread_sse4_ready.cpp', extra_source], thread_dependencies, [], []],
    #[['pi_parallel_justThread_actors.cpp', extra_source], [], [], []],
    #[['pi_parallel_justThread_dataflow.cpp', extra_source], [], [], []],
    [['pi_parallel_openmp_batched.cpp', extra_source], [], openmp_cflags, openmp_libs],
    [['pi_parallel_openmp_unbatched.cpp', extra_source], [], openmp_cflags, openmp_libs],
    [['pi_parallel_tbb_autobatched.cpp', extra_source], [], [], ['-ltbb']],
    [['pi_parallel_threadsGlobalState.cpp', extra_source], thread_dependencies, [], []],
    [['pi_sequential_for.cpp', extra_source], [], [], []],
]

foreach t: targets
    sources = t.get(0)
    root = sources.get(0).split('.').get(0)
    executable(root, sources: sources, dependencies: t.get(1), cpp_args: t.get(2), link_args: t.get(3))
endforeach
