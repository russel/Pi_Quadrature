from pyd.support import setup, Extension

all_name = 'processAll_extension_d'
slice_name = 'processSlice_extension_d'

compile_arguments = ['-O', '-inline', '-release']

setup(
    name=all_name,
    version='0.1',
    ext_modules=[
        Extension(
            slice_name,
            ['processAll_extension_d.d'],
            extra_compile_args=compile_arguments,
            build_deimos=True,
            d_lump=True
        )
    ],
)

setup(
    name=slice_name,
    version='0.1',
    ext_modules=[
        Extension(
            slice_name,
            ['processSlice_extension_d.d'],
            extra_compile_args=compile_arguments,
            build_deimos=True,
            d_lump=True
        )
    ],
)
