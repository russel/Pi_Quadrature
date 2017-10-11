from pyd.support import setup, Extension

all_name = 'processAll_extension_d'
slice_name = 'processSlice_extension_d'

compile_arguments = ['-O', '-inline', '-release']

setup(
    name='D Extensions',
    version='0.1',
    ext_modules=[
        Extension(
            slice_name,
            [slice_name + '.d'],
            extra_compile_args=compile_arguments,
            build_deimos=True,
            d_lump=True
        ),
        Extension(
            all_name,
            [all_name + '.d'],
            extra_compile_args=compile_arguments,
            build_deimos=True,
            d_lump=True
        ),
    ],
)
