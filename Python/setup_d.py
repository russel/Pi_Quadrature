from pyd.support import setup, Extension

projName = 'processAll_extension_d'

setup(
    name=projName,
    version='0.1',
    ext_modules=[
        Extension(projName, ['processAll_extension_d.d'],
            extra_compile_args=['-O', '-inline', '-release'],
            build_deimos=True,
            d_lump=True
        )
    ],
)
