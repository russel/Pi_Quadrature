#! /usr/bin/env fan

class Build : build::BuildPod {
  new make() {
    podName    = "pi_quadrature_output"
    summary    = "Output pod for the Pi_Quadrature code."
    depends    = ["sys 1.0+"]
    srcDirs    = [`fan/`]
  }
}
