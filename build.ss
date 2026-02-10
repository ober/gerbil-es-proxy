#!/usr/bin/env gxi
(import :std/build-script)

(defbuild-script
  '("mock-es"
    (exe: "es-proxy")))
