open Bsb_internals;

let ( +/ ) = Filename.concat;

gcc(~includes=[root_project_dir +/ "libquantum" +/ "include"], destdir +/"Quantum_bindings.o", [destdir +/ "Quantum_bindings.c"]);
