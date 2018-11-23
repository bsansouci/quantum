let n = ref(15);

Arg.parse([], (str) => {
  n := int_of_string(str)
}, "Call `quantum [number to factorize]`")

let n = n^;
if (n < 15) {
  print_endline("Invalid number\n");
} else {
  let width = Quantum.quantum_getwidth(n * n);
  let swidth = Quantum.quantum_getwidth(n);

  Printf.printf("N = %d, %d qubits required\n", n, width + 3 * swidth + 2);

  let x = ref(0);
  while (Quantum.quantum_gcd(n, x^) > 1 || x^ < 2) {
    x := Quantum.rand() mod n;
  };

  let qr = Quantum.quantum_new_qureg(Int64.zero, width);
  for (i in 0 to width - 1) {
    Quantum.quantum_hadamard_mut(i, qr);
  };

  /*print_endline(Printf.sprintf(">>> Measured: %s\n", Int64.to_string(Quantum.quantum_measure(qr))));*/

  Quantum.quantum_addscratch_mut(3 * swidth + 2, qr);

  Quantum.quantum_exp_mod_n_mut(~n, ~x=x^, ~width_input=width, ~width=swidth, qr);

  for (_ in 0 to 3 * swidth + 2 - 1) {
    ignore @@ Quantum.quantum_bmeasure_mut(0, qr);
  };

  Quantum.quantum_qft_mut(width, qr);

  for (i in 0 to width / 2 - 1) {
    Quantum.quantum_cnot_mut(i, width - i - 1, qr);
    Quantum.quantum_cnot_mut(width - i - 1, i, qr);
    Quantum.quantum_cnot_mut(i, width - i - 1, qr);
  };

  let c = Quantum.quantum_measure(qr);
  /*print_endline(Printf.sprintf("Measured: %s", Int64.to_string(c)));*/

  if (Int64.compare(c, Int64.minus_one) == 0) {
    print_endline("Impossible Measurement!\n");
  } else if (Int64.compare(c, Int64.zero) == 0) {
    print_endline("Measured zero, try again.\n");
  } else {
    let q = 1 lsl width;

    /*print_endline(
        Printf.sprintf(
          "Measured %s (%f), q: %d",
          Int64.to_string(c),
          Int64.to_float(c) /. float_of_int(q),
          q,
        ),
      );*/

    let (c, q) = Quantum.quantum_frac_approx(c, q, width);

    print_endline(
      Printf.sprintf(
        "fractional approximation is %s/%i.\n",
        Int64.to_string(c),
        q,
      ),
    );

    let q =
      if (q mod 2 == 1 && 2 * q < 1 lsl width) {
        print_endline("Odd denominator, trying to expand by 2.\n");
        q * 2;
      } else {
        q;
      };
    if (q mod 2 == 1) {
      print_endline("Odd period, try again.\n");
    } else {
      print_endline(Printf.sprintf("Possible period is %i.\n", q));

      let a = (Quantum.quantum_ipow(x^, q / 2) + 1) mod n;
      let b = (Quantum.quantum_ipow(x^, q / 2) - 1) mod n;

      let a = Quantum.quantum_gcd(n, a);
      let b = Quantum.quantum_gcd(n, b);

      let factor = a > b ? a : b;

      if (factor < n && factor > 1) {
        print_endline(
          Printf.sprintf("%i = %i * %i\n", n, factor, n / factor),
        );
        Quantum.quantum_delete_qureg_mut(qr);
      } else {
        print_endline("Unable to determine factors, try again.\n");
      };
    };
  };
};
