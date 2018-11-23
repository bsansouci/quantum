[@no_alloc] external quantum_getwidth: int => int = "my_quantum_getwidth";
[%c.raw
  {|
      value my_quantum_getwidth(value a) {
        return Val_int(quantum_getwidth(Int_val(a)));
      }
    |}
];

[@no_alloc] external quantum_gcd: (int, int) => int = "my_quantum_gcd";
[%c.raw
  {|
      value my_quantum_gcd(value a, value b) {
        return Val_int(quantum_gcd(Int_val(a), Int_val(b)));
      }
    |}
];

type quantumRegisterT;
external quantum_new_qureg: (Int64.t, int) => quantumRegisterT =
  "my_quantum_new_qureg";
[%c.raw
  {|
    CAMLprim value my_quantum_new_qureg(value initval, value width) {
      CAMLparam2(initval, width);
      CAMLlocal1(ret);
      /*printf("Int64_val(initval): %llu", Int64_val(initval));*/
      quantum_reg reg = quantum_new_qureg(Int64_val(initval), Int_val(width));

      quantum_reg *newReg = malloc(sizeof(quantum_reg));
      memcpy(newReg, &reg, sizeof(quantum_reg));

      /*newReg.width = reg.width;
      newReg.size = reg.size;
      newReg.hashw = reg.hashw;
      newReg.node = reg.node;
      newReg.hash = reg.hash;
*/
      /*printf("%d, %d, %d, %llu, %p\n", newReg->width, newReg->size, newReg->hashw, newReg->node[0].state, newReg->hash);*/
      ret = caml_alloc_small(1, 0);
      Field(ret, 0) = (long)newReg;

      CAMLreturn(ret);
    }
  |}
];

[@noalloc] external seed_random: unit => unit = "my_seed_random";
[%c.raw {|
  void my_seed_random() {
    srand(time(0));
  }
|}];

external rand: unit => int = "my_rand";
[%c.raw
  {|
  CAMLprim value my_rand() {
    CAMLparam0();
    CAMLreturn(Val_int(rand()));
  }
|}
];

[@noalloc]
external quantum_hadamard_mut: (int, quantumRegisterT) => unit =
  "my_quantum_hadamard";
[%c.raw
  {|
  void my_quantum_hadamard(value a, value reg) {
    quantum_hadamard(Int_val(a), (quantum_reg *)Field(reg, 0));
  }
|}
];

[@noalloc]
external quantum_addscratch_mut: (int, quantumRegisterT) => unit =
  "my_quantum_addscratch";
[%c.raw
  {|
  void my_quantum_addscratch(value a, value reg) {
    quantum_addscratch(Int_val(a), (quantum_reg *)Field(reg, 0));
  }
|}
];

[@noalloc]
external quantum_exp_mod_n_mut:
  (~n: int, ~x: int, ~width_input: int, ~width: int, quantumRegisterT) => unit =
  "my_quantum_exp_mod_n";
[%c.raw
  {|
  void my_quantum_exp_mod_n(value a, value b, value c, value d, value reg) {
    quantum_exp_mod_n(Int_val(a), Int_val(b), Int_val(c), Int_val(d), (quantum_reg *)Field(reg, 0));
  }
|}
];

[@noalloc]
external quantum_qft_mut: (int, quantumRegisterT) => unit = "my_quantum_qft";
[%c.raw
  {|
  void my_quantum_qft(value a, value reg) {
    quantum_qft(Int_val(a), (quantum_reg *)Field(reg, 0));
  }
|}
];

[@noalloc]
external quantum_bmeasure_mut: (int, quantumRegisterT) => int =
  "my_quantum_bmeasure";
[%c.raw
  {|
  value my_quantum_bmeasure(value a, value reg) {
    return Val_int(quantum_bmeasure(Int_val(a), (quantum_reg *)Field(reg, 0)));
  }
|}
];

[@noalloc] external quantum_ipow: (int, int) => int = "my_quantum_ipow";
[%c.raw
  {|
  value my_quantum_ipow(value a, value b) {
    return Val_int(quantum_ipow(Int_val(a), Int_val(b)));
  }
|}
];

external quantum_measure: quantumRegisterT => Int64.t = "my_quantum_measure";
[%c.raw
  {|
  CAMLprim value my_quantum_measure(value reg) {
    CAMLparam1(reg);
    CAMLreturn(caml_copy_int64(quantum_measure(*(quantum_reg *)Field(reg, 0))));
  }
|}
];

external quantum_frac_approx: (Int64.t, int, int) => (Int64.t, int) =
  "my_quantum_frac_approx";
[%c.raw
  {|
  CAMLprim value my_quantum_frac_approx(value cVal, value qVal, value width) {
    CAMLparam3(cVal, qVal, width);
    CAMLlocal1(ret);
    int c = (int) Int64_val(cVal);
    int q = Int_val(qVal);
    quantum_frac_approx(&c, &q, Int_val(width));

    ret = caml_alloc_small(2, 0);
    Field(ret, 0) = caml_copy_int64(c);
    Field(ret, 1) = Val_int(q);
    CAMLreturn(ret);
  }
|}
];

[@noalloc]
external quantum_cnot_mut: (int, int, quantumRegisterT) => unit =
  "my_quantum_cnot";
[%c.raw
  {|
  void my_quantum_cnot(value a, value b, value reg) {
    quantum_cnot(Int_val(a), Int_val(b), (quantum_reg *)Field(reg, 0));
  }
|}
];

[@noalloc]
external quantum_delete_qureg_mut: quantumRegisterT => unit =
  "my_quantum_delete_qureg";
[%c.raw
  {|
  void my_quantum_delete_qureg(value reg) {
    quantum_delete_qureg((quantum_reg *)Field(reg, 0));
  }
|}
];

seed_random();
