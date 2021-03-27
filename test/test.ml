open OUnit2

let () =
  run_test_tt_main ("tests" >::: [
    BfTest.tests;
    NamedTest.tests;
  ])