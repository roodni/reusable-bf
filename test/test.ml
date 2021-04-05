open OUnit2

let () =
  run_test_tt_main ("tests" >::: [
    ReusableTest.tests;
    NamedTest.tests;
    BfTest.tests;
  ])