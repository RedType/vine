fn binet(n: i32) -> usize {
    // using binet's formula
    let r5 = 5_f64.sqrt();
    let term_a = (1_f64 + r5) / 2_f64;
    let term_b = (1_f64 - r5) / 2_f64;
    ((term_a.powi(n) - term_b.powi(n)) / r5).round() as _
}

#[test]
fn fibonacci_to_10() {
    let expected = vec![0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55];
    let actual = (0..=10).map(|n| binet(n)).collect::<Vec<_>>();
    assert_eq!(expected, actual);
}
