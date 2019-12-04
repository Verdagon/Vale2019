
fn round(num: Float) {
  let distanceToLower = num - Int(num);
  Int(num) + if {distanceToLower < 0.5} { 0 } else { 1 }
}

fn main() int {
  round(7.9)
}
