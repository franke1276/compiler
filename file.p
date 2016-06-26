fn hello(s: String): String {
  s + "hallo"
}
fn add(a: Int,b: Int): Int {
  let x: Int = a + 7
  x + b / 3
}
fn main(): Void {
  print(hello("world"))
  print(add(4 * (5 + 7),7))
}