fn kartoffel() -> bool {
    true
}

fn potato() -> bool {
    false
}

fn salad() -> bool {
    potato()
}

fn main() -> () {
    println!("Hello World!");
    salad();
}
