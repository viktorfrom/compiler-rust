pub fn program() -> String {
    let program = " 
        fn testfn1() -> i32 {
            let a: i32 = 1;
            let b: i32 = a;
            let c: i32 = b;
            return c;
        };

        fn testfn2() -> i32 {
            let b: bool = true && true;
            if b {
                return 50;
            };
            return 1;
        };

        fn main() -> i32 {
            let a: i32 = testfn1(); 
            let b: i32 = testfn2();
            return b;
        };
        "
    .to_string();

    return program;
}
