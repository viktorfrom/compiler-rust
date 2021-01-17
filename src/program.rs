pub fn program() -> String {
    let program = "
        fn main() -> i32 {
            let a: i32 = testfn2(); 
            let b: i32 = testfn3();
            return a + b
        }

        "
    .to_string();
    // "

    //     fn testfn1(a: bool) -> i32 {
    //         let c: i32 = 2;

    //         if a {
    //             let b: i32 = 1;
    //             return b
    //         } else {
    //             return c
    //         };

    //     }

    //     fn testfn2() -> i32 {
    //         return testfn1(true);
    //     }

    //     fn testfn3(a: i32, b: bool, c: i32) -> i32 {
    //         let d: bool = c && b;
    //         let n: i32 = 0;
    //         return n
    //     }
    // "

    return program;
}
