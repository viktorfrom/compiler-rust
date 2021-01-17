pub fn program() -> String {
    let program = "

        fn testfn1(a: bool) -> i32 { 
            let c: i32 = 2;

            if a { 
                let b: i32 = 1; 
                return b
            };

            return c;
        }

        fn testfn2() -> i32 {
            return testfn1(true);
        }
        
        fn main() -> i32 {
            let a: i32 = testfn1(); 
            let b: i32 = testfn2();
            return a + b;
        };

        "
    .to_string();

    // let program = "

    //     fn testfn1() -> i32 {
    //         let a: i32 = 1;
    //         let b: i32 = a;
    //         let c: i32 = b;
    //         return c;
    //     };

    //     fn testfn2() -> i32 {
    //         let b: bool = true && true;
    //         if b {
    //             return 50;
    //         };
    //         return 1;
    //     };

    //     fn main() -> i32 {
    //         let a: i32 = testfn1();
    //         let b: i32 = testfn2();
    //         return b;
    //     };
    //     "
    // .to_string();

    return program;
}
