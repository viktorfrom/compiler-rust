pub fn program1() -> String {
    let program = "

        fn testfn1(a: bool) -> i32 {
            let b: i32 = (((1 + 2 + 3)));
            if a {
                let c: i32 = 1;
                return c
            } else {
                return (b)
            };
        }

        fn testfn2() -> i32 {
            {{{ return testfn1(true); }}}
        }

        fn testfn3(d: bool, e: bool) -> i32 {
            let f: bool = d && e;
            let n: i32 = 1;
            while f == true {
                n += 1;
                f = false;
            };
            return n;    
        }

        fn main() -> i32 {
            let g: i32 = testfn2() + testfn3(true, true);
            return g
        }

        "
    .to_string();

    return program;
}

// Example for Inkwell/LLVM
pub fn program2() -> String {
    let program = "

        fn testfn1() -> i32 {
            let b: i32 = (((1 + 2 + 3)));
            if true {
                let c: i32 = 1;
                return c
            };
            return (b)
        }

        fn testfn2() -> i32 {
            {{{ return testfn1(); }}}
        }

        fn testfn3() -> i32 {
            let f: bool = true && true;
            let n: i32 = 1;
            if f == true {
                n += 1;
                f = false;
            };
            return n;    
        }

        fn main() -> i32 {
            let g: i32 = testfn2() + testfn3();
            return g 
        }

        "
    .to_string();

    return program;
}
